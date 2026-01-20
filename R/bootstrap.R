############################################################
# surveySHAP Bootstrap Utilities (Copy-ready R script)
# ----------------------------------------------------------
# Implements bootstrap for:
#   (1) strength: one-way group + two-way group-pairs
#   (2) direction: main active (variable-level) + interaction active (onehot-pair)
#
# This script assumes your repo already provides:
#   - run_survey_shap()
#   - (and all its internal dependencies)
#
# Design choice: (A) Bootstrap the *existing tables* (no new definitions)
# Notes:
#   - Interaction SHAP uses subsampling inside run_survey_shap(); we control its seed
#     per replicate to reduce Monte Carlo noise.
#   - We align all bootstrap outputs to the baseline keys from the full-data run.
############################################################

# ---- Helpers: keying and alignment ------------------------------------------

# Convert a strength main_group df (columns: group, strength) to named numeric
.as_named_strength_main <- function(df) {
  stopifnot(is.data.frame(df))
  stopifnot(all(c("group", "strength") %in% names(df)))
  v <- df$strength
  names(v) <- df$group
  v
}

# Convert a strength interaction_group df (columns: pair, strength) to named numeric
.as_named_strength_pair <- function(df) {
  stopifnot(is.data.frame(df))
  stopifnot(all(c("pair", "strength") %in% names(df)))
  v <- df$strength
  names(v) <- df$pair
  v
}

# Main direction df expected columns: variable, level, n, shap_active
# Key: "variable::level"
.as_named_direction_main <- function(df) {
  stopifnot(is.data.frame(df))
  req <- c("variable", "level", "n", "shap_active")
  stopifnot(all(req %in% names(df)))
  key <- paste(df$variable, df$level, sep = "::")
  v <- df$shap_active
  names(v) <- key
  n <- df$n
  names(n) <- key
  list(v = v, n = n)
}

# Interaction direction df expected columns: feat_i, feat_j, group_i, group_j, direction_active, n_active
# Key: "feat_i×feat_j" (exact order as produced; do not reorder to avoid mismatches)
.as_named_direction_interaction <- function(df) {
  stopifnot(is.data.frame(df))
  req <- c("feat_i", "feat_j", "group_i", "group_j", "direction_active", "n_active")
  stopifnot(all(req %in% names(df)))
  key <- paste(df$feat_i, df$feat_j, sep = "×")
  v <- df$direction_active
  names(v) <- key
  n <- df$n_active
  names(n) <- key
  list(v = v, n = n)
}

# Align a named numeric vector to a reference name set
.align_named_numeric <- function(x, ref_names) {
  out <- rep(NA_real_, length(ref_names))
  names(out) <- ref_names
  if (!is.null(x) && length(x) > 0) {
    keep <- intersect(names(x), ref_names)
    out[keep] <- x[keep]
  }
  out
}

# Summarise a bootstrap matrix with percentile CI + presence prob + top-k prob
.summarise_matrix <- function(mat, hat = NULL, conf = 0.95, top_k = 5) {
  stopifnot(is.matrix(mat))
  nm <- colnames(mat)
  if (is.null(nm)) stop("mat must have colnames (keys).")

  alpha <- (1 - conf) / 2
  probs <- c(alpha, 1 - alpha)

  mean_b <- apply(mat, 2, function(x) mean(x, na.rm = TRUE))
  sd_b   <- apply(mat, 2, function(x) stats::sd(x, na.rm = TRUE))
  ci     <- t(apply(mat, 2, function(x) stats::quantile(x, probs = probs, na.rm = TRUE, names = FALSE)))
  colnames(ci) <- c("ci_lo", "ci_hi")
  present_prob <- apply(mat, 2, function(x) mean(!is.na(x)))

  # top-k probability by value (decreasing), per replicate
  topk_prob <- rep(NA_real_, length(nm))
  if (ncol(mat) > 0) {
    topk_count <- rep(0L, length(nm))
    for (b in seq_len(nrow(mat))) {
      x <- mat[b, ]
      if (all(is.na(x))) next
      kk <- min(top_k, sum(!is.na(x)))
      idx <- order(x, decreasing = TRUE, na.last = NA)[seq_len(kk)]
      topk_count[idx] <- topk_count[idx] + 1L
    }
    topk_prob <- topk_count / nrow(mat)
  }

  # optional point estimate alignment
  est <- rep(NA_real_, length(nm))
  if (!is.null(hat)) {
    if (is.null(names(hat))) stop("hat must be named if provided.")
    est <- .align_named_numeric(hat, nm)
  }

  data.frame(
    key = nm,
    estimate = as.numeric(est),
    mean_boot = as.numeric(mean_b),
    sd_boot = as.numeric(sd_b),
    ci_lo = ci[, "ci_lo"],
    ci_hi = ci[, "ci_hi"],
    present_prob = as.numeric(present_prob),
    topk_prob = as.numeric(topk_prob),
    stringsAsFactors = FALSE
  )
}

# Summarise direction matrices with *two* top-k probs: top positive and top negative
.summarise_direction_matrix <- function(mat, hat = NULL, conf = 0.95, top_k = 10) {
  stopifnot(is.matrix(mat))
  nm <- colnames(mat)
  if (is.null(nm)) stop("mat must have colnames (keys).")

  alpha <- (1 - conf) / 2
  probs <- c(alpha, 1 - alpha)

  mean_b <- apply(mat, 2, function(x) mean(x, na.rm = TRUE))
  sd_b   <- apply(mat, 2, function(x) stats::sd(x, na.rm = TRUE))
  ci     <- t(apply(mat, 2, function(x) stats::quantile(x, probs = probs, na.rm = TRUE, names = FALSE)))
  colnames(ci) <- c("ci_lo", "ci_hi")
  present_prob <- apply(mat, 2, function(x) mean(!is.na(x)))

  # top-k positive prob
  top_pos_prob <- rep(NA_real_, length(nm))
  # top-k negative prob (most negative)
  top_neg_prob <- rep(NA_real_, length(nm))

  if (ncol(mat) > 0) {
    pos_count <- rep(0L, length(nm))
    neg_count <- rep(0L, length(nm))

    for (b in seq_len(nrow(mat))) {
      x <- mat[b, ]
      if (all(is.na(x))) next

      # positive: largest
      kk_pos <- min(top_k, sum(!is.na(x)))
      idx_pos <- order(x, decreasing = TRUE, na.last = NA)[seq_len(kk_pos)]
      pos_count[idx_pos] <- pos_count[idx_pos] + 1L

      # negative: smallest
      kk_neg <- min(top_k, sum(!is.na(x)))
      idx_neg <- order(x, decreasing = FALSE, na.last = NA)[seq_len(kk_neg)]
      neg_count[idx_neg] <- neg_count[idx_neg] + 1L
    }

    top_pos_prob <- pos_count / nrow(mat)
    top_neg_prob <- neg_count / nrow(mat)
  }

  est <- rep(NA_real_, length(nm))
  if (!is.null(hat)) {
    if (is.null(names(hat))) stop("hat must be named if provided.")
    est <- .align_named_numeric(hat, nm)
  }

  data.frame(
    key = nm,
    estimate = as.numeric(est),
    mean_boot = as.numeric(mean_b),
    sd_boot = as.numeric(sd_b),
    ci_lo = ci[, "ci_lo"],
    ci_hi = ci[, "ci_hi"],
    present_prob = as.numeric(present_prob),
    top_pos_prob = as.numeric(top_pos_prob),
    top_neg_prob = as.numeric(top_neg_prob),
    stringsAsFactors = FALSE
  )
}

# ---- Bootstrap runner --------------------------------------------------------

#' Bootstrap run for surveySHAP summaries (strength + direction)
#'
#' @param data Raw input data.frame passed to run_survey_shap().
#' @param B Number of bootstrap replicates.
#' @param seed Global seed.
#' @param parallel Whether to use base parallel::parLapply.
#' @param n_cores Number of cores if parallel=TRUE.
#' @param run_args List of additional args forwarded to run_survey_shap()
#'        (e.g., years/policy settings if your wrapper uses them; otherwise ignore).
#'        IMPORTANT: we will override run_args$seed per replicate to control subsampling.
#' @param xgb_seed_per_rep Logical; if TRUE, also inject xgboost seed via run_args$params$seed
#'        (only works if your run_survey_shap forwards params into fit_survey_xgb()).
#' @param verbose Logical.
#'
#' @return A list containing baseline outputs, bootstrap matrices, and failure indices.
bootstrap_survey_shap <- function(data,
                                  B = 200,
                                  seed = 1,
                                  parallel = FALSE,
                                  n_cores = max(1L, parallel::detectCores() - 1L),
                                  run_args = list(),
                                  xgb_seed_per_rep = TRUE,
                                  verbose = TRUE) {
  stopifnot(is.data.frame(data), B >= 1)

  set.seed(seed)

  # ----- Baseline run (defines reference keys) -----
  run_args0 <- run_args
  run_args0$seed <- seed
  if (xgb_seed_per_rep) {
    if (is.null(run_args0$params)) run_args0$params <- list()
    run_args0$params$seed <- seed
  }

  base <- try(do.call(run_survey_shap, c(list(data = data), run_args0)), silent = TRUE)
  if (inherits(base, "try-error")) stop("Baseline run_survey_shap() failed:\n", base)

  # Extract baseline named objects
  base_strength_main <- .as_named_strength_main(base$strength_main_group)
  base_strength_pair <- .as_named_strength_pair(base$strength_interaction_group)

  base_dir_main <- .as_named_direction_main(base$direction_main_active)
  base_dir_int  <- .as_named_direction_interaction(base$direction_interaction_active)

  ref_strength_main_names <- names(base_strength_main)
  ref_strength_pair_names <- names(base_strength_pair)
  ref_dir_main_keys <- names(base_dir_main$v)
  ref_dir_int_keys  <- names(base_dir_int$v)

  # Storage matrices
  strength_main_mat <- matrix(NA_real_, nrow = B, ncol = length(ref_strength_main_names),
                              dimnames = list(NULL, ref_strength_main_names))
  strength_pair_mat <- matrix(NA_real_, nrow = B, ncol = length(ref_strength_pair_names),
                              dimnames = list(NULL, ref_strength_pair_names))

  dir_main_mat <- matrix(NA_real_, nrow = B, ncol = length(ref_dir_main_keys),
                         dimnames = list(NULL, ref_dir_main_keys))
  dir_int_mat  <- matrix(NA_real_, nrow = B, ncol = length(ref_dir_int_keys),
                         dimnames = list(NULL, ref_dir_int_keys))

  # Also keep n (counts) for direction tables
  dir_main_n_mat <- matrix(NA_real_, nrow = B, ncol = length(ref_dir_main_keys),
                           dimnames = list(NULL, ref_dir_main_keys))
  dir_int_n_mat  <- matrix(NA_real_, nrow = B, ncol = length(ref_dir_int_keys),
                           dimnames = list(NULL, ref_dir_int_keys))

  failures <- integer(0)
  n <- nrow(data)

  # One replicate worker
  one_rep <- function(b) {
    idx <- sample.int(n, size = n, replace = TRUE)
    db <- data[idx, , drop = FALSE]

    args_b <- run_args
    # deterministic per-replicate seed for interaction subsample selection
    args_b$seed <- seed + b
    if (xgb_seed_per_rep) {
      if (is.null(args_b$params)) args_b$params <- list()
      args_b$params$seed <- seed + b
    }

    rslt <- do.call(run_survey_shap, c(list(data = db), args_b))

    sm <- .as_named_strength_main(rslt$strength_main_group)
    sp <- .as_named_strength_pair(rslt$strength_interaction_group)

    dm <- .as_named_direction_main(rslt$direction_main_active)
    di <- .as_named_direction_interaction(rslt$direction_interaction_active)

    list(
      strength_main = .align_named_numeric(sm, ref_strength_main_names),
      strength_pair = .align_named_numeric(sp, ref_strength_pair_names),
      dir_main_v = .align_named_numeric(dm$v, ref_dir_main_keys),
      dir_main_n = .align_named_numeric(dm$n, ref_dir_main_keys),
      dir_int_v  = .align_named_numeric(di$v, ref_dir_int_keys),
      dir_int_n  = .align_named_numeric(di$n, ref_dir_int_keys)
    )
  }

  if (!parallel) {
    for (b in seq_len(B)) {
      if (verbose && (b %% max(1L, floor(B / 10))) == 0L) {
        message(sprintf("Bootstrap %d / %d", b, B))
      }
      out <- try(one_rep(b), silent = TRUE)
      if (inherits(out, "try-error")) {
        failures <- c(failures, b)
        next
      }
      strength_main_mat[b, ] <- out$strength_main
      strength_pair_mat[b, ] <- out$strength_pair
      dir_main_mat[b, ] <- out$dir_main_v
      dir_main_n_mat[b, ] <- out$dir_main_n
      dir_int_mat[b, ] <- out$dir_int_v
      dir_int_n_mat[b, ] <- out$dir_int_n
    }
  } else {
    n_cores <- max(1L, as.integer(n_cores))
    cl <- parallel::makeCluster(n_cores)
    on.exit(parallel::stopCluster(cl), add = TRUE)

    parallel::clusterExport(
      cl,
      varlist = c("data", "n", "seed", "B", "run_args", "xgb_seed_per_rep",
                  "ref_strength_main_names", "ref_strength_pair_names",
                  "ref_dir_main_keys", "ref_dir_int_keys",
                  ".as_named_strength_main", ".as_named_strength_pair",
                  ".as_named_direction_main", ".as_named_direction_interaction",
                  ".align_named_numeric", "run_survey_shap"),
      envir = environment()
    )

    res <- parallel::parLapply(cl, X = seq_len(B), fun = function(b) {
      set.seed(seed + b)
      idx <- sample.int(n, size = n, replace = TRUE)
      db <- data[idx, , drop = FALSE]

      args_b <- run_args
      args_b$seed <- seed + b
      if (xgb_seed_per_rep) {
        if (is.null(args_b$params)) args_b$params <- list()
        args_b$params$seed <- seed + b
      }

      rslt <- do.call(run_survey_shap, c(list(data = db), args_b))

      sm <- .as_named_strength_main(rslt$strength_main_group)
      sp <- .as_named_strength_pair(rslt$strength_interaction_group)

      dm <- .as_named_direction_main(rslt$direction_main_active)
      di <- .as_named_direction_interaction(rslt$direction_interaction_active)

      list(
        ok = TRUE,
        strength_main = .align_named_numeric(sm, ref_strength_main_names),
        strength_pair = .align_named_numeric(sp, ref_strength_pair_names),
        dir_main_v = .align_named_numeric(dm$v, ref_dir_main_keys),
        dir_main_n = .align_named_numeric(dm$n, ref_dir_main_keys),
        dir_int_v  = .align_named_numeric(di$v, ref_dir_int_keys),
        dir_int_n  = .align_named_numeric(di$n, ref_dir_int_keys)
      )
    })

    for (b in seq_len(B)) {
      out <- res[[b]]
      if (is.null(out$ok) || !isTRUE(out$ok)) {
        failures <- c(failures, b)
        next
      }
      strength_main_mat[b, ] <- out$strength_main
      strength_pair_mat[b, ] <- out$strength_pair
      dir_main_mat[b, ] <- out$dir_main_v
      dir_main_n_mat[b, ] <- out$dir_main_n
      dir_int_mat[b, ] <- out$dir_int_v
      dir_int_n_mat[b, ] <- out$dir_int_n
    }
  }

  list(
    baseline = list(
      rslt = base,
      strength_main = base_strength_main,
      strength_pair = base_strength_pair,
      direction_main = base_dir_main,   # list(v=, n=)
      direction_int  = base_dir_int     # list(v=, n=)
    ),
    boot = list(
      strength_main_mat = strength_main_mat,
      strength_pair_mat = strength_pair_mat,
      dir_main_mat = dir_main_mat,
      dir_main_n_mat = dir_main_n_mat,
      dir_int_mat = dir_int_mat,
      dir_int_n_mat = dir_int_n_mat,
      failures = failures
    ),
    meta = list(B = B, seed = seed, parallel = parallel, n_cores = n_cores)
  )
}

# ---- Summaries: strength + direction ----------------------------------------

#' Summarise bootstrap results for strength tables
#'
#' @param boot_obj output from bootstrap_survey_shap()
#' @param conf confidence level
#' @param top_k top-k probability cutoff
#' @return list(oneway = df, twoway = df)
summarise_bootstrap_strength <- function(boot_obj, conf = 0.95, top_k = 5) {
  stopifnot(is.list(boot_obj), !is.null(boot_obj$boot), !is.null(boot_obj$baseline))
  sm_mat <- boot_obj$boot$strength_main_mat
  sp_mat <- boot_obj$boot$strength_pair_mat

  sm_hat <- boot_obj$baseline$strength_main
  sp_hat <- boot_obj$baseline$strength_pair

  list(
    oneway = .summarise_matrix(sm_mat, hat = sm_hat, conf = conf, top_k = top_k),
    twoway = .summarise_matrix(sp_mat, hat = sp_hat, conf = conf, top_k = top_k)
  )
}

#' Summarise bootstrap results for direction tables (main + interaction)
#'
#' @param boot_obj output from bootstrap_survey_shap()
#' @param conf confidence level
#' @param top_k top-k probability cutoff (positive/negative separately)
#' @return list(main = df, interaction = df)
#'
#' The returned dfs are keyed by:
#'   - main: "variable::level"
#'   - interaction: "feat_i×feat_j"
summarise_bootstrap_direction <- function(boot_obj, conf = 0.95, top_k = 10) {
  stopifnot(is.list(boot_obj), !is.null(boot_obj$boot), !is.null(boot_obj$baseline))
  dm_mat <- boot_obj$boot$dir_main_mat
  di_mat <- boot_obj$boot$dir_int_mat

  dm_hat <- boot_obj$baseline$direction_main$v
  di_hat <- boot_obj$baseline$direction_int$v

  list(
    main = .summarise_direction_matrix(dm_mat, hat = dm_hat, conf = conf, top_k = top_k),
    interaction = .summarise_direction_matrix(di_mat, hat = di_hat, conf = conf, top_k = top_k)
  )
}

# ---- Optional: decode keys back into columns --------------------------------

# Turn "variable::level" into columns
decode_main_direction_keys <- function(df) {
  stopifnot(is.data.frame(df), "key" %in% names(df))
  sp <- strsplit(df$key, "::", fixed = TRUE)
  df$variable <- vapply(sp, `[`, character(1), 1)
  df$level    <- vapply(sp, function(x) paste(x[-1], collapse = "::"), character(1))
  df
}

# Turn "feat_i×feat_j" into columns
decode_interaction_direction_keys <- function(df) {
  stopifnot(is.data.frame(df), "key" %in% names(df))
  sp <- strsplit(df$key, "×", fixed = TRUE)
  df$feat_i <- vapply(sp, `[`, character(1), 1)
  df$feat_j <- vapply(sp, function(x) paste(x[-1], collapse = "×"), character(1))
  df
}

############################################################
# Example usage (commented out)
############################################################
# boot_obj <- bootstrap_survey_shap(
#   data = your_df,
#   B = 200,
#   seed = 1,
#   parallel = TRUE,
#   n_cores = 8,
#   run_args = list(
#     y_var = "guns_rf",          # if your run_survey_shap supports these args
#     covars = c("partisan","race","gender","college","gun_own"),
#     interaction_subsample_n = 5000
#   )
# )
#
# sum_strength <- summarise_bootstrap_strength(boot_obj, conf = 0.95, top_k = 5)
# sum_dir      <- summarise_bootstrap_direction(boot_obj, conf = 0.95, top_k = 10)
#
# sum_dir$main <- decode_main_direction_keys(sum_dir$main)
# sum_dir$interaction <- decode_interaction_direction_keys(sum_dir$interaction)
############################################################
