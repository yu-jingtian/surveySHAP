############################################################
# Bootstrap utilities
############################################################

# --- internal helpers (not exported) ---

.as_named_strength_main <- function(df) {
  stopifnot(is.data.frame(df), all(c("group", "strength") %in% names(df)))
  v <- df$strength
  names(v) <- df$group
  v
}

.as_named_strength_pair <- function(df) {
  stopifnot(is.data.frame(df), all(c("pair", "strength") %in% names(df)))
  v <- df$strength
  names(v) <- df$pair
  v
}

# direction_main_active in your repo: columns (level, shap_active, n, onehot)
# Use onehot as the stable key.
.as_named_direction_main <- function(df) {
  stopifnot(is.data.frame(df), all(c("level", "shap_active", "n", "onehot") %in% names(df)))
  key <- as.character(df$onehot)
  v <- df$shap_active; names(v) <- key
  n <- df$n;          names(n) <- key
  list(v = v, n = n)
}

# direction_interaction_active in your repo already has feat_i/feat_j columns.
.as_named_direction_interaction <- function(df) {
  stopifnot(is.data.frame(df),
            all(c("feat_i", "feat_j", "group_i", "group_j", "direction_active", "n_active") %in% names(df)))
  key <- paste(df$feat_i, df$feat_j, sep = "×")
  v <- df$direction_active; names(v) <- key
  n <- df$n_active;         names(n) <- key
  list(v = v, n = n)
}

.align_named_numeric <- function(x, ref_names) {
  out <- rep(NA_real_, length(ref_names))
  names(out) <- ref_names
  if (!is.null(x) && length(x) > 0) {
    keep <- intersect(names(x), ref_names)
    out[keep] <- x[keep]
  }
  out
}


#' Bootstrap run for surveySHAP summaries (strength + direction)
#'
#' Parallel-safe even when the package is NOT installed on workers (devtools::load_all / sourced).
#'
#' @export
bootstrap_survey_shap <- function(data,
                                  B = 200,
                                  seed = 1,
                                  parallel = FALSE,
                                  n_cores = max(1L, parallel::detectCores() - 1L),
                                  run_args = list(),
                                  verbose = TRUE) {
  stopifnot(is.data.frame(data), B >= 1)

  # Forward only arguments supported by run_survey_shap()
  allowed <- names(formals(run_survey_shap))
  run_args <- run_args[names(run_args) %in% allowed]

  # Baseline run defines reference keys
  args0 <- run_args
  args0$seed <- as.integer(seed)
  base <- try(do.call(run_survey_shap, c(list(data = data), args0)), silent = TRUE)
  if (inherits(base, "try-error")) stop("Baseline run_survey_shap() failed:\n", base)

  base_strength_main <- .as_named_strength_main(base$strength_main_group)
  base_strength_pair <- .as_named_strength_pair(base$strength_interaction_group)
  base_dir_main <- .as_named_direction_main(base$direction_main_active)
  base_dir_int  <- .as_named_direction_interaction(base$direction_interaction_active)

  ref_strength_main_names <- names(base_strength_main)
  ref_strength_pair_names <- names(base_strength_pair)
  ref_dir_main_keys <- names(base_dir_main$v)
  ref_dir_int_keys  <- names(base_dir_int$v)

  strength_main_mat <- matrix(NA_real_, nrow = B, ncol = length(ref_strength_main_names),
                              dimnames = list(NULL, ref_strength_main_names))
  strength_pair_mat <- matrix(NA_real_, nrow = B, ncol = length(ref_strength_pair_names),
                              dimnames = list(NULL, ref_strength_pair_names))

  dir_main_mat <- matrix(NA_real_, nrow = B, ncol = length(ref_dir_main_keys),
                         dimnames = list(NULL, ref_dir_main_keys))
  dir_int_mat  <- matrix(NA_real_, nrow = B, ncol = length(ref_dir_int_keys),
                         dimnames = list(NULL, ref_dir_int_keys))

  dir_main_n_mat <- matrix(NA_real_, nrow = B, ncol = length(ref_dir_main_keys),
                           dimnames = list(NULL, ref_dir_main_keys))
  dir_int_n_mat  <- matrix(NA_real_, nrow = B, ncol = length(ref_dir_int_keys),
                           dimnames = list(NULL, ref_dir_int_keys))

  failures <- integer(0)
  n <- nrow(data)

  one_rep <- function(b) {
    idx <- sample.int(n, size = n, replace = TRUE)
    db <- data[idx, , drop = FALSE]

    args_b <- run_args
    args_b$seed <- as.integer(seed + b)

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
      dir_main_mat[b, ]      <- out$dir_main_v
      dir_main_n_mat[b, ]    <- out$dir_main_n
      dir_int_mat[b, ]       <- out$dir_int_v
      dir_int_n_mat[b, ]     <- out$dir_int_n
    }
  } else {

    # ---------- Robust export machinery ----------
    resolve_symbol <- function(sym) {
      # 1) Global / calling environments
      if (exists(sym, envir = .GlobalEnv, inherits = TRUE)) {
        return(get(sym, envir = .GlobalEnv, inherits = TRUE))
      }
      if (exists(sym, envir = parent.frame(), inherits = TRUE)) {
        return(get(sym, envir = parent.frame(), inherits = TRUE))
      }

      # 2) Where run_survey_shap() is defined
      env_rs <- environment(run_survey_shap)
      if (!is.null(env_rs) && exists(sym, envir = env_rs, inherits = TRUE)) {
        return(get(sym, envir = env_rs, inherits = TRUE))
      }

      # 3) Package namespace if available (works when package is installed/loaded)
      ns_name <- tryCatch(getNamespaceName(env_rs), error = function(e) NULL)
      if (!is.null(ns_name) && isNamespaceLoaded(ns_name)) {
        ns <- asNamespace(ns_name)
        if (exists(sym, envir = ns, inherits = TRUE)) {
          return(get(sym, envir = ns, inherits = TRUE))
        }
      }

      stop(sprintf("Cannot resolve symbol '%s' for parallel export.", sym))
    }

    # Build an explicit export env containing everything workers need
    export_env <- new.env(parent = emptyenv())

    # Core objects workers need
    base_symbols <- c(
      "data", "n", "seed", "run_args",
      "ref_strength_main_names", "ref_strength_pair_names",
      "ref_dir_main_keys", "ref_dir_int_keys",
      "run_survey_shap",
      ".as_named_strength_main", ".as_named_strength_pair",
      ".as_named_direction_main", ".as_named_direction_interaction",
      ".align_named_numeric"
    )

    # Likely pipeline dependencies (your errors confirm these exist)
    pipe_symbols <- c(
      "build_xgb_design", "fit_survey_xgb",
      "compute_shap_main", "compute_shap_interaction",
      "shap_strength_main_group", "shap_direction_main_active",
      "shap_strength_interaction_group", "shap_direction_interaction_active",
      "full_dummy_contrasts"
    )

    # Add any other helpers your build/design functions might call
    # (safe even if not used; resolve_symbol will error if truly missing)
    # You can append more here if another "could not find function" appears.
    all_symbols <- unique(c(base_symbols, pipe_symbols))

    for (sym in all_symbols) {
      assign(sym, resolve_symbol(sym), envir = export_env)
    }

    n_cores <- max(1L, as.integer(n_cores))
    cl <- parallel::makeCluster(n_cores)
    on.exit(parallel::stopCluster(cl), add = TRUE)

    parallel::clusterEvalQ(cl, {
      library(xgboost)
      library(Matrix)
      NULL
    })

    parallel::clusterExport(cl, varlist = all_symbols, envir = export_env)

    res <- parallel::parLapply(cl, X = seq_len(B), fun = function(b) {
      set.seed(as.integer(seed + b))
      idx <- sample.int(n, size = n, replace = TRUE)
      db <- data[idx, , drop = FALSE]

      args_b <- run_args
      args_b$seed <- as.integer(seed + b)

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
      dir_main_mat[b, ]      <- out$dir_main_v
      dir_main_n_mat[b, ]    <- out$dir_main_n
      dir_int_mat[b, ]       <- out$dir_int_v
      dir_int_n_mat[b, ]     <- out$dir_int_n
    }
  }

  list(
    baseline = list(
      rslt = base,
      strength_main = base_strength_main,
      strength_pair = base_strength_pair,
      direction_main = base_dir_main,
      direction_int  = base_dir_int
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
    meta = list(
      B = B,
      seed = seed,
      parallel = parallel,
      n_cores = n_cores,
      forwarded_args = names(run_args)
    )
  )
}





#' Summarise bootstrap results for strength tables
#'
#' @param boot_obj Output from \code{bootstrap_survey_shap()}.
#' @param conf Confidence level.
#' @param top_k Top-k probability cutoff.
#' @return A list with \code{oneway} and \code{twoway} data.frames.
#' @export
summarise_bootstrap_strength <- function(boot_obj, conf = 0.95, top_k = 5) {
  mat_sum <- function(mat, hat, conf, top_k) {
    nm <- colnames(mat)
    alpha <- (1 - conf) / 2
    probs <- c(alpha, 1 - alpha)

    mean_b <- apply(mat, 2, function(x) mean(x, na.rm = TRUE))
    sd_b   <- apply(mat, 2, function(x) stats::sd(x, na.rm = TRUE))
    ci     <- t(apply(mat, 2, function(x) stats::quantile(x, probs = probs, na.rm = TRUE, names = FALSE)))
    colnames(ci) <- c("ci_lo", "ci_hi")
    present_prob <- apply(mat, 2, function(x) mean(!is.na(x)))

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

    est <- rep(NA_real_, length(nm)); names(est) <- nm
    keep <- intersect(names(hat), nm)
    est[keep] <- hat[keep]

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

  sm <- mat_sum(
    boot_obj$boot$strength_main_mat,
    boot_obj$baseline$strength_main,
    conf = conf,
    top_k = top_k
  )
  sp <- mat_sum(
    boot_obj$boot$strength_pair_mat,
    boot_obj$baseline$strength_pair,
    conf = conf,
    top_k = top_k
  )

  list(oneway = sm, twoway = sp)
}


#' Summarise bootstrap results for direction tables (main + interaction)
#'
#' @param boot_obj Output from \code{bootstrap_survey_shap()}.
#' @param conf Confidence level.
#' @param top_k Top-k probability cutoff for positive/negative separately.
#' @return A list with \code{main} and \code{interaction} data.frames.
#' @export
summarise_bootstrap_direction <- function(boot_obj, conf = 0.95, top_k = 10) {
  dir_sum <- function(mat, hat, conf, top_k) {
    nm <- colnames(mat)
    alpha <- (1 - conf) / 2
    probs <- c(alpha, 1 - alpha)

    mean_b <- apply(mat, 2, function(x) mean(x, na.rm = TRUE))
    sd_b   <- apply(mat, 2, function(x) stats::sd(x, na.rm = TRUE))
    ci     <- t(apply(mat, 2, function(x) stats::quantile(x, probs = probs, na.rm = TRUE, names = FALSE)))
    colnames(ci) <- c("ci_lo", "ci_hi")
    present_prob <- apply(mat, 2, function(x) mean(!is.na(x)))

    pos_prob <- rep(NA_real_, length(nm))
    neg_prob <- rep(NA_real_, length(nm))

    if (ncol(mat) > 0) {
      pos_count <- rep(0L, length(nm))
      neg_count <- rep(0L, length(nm))
      for (b in seq_len(nrow(mat))) {
        x <- mat[b, ]
        if (all(is.na(x))) next
        kk <- min(top_k, sum(!is.na(x)))

        idx_pos <- order(x, decreasing = TRUE, na.last = NA)[seq_len(kk)]
        idx_neg <- order(x, decreasing = FALSE, na.last = NA)[seq_len(kk)]

        pos_count[idx_pos] <- pos_count[idx_pos] + 1L
        neg_count[idx_neg] <- neg_count[idx_neg] + 1L
      }
      pos_prob <- pos_count / nrow(mat)
      neg_prob <- neg_count / nrow(mat)
    }

    est <- rep(NA_real_, length(nm)); names(est) <- nm
    keep <- intersect(names(hat), nm)
    est[keep] <- hat[keep]

    data.frame(
      key = nm,
      estimate = as.numeric(est),
      mean_boot = as.numeric(mean_b),
      sd_boot = as.numeric(sd_b),
      ci_lo = ci[, "ci_lo"],
      ci_hi = ci[, "ci_hi"],
      present_prob = as.numeric(present_prob),
      top_pos_prob = as.numeric(pos_prob),
      top_neg_prob = as.numeric(neg_prob),
      stringsAsFactors = FALSE
    )
  }

  dm_hat <- boot_obj$baseline$direction_main$v
  di_hat <- boot_obj$baseline$direction_int$v

  dm <- dir_sum(boot_obj$boot$dir_main_mat, dm_hat, conf = conf, top_k = top_k)
  di <- dir_sum(boot_obj$boot$dir_int_mat,  di_hat, conf = conf, top_k = top_k)

  list(main = dm, interaction = di)
}


#' Decode main direction keys "onehot" into (variable, level)
#'
#' In your repo, one-way direction keys are stored as one-hot column names like:
#'   "partisanDem", "raceWhite", "genderMen", "collegeYes", "gun_ownYes"
#'
#' This helper splits the key by matching known prefixes.
#'
#' @param df A data.frame with column \code{key} that equals the one-hot name.
#' @param prefixes Character vector of group prefixes (default matches your package).
#' @return The same data.frame with added \code{variable} and \code{level} columns.
#' @export
decode_main_direction_keys <- function(df,
                                       prefixes = c("gun_own", "partisan", "race", "gender", "college")) {
  stopifnot(is.data.frame(df), "key" %in% names(df))
  df$variable <- NA_character_
  df$level <- NA_character_

  for (pfx in prefixes) {
    hit <- startsWith(df$key, pfx)
    if (any(hit)) {
      df$variable[hit] <- pfx
      df$level[hit] <- substring(df$key[hit], nchar(pfx) + 1L)
    }
  }
  df
}


#' Decode interaction direction keys "feat_i×feat_j" into columns
#'
#' @param df A data.frame with column \code{key}.
#' @return The same data.frame with added \code{feat_i} and \code{feat_j} columns.
#' @export
decode_interaction_direction_keys <- function(df) {
  stopifnot(is.data.frame(df), "key" %in% names(df))
  sp <- strsplit(df$key, "×", fixed = TRUE)
  df$feat_i <- vapply(sp, `[`, character(1), 1)
  df$feat_j <- vapply(sp, function(x) paste(x[-1], collapse = "×"), character(1))
  df
}
