############################################################
# Bootstrap utilities (targeted; weight-aware)
# - boot_strength(): bootstrap ONE strength target (main or interaction)
# - boot_direction(): bootstrap ONE direction target (main level or interaction level/stratum)
#
# NOTE: For numeric variables (educ, rucc), "main direction by level" is undefined;
# boot_direction(feature="educ", level=NA) returns NA.
#
# For interaction direction:
# - categorical × categorical: require both levels (active-active rows)
# - numeric × categorical: specify level=NA for numeric and level=<cat-level> for categorical
############################################################

.canon_pair <- function(feature, level = NULL) {
  stopifnot(is.character(feature), length(feature) %in% c(1L, 2L))
  if (length(feature) == 1L) {
    if (!is.null(level)) stopifnot(length(level) == 1L)
    return(list(feature = feature, level = level))
  }

  stopifnot(length(feature) == 2L)
  if (!is.null(level)) stopifnot(length(level) == 2L)

  ord <- order(feature)
  feature2 <- feature[ord]
  level2 <- if (is.null(level)) NULL else level[ord]
  list(feature = feature2, level = level2)
}

.make_pair_label <- function(f1, f2) {
  if (f1 <= f2) paste0(f1, " × ", f2) else paste0(f2, " × ", f1)
}

.wmean <- function(x, w) {
  x <- as.numeric(x)
  w <- as.numeric(w)
  ok <- !is.na(x) & !is.na(w) & w > 0
  if (!any(ok)) return(NA_real_)
  sum(w[ok] * x[ok]) / sum(w[ok])
}

.n_eff <- function(w) {
  w <- as.numeric(w)
  ok <- !is.na(w) & w > 0
  if (!any(ok)) return(0)
  sw <- sum(w[ok])
  sw2 <- sum(w[ok]^2)
  if (sw2 <= 0) return(0)
  (sw^2) / sw2
}

.summarise_boot_scalar <- function(estimate, boot, conf = 0.95) {
  boot <- as.numeric(boot)
  ok <- !is.na(boot)
  present_prob <- mean(ok)

  if (sum(ok) == 0L) {
    return(data.frame(
      estimate = estimate,
      mean_boot = NA_real_,
      sd_boot = NA_real_,
      ci_lo = NA_real_,
      ci_hi = NA_real_,
      present_prob = present_prob,
      stringsAsFactors = FALSE
    ))
  }

  alpha <- (1 - conf) / 2
  qs <- stats::quantile(boot[ok], probs = c(alpha, 1 - alpha), names = FALSE, type = 7)

  data.frame(
    estimate = estimate,
    mean_boot = mean(boot[ok]),
    sd_boot = stats::sd(boot[ok]),
    ci_lo = qs[1],
    ci_hi = qs[2],
    present_prob = present_prob,
    stringsAsFactors = FALSE
  )
}

.bootstrap_scalar <- function(data, B, seed, parallel, n_cores, FUN_one) {
  stopifnot(is.data.frame(data))
  B <- as.integer(B)
  if (B < 1L) stop("B must be >= 1")
  if (!is.null(seed)) seed <- as.integer(seed)

  if (is.null(seed)) {
    seeds <- sample.int(.Machine$integer.max, B)
  } else {
    seeds <- seed + seq_len(B)
  }

  n <- nrow(data)

  one_rep <- function(b) {
    s <- seeds[b]
    set.seed(s)
    idx <- sample.int(n, n, replace = TRUE)
    dat_b <- data[idx, , drop = FALSE]

    out <- tryCatch(
      FUN_one(dat_b, seed = s),
      error = function(e) NA_real_
    )
    as.numeric(out)
  }

  if (isTRUE(parallel)) {
    if (is.null(n_cores)) {
      n_cores <- max(1L, parallel::detectCores() - 1L)
    }
    n_cores <- as.integer(n_cores)

    cl <- parallel::makeCluster(n_cores)
    on.exit(parallel::stopCluster(cl), add = TRUE)

    parallel::clusterEvalQ(cl, {
      suppressPackageStartupMessages({
        library(xgboost)
        library(Matrix)
      })
      NULL
    })

    to_export <- unique(c(
      ".canon_pair", ".make_pair_label", ".wmean", ".n_eff",
      ".summarise_boot_scalar", ".bootstrap_scalar",
      "build_xgb_design", "fit_survey_xgb",
      "compute_shap_main", "compute_shap_interaction",
      "shap_strength_main_group", "shap_strength_interaction_group",
      "active_shap_by_group"
    ))

    parallel::clusterExport(cl, varlist = to_export, envir = environment())

    boot <- unlist(parallel::parLapply(cl, seq_len(B), one_rep), use.names = FALSE)
  } else {
    boot <- vapply(seq_len(B), one_rep, numeric(1))
  }

  list(boot = boot, seeds = seeds)
}

# --- target computations ------------------------------------------------------

.compute_strength_target <- function(data, feature, interaction_subsample_n, seed) {
  feature <- .canon_pair(feature)$feature

  design <- build_xgb_design(data)

  if (is.null(seed)) {
    fit <- fit_survey_xgb(design$X, design$y, w = design$w)
  } else {
    fit <- fit_survey_xgb(design$X, design$y, w = design$w, seed = as.integer(seed))
  }

  if (length(feature) == 1L) {
    main <- compute_shap_main(fit$model, fit$dall, feature_names = design$feature_names)
    df_strength <- shap_strength_main_group(main$shap_feat, design$feature_names, w = design$w)
    val <- df_strength$strength[df_strength$group == feature]
    if (length(val) != 1L) stop("Group not found in strength_main: ", feature)
    return(as.numeric(val))
  }

  if (is.null(seed)) {
    inter <- compute_shap_interaction(fit$model, design$X, subsample_n = interaction_subsample_n, w = design$w)
  } else {
    inter <- compute_shap_interaction(fit$model, design$X, subsample_n = interaction_subsample_n, seed = as.integer(seed), w = design$w)
  }

  df_int <- shap_strength_interaction_group(inter$shap_int_feat, design$feature_names, w_sub = inter$w_sub)

  pair_label <- .make_pair_label(feature[1], feature[2])
  val <- df_int$strength[df_int$pair == pair_label]
  if (length(val) != 1L) stop("Pair not found in strength_interaction: ", pair_label)
  as.numeric(val)
}

.compute_direction_target <- function(data, feature, level, interaction_subsample_n, seed) {
  canon <- .canon_pair(feature, level)
  feature <- canon$feature
  level <- canon$level

  design <- build_xgb_design(data)

  if (is.null(seed)) {
    fit <- fit_survey_xgb(design$X, design$y, w = design$w)
  } else {
    fit <- fit_survey_xgb(design$X, design$y, w = design$w, seed = as.integer(seed))
  }

  numeric_vars <- c("educ", "rucc")

  if (length(feature) == 1L) {
    if (feature %in% numeric_vars) return(NA_real_)  # undefined
    main <- compute_shap_main(fit$model, fit$dall, feature_names = design$feature_names)
    tab <- active_shap_by_group(feature, as.factor(design$df[[feature]]), main$shap_feat, w = design$w, min_n_eff = 0)
    val <- tab$shap_active[tab$level == level]
    if (length(val) != 1L) stop("Level not found for main direction: ", paste0(feature, "=", level))
    return(as.numeric(val))
  }

  # interaction direction
  f1 <- feature[1]; f2 <- feature[2]
  l1 <- level[1];   l2 <- level[2]

  if (is.null(seed)) {
    inter <- compute_shap_interaction(fit$model, design$X, subsample_n = interaction_subsample_n, w = design$w)
  } else {
    inter <- compute_shap_interaction(fit$model, design$X, subsample_n = interaction_subsample_n, seed = as.integer(seed), w = design$w)
  }

  feature_names <- design$feature_names
  X_sub <- inter$X_sub
  shap_int_feat <- inter$shap_int_feat
  ww <- if (is.null(inter$w_sub)) rep(1, nrow(X_sub)) else as.numeric(inter$w_sub)

  f1_num <- f1 %in% numeric_vars
  f2_num <- f2 %in% numeric_vars

  # indices
  ii <- match(if (f1_num) f1 else paste0(f1, l1), feature_names)
  jj <- match(if (f2_num) f2 else paste0(f2, l2), feature_names)

  if (is.na(ii) || is.na(jj)) stop("Feature columns not found in design matrix.")

  if (!f1_num && !f2_num) {
    idx <- which(X_sub[, ii] != 0 & X_sub[, jj] != 0)
    if (length(idx) == 0L) stop("No co-active rows for this level-pair in interaction subsample.")
    return(.wmean(shap_int_feat[idx, ii, jj], ww[idx]))
  }

  if (f1_num && f2_num) {
    return(.wmean(shap_int_feat[, ii, jj], ww))
  }

  # one numeric, one categorical: subset where the categorical one-hot is active
  if (f1_num && !f2_num) {
    idx <- which(X_sub[, jj] != 0)
    if (length(idx) == 0L) stop("No active rows for categorical level in interaction subsample.")
    return(.wmean(shap_int_feat[idx, ii, jj], ww[idx]))
  }
  if (!f1_num && f2_num) {
    idx <- which(X_sub[, ii] != 0)
    if (length(idx) == 0L) stop("No active rows for categorical level in interaction subsample.")
    return(.wmean(shap_int_feat[idx, ii, jj], ww[idx]))
  }

  NA_real_
}

# --- exported: boot_strength --------------------------------------------------

#' Bootstrap a single SHAP strength target (main effect or interaction)
#'
#' @param data Input data.frame (same as run_survey_shap()).
#' @param feature Character vector length 1 (main) or 2 (interaction).
#' @param B Number of bootstrap replicates.
#' @param conf CI level for percentile CI (default 0.95).
#' @param seed Optional integer seed for reproducibility.
#' @param parallel Logical; use parallel workers?
#' @param n_cores Optional integer number of workers when parallel=TRUE.
#' @param interaction_subsample_n Subsample size for interaction SHAP.
#' @return A list with target, estimate, boot vector, and summary.
#' @export
boot_strength <- function(data,
                          feature,
                          B = 200L,
                          conf = 0.95,
                          seed = 1L,
                          parallel = TRUE,
                          n_cores = NULL,
                          interaction_subsample_n = 5000L) {
  stopifnot(is.data.frame(data))
  stopifnot(is.character(feature), length(feature) %in% c(1L, 2L))

  canon <- .canon_pair(feature)
  feature <- canon$feature

  estimate <- .compute_strength_target(
    data = data,
    feature = feature,
    interaction_subsample_n = interaction_subsample_n,
    seed = seed
  )

  FUN_one <- function(dat_b, seed) {
    .compute_strength_target(
      data = dat_b,
      feature = feature,
      interaction_subsample_n = interaction_subsample_n,
      seed = seed
    )
  }

  bt <- .bootstrap_scalar(
    data = data,
    B = B,
    seed = seed,
    parallel = parallel,
    n_cores = n_cores,
    FUN_one = FUN_one
  )

  target <- if (length(feature) == 1L) {
    list(type = "strength_main", feature = feature)
  } else {
    list(type = "strength_interaction", feature = feature, pair = .make_pair_label(feature[1], feature[2]))
  }

  list(
    target = target,
    estimate = estimate,
    boot = bt$boot,
    summary = .summarise_boot_scalar(estimate, bt$boot, conf = conf),
    meta = list(B = as.integer(B), conf = conf, seed = seed, parallel = parallel, n_cores = n_cores,
                interaction_subsample_n = as.integer(interaction_subsample_n))
  )
}

# --- exported: boot_direction -------------------------------------------------

#' Bootstrap a single SHAP direction target (active SHAP / interaction SHAP)
#'
#' Main (categorical): direction(feature="partisan", level="Dem.") = weighted mean active SHAP for that level.
#' Main (numeric): returns NA.
#'
#' Interaction (categorical × categorical): weighted mean interaction SHAP among rows where BOTH one-hot indicators are active.
#' Interaction (numeric × categorical): specify level=NA for numeric and level=<cat-level> for categorical; computes mean among rows where the categorical one-hot is active.
#'
#' @param data Input data.frame.
#' @param feature Character vector length 1 or 2.
#' @param level Character vector length 1 or 2 (must match feature length; may contain NA for numeric features).
#' @param B Number of bootstrap replicates.
#' @param conf CI level for percentile CI (default 0.95).
#' @param seed Optional integer seed for reproducibility.
#' @param parallel Logical; use parallel workers?
#' @param n_cores Optional integer number of workers when parallel=TRUE.
#' @param interaction_subsample_n Subsample size for interaction SHAP.
#' @return A list with target, estimate, boot vector, and summary.
#' @export
boot_direction <- function(data,
                           feature,
                           level,
                           B = 200L,
                           conf = 0.95,
                           seed = 1L,
                           parallel = TRUE,
                           n_cores = NULL,
                           interaction_subsample_n = 5000L) {
  stopifnot(is.data.frame(data))
  stopifnot(is.character(feature), length(feature) %in% c(1L, 2L))
  stopifnot(length(level) == length(feature))

  canon <- .canon_pair(feature, as.character(level))
  feature <- canon$feature
  level <- canon$level

  estimate <- .compute_direction_target(
    data = data,
    feature = feature,
    level = level,
    interaction_subsample_n = interaction_subsample_n,
    seed = seed
  )

  FUN_one <- function(dat_b, seed) {
    .compute_direction_target(
      data = dat_b,
      feature = feature,
      level = level,
      interaction_subsample_n = interaction_subsample_n,
      seed = seed
    )
  }

  bt <- .bootstrap_scalar(
    data = data,
    B = B,
    seed = seed,
    parallel = parallel,
    n_cores = n_cores,
    FUN_one = FUN_one
  )

  target <- if (length(feature) == 1L) {
    list(type = "direction_main", feature = feature, level = level)
  } else {
    list(type = "direction_interaction", feature = feature, level = level, pair = .make_pair_label(feature[1], feature[2]))
  }

  list(
    target = target,
    estimate = estimate,
    boot = bt$boot,
    summary = .summarise_boot_scalar(estimate, bt$boot, conf = conf),
    meta = list(B = as.integer(B), conf = conf, seed = seed, parallel = parallel, n_cores = n_cores,
                interaction_subsample_n = as.integer(interaction_subsample_n))
  )
}
