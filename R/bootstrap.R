############################################################
# Bootstrap utilities (targeted)
# - boot_strength(): bootstrap ONE strength target (main or interaction)
# - boot_direction(): bootstrap ONE direction target (main or interaction level-pair)
#
# Drop this whole file into R/ (e.g., R/bootstrap_targeted.R)
############################################################

# NOTE:
# These functions intentionally reuse your existing package internals:
#   build_xgb_design(), fit_survey_xgb(), compute_shap_main(), compute_shap_interaction(),
#   shap_strength_main_group(), shap_strength_interaction_group(), active_shap_by_group()
#
# They do NOT rely on your old "bootstrap everything then summarize" workflow.

# --- internal helpers ---------------------------------------------------------

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

  # deterministic per-replicate seeds
  if (is.null(seed)) {
    seeds <- sample.int(.Machine$integer.max, B)
  } else {
    seeds <- seed + seq_len(B)
  }

  n <- nrow(data)

  # worker function
  one_rep <- function(b) {
    s <- seeds[b]
    set.seed(s)
    idx <- sample.int(n, n, replace = TRUE)
    dat_b <- data[idx, , drop = FALSE]

    # run computation; fail -> NA
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

    # Load packages needed on workers
    parallel::clusterEvalQ(cl, {
      suppressPackageStartupMessages({
        library(xgboost)
        library(Matrix)
      })
      NULL
    })

    # Export symbols from current namespace/session.
    # Assumes this file is part of the package namespace when running in-package.
    to_export <- unique(c(
      # from this file
      ".canon_pair", ".make_pair_label", ".summarise_boot_scalar", ".bootstrap_scalar",
      # core pipeline functions you already have in your package
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

  # fit
  if (is.null(seed)) {
    fit <- fit_survey_xgb(design$X, design$y)
  } else {
    fit <- fit_survey_xgb(design$X, design$y, seed = as.integer(seed))
  }

  if (length(feature) == 1L) {
    main <- compute_shap_main(fit$model, fit$dall, feature_names = design$feature_names)
    df_strength <- shap_strength_main_group(main$shap_feat, design$feature_names)
    val <- df_strength$strength[df_strength$group == feature]
    if (length(val) != 1L) stop("Group not found in strength_main: ", feature)
    return(as.numeric(val))
  }

  # interaction strength
  if (is.null(seed)) {
    inter <- compute_shap_interaction(fit$model, design$X, subsample_n = interaction_subsample_n)
  } else {
    inter <- compute_shap_interaction(fit$model, design$X, subsample_n = interaction_subsample_n, seed = as.integer(seed))
  }

  df_int <- shap_strength_interaction_group(inter$shap_int_feat, design$feature_names)

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

  # fit
  if (is.null(seed)) {
    fit <- fit_survey_xgb(design$X, design$y)
  } else {
    fit <- fit_survey_xgb(design$X, design$y, seed = as.integer(seed))
  }

  # main shap always needed for main direction
  if (length(feature) == 1L) {
    main <- compute_shap_main(fit$model, fit$dall, feature_names = design$feature_names)

    # active_shap_by_group already returns mean by level
    tab <- active_shap_by_group(feature, design$df[[feature]], main$shap_feat)
    val <- tab$shap_active[tab$level == level]
    if (length(val) != 1L) stop("Level not found for main direction: ", paste0(feature, "=", level))
    return(as.numeric(val))
  }

  # interaction direction for (feat1,level1) × (feat2,level2)
  onehot1 <- paste0(feature[1], level[1])
  onehot2 <- paste0(feature[2], level[2])

  if (is.null(seed)) {
    inter <- compute_shap_interaction(fit$model, design$X, subsample_n = interaction_subsample_n)
  } else {
    inter <- compute_shap_interaction(fit$model, design$X, subsample_n = interaction_subsample_n, seed = as.integer(seed))
  }

  feature_names <- design$feature_names
  ii <- match(onehot1, feature_names)
  jj <- match(onehot2, feature_names)
  if (is.na(ii) || is.na(jj)) {
    stop("One-hot not found in design matrix: ",
         paste0(onehot1, if (is.na(ii)) " (missing)" else ""),
         " ; ",
         paste0(onehot2, if (is.na(jj)) " (missing)" else ""))
  }

  # shap_int_feat is m x p x p with p = length(feature_names)
  # X_sub is m x p used for predinteraction
  X_sub <- inter$X_sub
  shap_int_feat <- inter$shap_int_feat

  active_idx <- which(X_sub[, ii] != 0 & X_sub[, jj] != 0)
  if (length(active_idx) == 0L) stop("No active rows for this level-pair in interaction subsample.")
  mean(shap_int_feat[active_idx, ii, jj])
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

  # baseline estimate (non-bootstrap)
  estimate <- .compute_strength_target(
    data = data,
    feature = feature,
    interaction_subsample_n = interaction_subsample_n,
    seed = seed
  )

  # bootstrap
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

#' Bootstrap a single SHAP direction target (active SHAP)
#'
#' Main: direction(feature="partisan", level="Dem.") = mean active SHAP for that level.
#' Interaction: direction((feat1,level1) × (feat2,level2)) = mean interaction SHAP among rows
#' where BOTH one-hot indicators are active in the interaction subsample.
#'
#' @param data Input data.frame.
#' @param feature Character vector length 1 or 2.
#' @param level Character vector length 1 or 2 (must match feature length).
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
  stopifnot(is.character(level), length(level) == length(feature))

  canon <- .canon_pair(feature, level)
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
    list(type = "direction_main", feature = feature, level = level, onehot = paste0(feature, level))
  } else {
    list(
      type = "direction_interaction",
      feature = feature,
      level = level,
      onehot = c(paste0(feature[1], level[1]), paste0(feature[2], level[2])),
      pair = .make_pair_label(feature[1], feature[2])
    )
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
