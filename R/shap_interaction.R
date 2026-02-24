#' Compute SHAP interaction values on a subsample
#'
#' Uses xgboost native interaction SHAP via `predinteraction = TRUE`.
#' This can be memory-heavy; subsampling is recommended.
#'
#' @param model An xgb.Booster.
#' @param X Full feature matrix (dgCMatrix).
#' @param subsample_n Number of rows to sample for interaction SHAP.
#' @param seed Random seed for subsampling.
#' @return A list with `shap_int_feat` (m x p x p array), `idx_sub`, and `X_sub`.
#' @export
compute_shap_interaction <- function(model,
                                    X,
                                    subsample_n = 5000L,
                                    seed = 123,
                                    w_full = NULL) {
  stopifnot(inherits(X, "dgCMatrix"))
  n <- nrow(X)
  subsample_n <- as.integer(min(subsample_n, n))
  if (subsample_n < 2L) stop("subsample_n must be >= 2")

  if (!is.null(w_full)) {
    w_full <- as.numeric(w_full)
    if (length(w_full) != n) stop("`w_full` must have length nrow(X).")
    if (any(!is.finite(w_full)) || any(w_full < 0)) stop("`w_full` must be finite and nonnegative.")
  }

  set.seed(seed)
  idx_sub <- sample.int(n, subsample_n, replace = FALSE)
  X_sub <- X[idx_sub, , drop = FALSE]
  dsub <- xgboost::xgb.DMatrix(data = X_sub)

  shap_int <- stats::predict(model, dsub, predinteraction = TRUE)

  p1 <- dim(shap_int)[2]
  p <- p1 - 1L
  shap_int_feat <- shap_int[, 1:p, 1:p, drop = FALSE]  # drop bias dimension

  w_sub <- if (is.null(w_full)) NULL else w_full[idx_sub]

  list(shap_int_feat = shap_int_feat, idx_sub = idx_sub, X_sub = X_sub, w_sub = w_sub)
}


#' Interaction strength by group pair (excluding within-factor pairs)
#'
#' Aggregates mean(|interaction|) over one-hot pairs to group-pair totals, excluding
#' within-factor pairs like partisan x partisan.
#'
#' @param shap_int_feat m x p x p interaction array excluding bias.
#' @param feature_names One-hot feature names (length p).
#' @param feat_group Group labels (length p); if NULL computed by `feature_to_group()`.
#' @return A data.frame with columns `pair` and `strength`, sorted decreasing.
#' @export
shap_strength_interaction_group <- function(shap_int_feat,
                                           feature_names,
                                           feat_group = NULL,
                                           w = NULL) {
  stopifnot(length(dim(shap_int_feat)) == 3)
  p <- dim(shap_int_feat)[2]
  stopifnot(length(feature_names) == p)

  if (is.null(feat_group)) {
    feat_group <- feature_to_group(feature_names)
  }

  if (!is.null(w)) {
    w <- as.numeric(w)
    if (length(w) != dim(shap_int_feat)[1]) stop("`w` must have length m (rows of interaction array).")
    if (any(!is.finite(w)) || any(w < 0)) stop("`w` must be finite and nonnegative.")
    wsum <- sum(w)
    if (!is.finite(wsum) || wsum <= 0) stop("Invalid weights: sum(w) must be > 0.")
    # weighted mean over rows for each (i,j)
    int_strength_mat <- apply(abs(shap_int_feat), c(2, 3), function(v) sum(v * w) / wsum)
  } else {
    int_strength_mat <- apply(abs(shap_int_feat), c(2, 3), mean)
  }

  pairs <- which(upper.tri(int_strength_mat), arr.ind = TRUE)

  df <- data.frame(
    feat_i = feature_names[pairs[, 1]],
    feat_j = feature_names[pairs[, 2]],
    strength = int_strength_mat[pairs],
    stringsAsFactors = FALSE
  )

  df$group_i <- feat_group[pairs[, 1]]
  df$group_j <- feat_group[pairs[, 2]]

  df <- df[!is.na(df$group_i) & !is.na(df$group_j) & df$group_i != df$group_j, , drop = FALSE]

  gi <- df$group_i
  gj <- df$group_j
  df$pair <- ifelse(gi <= gj, paste0(gi, " × ", gj), paste0(gj, " × ", gi))

  agg <- stats::aggregate(strength ~ pair, data = df, FUN = sum)
  agg[order(-agg$strength), , drop = FALSE]
}


#' Interaction direction (active-active) for one-hot pairs
#'
#' For each one-hot pair (i, j), averages the interaction SHAP among rows where
#' BOTH one-hot indicators are active (non-zero). Excludes within-factor pairs.
#'
#' @param shap_int_feat m x p x p interaction array (excluding bias).
#' @param X_sub The feature matrix used to compute `shap_int_feat` (m x p).
#' @param feature_names One-hot feature names (length p).
#' @param feat_group Group labels (length p); if NULL computed by `feature_to_group()`.
#' @return A data.frame with columns feat_i, feat_j, direction_active, n_active, group_i, group_j.
#' @export
shap_direction_interaction_active <- function(shap_int_feat,
                                             X_sub,
                                             feature_names,
                                             feat_group = NULL,
                                             w = NULL) {
  stopifnot(length(dim(shap_int_feat)) == 3)
  stopifnot(inherits(X_sub, "dgCMatrix"))
  p <- dim(shap_int_feat)[2]
  stopifnot(ncol(X_sub) == p, length(feature_names) == p)

  if (is.null(feat_group)) {
    feat_group <- feature_to_group(feature_names)
  }

  if (!is.null(w)) {
    w <- as.numeric(w)
    if (length(w) != nrow(X_sub)) stop("`w` must have length nrow(X_sub).")
    if (any(!is.finite(w)) || any(w < 0)) stop("`w` must be finite and nonnegative.")
  }

  pairs <- which(upper.tri(matrix(1, p, p)), arr.ind = TRUE)

  df <- data.frame(
    i = pairs[, 1],
    j = pairs[, 2],
    feat_i = feature_names[pairs[, 1]],
    feat_j = feature_names[pairs[, 2]],
    group_i = feat_group[pairs[, 1]],
    group_j = feat_group[pairs[, 2]],
    stringsAsFactors = FALSE
  )

  df <- df[!is.na(df$group_i) & !is.na(df$group_j) & df$group_i != df$group_j, , drop = FALSE]

  active_mean <- numeric(nrow(df))
  n_active <- numeric(nrow(df))

  for (k in seq_len(nrow(df))) {
    ii <- df$i[k]
    jj <- df$j[k]

    active_idx <- which(X_sub[, ii] != 0 & X_sub[, jj] != 0)
    if (length(active_idx) == 0L) {
      active_mean[k] <- NA_real_
      n_active[k] <- 0
    } else if (is.null(w)) {
      active_mean[k] <- mean(shap_int_feat[active_idx, ii, jj])
      n_active[k] <- length(active_idx)
    } else {
      ww <- w[active_idx]
      wsum <- sum(ww)
      active_mean[k] <- sum(shap_int_feat[active_idx, ii, jj] * ww) / wsum
      n_active[k] <- wsum
    }
  }

  df$direction_active <- active_mean
  df$n_active <- n_active
  df <- df[!is.na(df$direction_active), , drop = FALSE]

  df <- df[order(-df$direction_active), , drop = FALSE]
  df[, c("feat_i","feat_j","group_i","group_j","direction_active","n_active")]
}
