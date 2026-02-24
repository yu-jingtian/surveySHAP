#' Compute SHAP interaction values on a subsample
#'
#' Uses xgboost native interaction SHAP via `predinteraction = TRUE`.
#' This can be memory-heavy; subsampling is recommended.
#'
#' @param model An xgb.Booster.
#' @param X Full feature matrix (dgCMatrix).
#' @param subsample_n Number of rows to sample for interaction SHAP.
#' @param seed Random seed for subsampling.
#' @param w Optional sample weights (length nrow(X)); used only for returning `w_sub`
#'   so downstream summaries can be weighted.
#' @return A list with `shap_int_feat` (m x p x p array), `idx_sub`, `X_sub`, and `w_sub`.
#' @export
compute_shap_interaction <- function(model, X, subsample_n = 5000L, seed = 123, w = NULL) {
  stopifnot(inherits(X, "dgCMatrix"))
  n <- nrow(X)
  subsample_n <- as.integer(min(subsample_n, n))
  if (subsample_n < 2L) stop("subsample_n must be >= 2")

  set.seed(seed)
  idx_sub <- sample.int(n, subsample_n, replace = FALSE)
  X_sub <- X[idx_sub, , drop = FALSE]
  dsub <- xgboost::xgb.DMatrix(data = X_sub)

  shap_int <- stats::predict(model, dsub, predinteraction = TRUE)

  p1 <- dim(shap_int)[2]
  p <- p1 - 1L
  shap_int_feat <- shap_int[, 1:p, 1:p, drop = FALSE]  # drop bias dimension

  w_sub <- NULL
  if (!is.null(w)) {
    stopifnot(length(w) == n)
    w_sub <- as.numeric(w)[idx_sub]
  }

  list(shap_int_feat = shap_int_feat, idx_sub = idx_sub, X_sub = X_sub, w_sub = w_sub)
}

# ---- internal helpers --------------------------------------------------------

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

#' Interaction strength by group pair (weighted; excluding within-group pairs)
#'
#' Aggregates weighted mean(|interaction|) over feature pairs to group-pair totals,
#' excluding within-group pairs like partisan × partisan.
#'
#' @param shap_int_feat m x p x p interaction array excluding bias.
#' @param feature_names Feature names (length p).
#' @param feat_group Group labels (length p); if NULL computed by `feature_to_group()`.
#' @param w_sub Optional sample weights for the interaction subsample (length m).
#' @return A data.frame with columns `pair` and `strength`, sorted decreasing.
#' @export
shap_strength_interaction_group <- function(shap_int_feat, feature_names, feat_group = NULL, w_sub = NULL) {
  stopifnot(length(dim(shap_int_feat)) == 3)
  p <- dim(shap_int_feat)[2]
  m <- dim(shap_int_feat)[1]
  stopifnot(length(feature_names) == p)

  if (is.null(feat_group)) {
    feat_group <- feature_to_group(feature_names)
  }

  if (is.null(w_sub)) {
    int_strength_mat <- apply(abs(shap_int_feat), c(2, 3), mean)
  } else {
    stopifnot(length(w_sub) == m)
    ww <- as.numeric(w_sub)
    int_strength_mat <- apply(abs(shap_int_feat), c(2, 3), function(v) .wmean(v, ww))
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

# ---- direction summaries -----------------------------------------------------

#' Interaction direction summaries (weighted; supports continuous × categorical)
#'
#' Provides direction summaries for:
#' - one-hot × one-hot: "active-active" mean interaction SHAP within co-active rows
#' - numeric × one-hot: mean interaction SHAP within rows where the one-hot is active
#' - numeric × numeric: global mean interaction SHAP (no levels)
#'
#' Filtering uses effective sample size (ESS) on the subset used for the mean.
#'
#' @param shap_int_feat m x p x p interaction array (excluding bias).
#' @param X_sub The feature matrix used to compute `shap_int_feat` (m x p).
#' @param feature_names Feature names (length p).
#' @param feat_group Group labels (length p); if NULL computed by `feature_to_group()`.
#' @param w_sub Optional weights for the interaction subsample (length m).
#' @param min_n_eff Minimum ESS for keeping a direction row (default 100).
#' @return A data.frame with interaction direction rows and ESS diagnostics.
#' @export
shap_direction_interaction_active <- function(
    shap_int_feat,
    X_sub,
    feature_names,
    feat_group = NULL,
    w_sub = NULL,
    min_n_eff = 100
) {
  stopifnot(length(dim(shap_int_feat)) == 3)
  stopifnot(inherits(X_sub, "dgCMatrix"))
  p <- dim(shap_int_feat)[2]
  m <- dim(shap_int_feat)[1]
  stopifnot(ncol(X_sub) == p, length(feature_names) == p)
  min_n_eff <- as.numeric(min_n_eff)

  if (is.null(feat_group)) {
    feat_group <- feature_to_group(feature_names)
  }

  if (is.null(w_sub)) {
    ww <- rep(1, m)
  } else {
    stopifnot(length(w_sub) == m)
    ww <- as.numeric(w_sub)
  }

  # determine which features are "one-hot" vs "numeric single-column"
  # heuristic: numeric features have exact name equal to their group (educ, rucc);
  # one-hot features start with group prefix.
  is_numeric_feat <- !is.na(feat_group) & feature_names == feat_group

  pairs <- which(upper.tri(matrix(1, p, p)), arr.ind = TRUE)

  rows <- vector("list", nrow(pairs))
  out_k <- 0L

  for (k in seq_len(nrow(pairs))) {
    ii <- pairs[k, 1]
    jj <- pairs[k, 2]

    gi <- feat_group[ii]
    gj <- feat_group[jj]
    if (is.na(gi) || is.na(gj) || gi == gj) next

    fi <- feature_names[ii]
    fj <- feature_names[jj]

    i_num <- is_numeric_feat[ii]
    j_num <- is_numeric_feat[jj]

    # helper to compute weighted mean interaction on subset
    mean_on <- function(idx) {
      if (length(idx) == 0L) return(list(mean = NA_real_, n_raw = 0L, w_sum = 0, n_eff = 0))
      v <- shap_int_feat[idx, ii, jj]
      w0 <- ww[idx]
      list(
        mean = .wmean(v, w0),
        n_raw = length(idx),
        w_sum = sum(w0, na.rm = TRUE),
        n_eff = .n_eff(w0)
      )
    }

    if (!i_num && !j_num) {
      # one-hot × one-hot: co-active rows
      idx <- which(X_sub[, ii] != 0 & X_sub[, jj] != 0)
      s <- mean_on(idx)
      if (!is.na(s$mean) && s$n_eff >= min_n_eff) {
        out_k <- out_k + 1L
        rows[[out_k]] <- data.frame(
          type = "cat×cat",
          feat_i = fi, feat_j = fj,
          group_i = gi, group_j = gj,
          level_i = sub(paste0("^", gi), "", fi),
          level_j = sub(paste0("^", gj), "", fj),
          direction = s$mean,
          n_raw = s$n_raw,
          w_sum = s$w_sum,
          n_eff = s$n_eff,
          stringsAsFactors = FALSE
        )
      }
      next
    }

    if (i_num && j_num) {
      # numeric × numeric: global mean
      s <- mean_on(seq_len(m))
      if (!is.na(s$mean) && s$n_eff >= min_n_eff) {
        out_k <- out_k + 1L
        rows[[out_k]] <- data.frame(
          type = "num×num",
          feat_i = fi, feat_j = fj,
          group_i = gi, group_j = gj,
          level_i = NA_character_, level_j = NA_character_,
          direction = s$mean,
          n_raw = s$n_raw,
          w_sum = s$w_sum,
          n_eff = s$n_eff,
          stringsAsFactors = FALSE
        )
      }
      next
    }

    # numeric × one-hot: subset where one-hot is active; report by the one-hot level
    if (i_num && !j_num) {
      idx <- which(X_sub[, jj] != 0)
      s <- mean_on(idx)
      if (!is.na(s$mean) && s$n_eff >= min_n_eff) {
        out_k <- out_k + 1L
        rows[[out_k]] <- data.frame(
          type = "num×cat",
          feat_i = fi, feat_j = fj,
          group_i = gi, group_j = gj,
          level_i = NA_character_,
          level_j = sub(paste0("^", gj), "", fj),
          direction = s$mean,
          n_raw = s$n_raw,
          w_sum = s$w_sum,
          n_eff = s$n_eff,
          stringsAsFactors = FALSE
        )
      }
      next
    }

    if (!i_num && j_num) {
      idx <- which(X_sub[, ii] != 0)
      s <- mean_on(idx)
      if (!is.na(s$mean) && s$n_eff >= min_n_eff) {
        out_k <- out_k + 1L
        rows[[out_k]] <- data.frame(
          type = "cat×num",
          feat_i = fi, feat_j = fj,
          group_i = gi, group_j = gj,
          level_i = sub(paste0("^", gi), "", fi),
          level_j = NA_character_,
          direction = s$mean,
          n_raw = s$n_raw,
          w_sum = s$w_sum,
          n_eff = s$n_eff,
          stringsAsFactors = FALSE
        )
      }
      next
    }
  }

  if (out_k == 0L) {
    return(data.frame(
      type = character(0),
      feat_i = character(0), feat_j = character(0),
      group_i = character(0), group_j = character(0),
      level_i = character(0), level_j = character(0),
      direction = numeric(0),
      n_raw = integer(0), w_sum = numeric(0), n_eff = numeric(0),
      stringsAsFactors = FALSE
    ))
  }

  df <- do.call(rbind, rows[seq_len(out_k)])

  # canonical pair label for easy grouping
  gi <- df$group_i
  gj <- df$group_j
  df$pair <- ifelse(gi <= gj, paste0(gi, " × ", gj), paste0(gj, " × ", gi))

  df <- df[order(-df$direction), , drop = FALSE]
  df
}
