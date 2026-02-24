#' Compute main-effect SHAP (TreeSHAP) for an xgboost model
#'
#' Uses xgboost native TreeSHAP via `predcontrib = TRUE`.
#'
#' @param model An xgb.Booster.
#' @param dmat An xgb.DMatrix.
#' @param feature_names Optional feature names for columns (excluding bias).
#' @return A list with `shap_feat` (n x p matrix), `bias` (length n),
#'   and `shap_full` (n x (p+1) matrix including bias column).
#' @export
compute_shap_main <- function(model, dmat, feature_names = NULL) {
  shap_full <- stats::predict(model, dmat, predcontrib = TRUE)
  shap_full <- as.matrix(shap_full)

  bias <- shap_full[, ncol(shap_full)]
  shap_feat <- shap_full[, -ncol(shap_full), drop = FALSE]

  if (!is.null(feature_names)) {
    stopifnot(length(feature_names) == ncol(shap_feat))
    colnames(shap_feat) <- feature_names
  }

  list(shap_feat = shap_feat, bias = bias, shap_full = shap_full)
}

#' Map one-hot feature names to variable group
#'
#' @param feature_names Character vector of one-hot feature names.
#' @param groups Named character vector mapping regex/prefix to group name.
#'   Default matches your script: gun_own, partisan, race, gender, college.
#' @return Character vector of group labels (same length as feature_names).
#' @export
feature_to_group <- function(
    feature_names,
    groups = c(
      gun_own  = "^gun_own",
      partisan = "^partisan",
      race     = "^race",
      gender   = "^gender",
      educ     = "^educ",
      rucc     = "^rucc"
    )
) {
  out <- rep(NA_character_, length(feature_names))
  for (g in names(groups)) {
    hit <- grepl(groups[[g]], feature_names)
    out[hit] <- g
  }
  out
}

#' Main SHAP strength by group
#'
#' Computes mean(|SHAP|) per one-hot feature and aggregates to group by summing.
#'
#' @param shap_feat n x p SHAP matrix (excluding bias).
#' @param feature_names Column names for shap_feat.
#' @param feat_group Group labels for each feature; if NULL computed by `feature_to_group()`.
#' @return A data.frame with columns `group` and `strength`, sorted decreasing.
#' @export
shap_strength_main_group <- function(shap_feat,
                                    feature_names = colnames(shap_feat),
                                    feat_group = NULL,
                                    w = NULL) {
  stopifnot(is.matrix(shap_feat))
  if (is.null(feature_names) || length(feature_names) != ncol(shap_feat)) {
    stop("Provide `feature_names` matching columns of `shap_feat`.")
  }
  if (is.null(feat_group)) {
    feat_group <- feature_to_group(feature_names)
  }

  if (!is.null(w)) {
    w <- as.numeric(w)
    if (length(w) != nrow(shap_feat)) stop("`w` must have length nrow(shap_feat).")
    if (any(!is.finite(w)) || any(w < 0)) stop("`w` must be finite and nonnegative.")
    wsum <- sum(w)
    if (!is.finite(wsum) || wsum <= 0) stop("Invalid weights: sum(w) must be > 0.")
    mean_abs <- colSums(abs(shap_feat) * w) / wsum
  } else {
    mean_abs <- colMeans(abs(shap_feat))
  }

  strength <- tapply(mean_abs, feat_group, sum, na.rm = TRUE)

  df <- data.frame(
    group = names(strength),
    strength = as.numeric(strength),
    stringsAsFactors = FALSE
  )
  df[order(-df$strength), , drop = FALSE]
}


#' Active SHAP direction table for a categorical variable
#'
#' For each observation, selects the SHAP value of the one-hot column that matches
#' its realized factor level, then averages within each level.
#'
#' @param group_prefix Prefix/regex for one-hot columns (e.g. "partisan").
#' @param group_factor Factor vector (length n).
#' @param shap_feat_mat n x p SHAP matrix (excluding bias), columns are one-hot features.
#' @return A data.frame with columns `level`, `shap_active`, `n`, `onehot`.
#' @export
active_shap_by_group <- function(group_prefix,
                                 group_factor,
                                 shap_feat_mat,
                                 w = NULL) {
  stopifnot(is.factor(group_factor))
  stopifnot(is.matrix(shap_feat_mat), nrow(shap_feat_mat) == length(group_factor))

  if (!is.null(w)) {
    w <- as.numeric(w)
    if (length(w) != length(group_factor)) stop("`w` must have length n.")
    if (any(!is.finite(w)) || any(w < 0)) stop("`w` must be finite and nonnegative.")
  }

  cols <- grep(paste0("^", group_prefix), colnames(shap_feat_mat), value = TRUE)
  if (length(cols) < 2) {
    stop("Found <2 one-hot columns for group_prefix=", group_prefix)
  }

  lev_chr <- as.character(group_factor)
  col_for_row <- paste0(group_prefix, lev_chr)

  idx <- match(col_for_row, colnames(shap_feat_mat))
  out <- rep(NA_real_, length(lev_chr))
  ok <- !is.na(idx)
  out[ok] <- shap_feat_mat[cbind(which(ok), idx[ok])]

  df <- data.frame(level = lev_chr, shap_active = out, stringsAsFactors = FALSE)
  if (!is.null(w)) df$w <- w
  df <- df[!is.na(df$shap_active), , drop = FALSE]

  if (is.null(w)) {
    agg_mean <- stats::aggregate(shap_active ~ level, data = df, FUN = mean)
    agg_n <- stats::aggregate(shap_active ~ level, data = df, FUN = length)
    names(agg_n)[2] <- "n"
    res <- merge(agg_mean, agg_n, by = "level")
  } else {
    # weighted mean and weight-sum (effective population mass)
    wsum_by <- tapply(df$w, df$level, sum)
    wmean_by <- tapply(df$shap_active * df$w, df$level, sum) / wsum_by

    res <- data.frame(
      level = names(wmean_by),
      shap_active = as.numeric(wmean_by),
      n = as.numeric(wsum_by),
      stringsAsFactors = FALSE
    )
  }

  res$onehot <- paste0(group_prefix, res$level)
  res
}


#' Main SHAP direction across all variables (active SHAP)
#'
#' @param df Cleaned modeling dataframe returned by `build_xgb_design()` (contains factor cols).
#' @param shap_feat n x p SHAP matrix excluding bias.
#' @param x_cols Covariate column names used in design.
#' @return A data.frame stacking all variables' active SHAP direction summaries.
#' @export
shap_direction_main_active <- function(df,
                                     shap_feat,
                                     x_cols = c("gun_own", "partisan", "race", "gender", "educ", "rucc"),
                                     w = NULL) {
  stopifnot(is.data.frame(df))
  out <- do.call(
    rbind,
    lapply(x_cols, function(nm) active_shap_by_group(nm, df[[nm]], shap_feat, w = w))
  )
  out
}
