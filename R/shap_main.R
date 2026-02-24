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

#' Map feature names to variable group
#'
#' Handles both one-hot features (prefix match) and numeric ordinal features (exact match).
#'
#' @param feature_names Character vector of feature names (design-matrix columns).
#' @param groups Named character vector mapping regex/prefix to group name.
#' @return Character vector of group labels (same length as feature_names).
#' @export
feature_to_group <- function(
    feature_names,
    groups = c(
      gun_own  = "^gun_own",
      partisan = "^partisan",
      race     = "^race",
      gender   = "^gender",
      educ     = "^educ$",
      rucc     = "^rucc$"
    )
) {
  out <- rep(NA_character_, length(feature_names))
  for (g in names(groups)) {
    hit <- grepl(groups[[g]], feature_names)
    out[hit] <- g
  }
  out
}

#' Main SHAP strength by group (weighted)
#'
#' Computes weighted mean(|SHAP|) per feature and aggregates to group by summing.
#'
#' @param shap_feat n x p SHAP matrix (excluding bias).
#' @param feature_names Column names for shap_feat.
#' @param feat_group Group labels for each feature; if NULL computed by `feature_to_group()`.
#' @param w Optional sample weights (length n). If NULL, unweighted means are used.
#' @return A data.frame with columns `group` and `strength`, sorted decreasing.
#' @export
shap_strength_main_group <- function(
    shap_feat,
    feature_names = colnames(shap_feat),
    feat_group = NULL,
    w = NULL
) {
  stopifnot(is.matrix(shap_feat))
  if (is.null(feature_names) || length(feature_names) != ncol(shap_feat)) {
    stop("Provide `feature_names` matching columns of `shap_feat`.")
  }
  if (is.null(feat_group)) {
    feat_group <- feature_to_group(feature_names)
  }

  if (is.null(w)) {
    mean_abs <- colMeans(abs(shap_feat))
  } else {
    stopifnot(length(w) == nrow(shap_feat))
    ww <- as.numeric(w)
    ok_w <- !is.na(ww) & ww > 0
    if (!any(ok_w)) stop("All weights are NA/non-positive.")
    # weighted mean per column
    mean_abs <- vapply(seq_len(ncol(shap_feat)), function(j) .wmean(abs(shap_feat[, j]), ww), numeric(1))
  }

  strength <- tapply(mean_abs, feat_group, sum, na.rm = TRUE)

  df <- data.frame(
    group = names(strength),
    strength = as.numeric(strength),
    stringsAsFactors = FALSE
  )
  df[order(-df$strength), , drop = FALSE]
}

#' Active SHAP direction table for a categorical variable (weighted + ESS filter)
#'
#' For each observation, selects the SHAP value of the one-hot column that matches
#' its realized factor level, then computes a weighted mean within each level.
#'
#' @param group_prefix Prefix for one-hot columns (e.g. "partisan").
#' @param group_factor Factor vector (length n).
#' @param shap_feat_mat n x p SHAP matrix (excluding bias), columns are features.
#' @param w Optional sample weights (length n). If NULL, unweighted means are used.
#' @param min_n_eff Minimum effective sample size for keeping a level (default 100).
#' @return A data.frame with columns `feature`, `level`, `shap_active`, `n_raw`, `w_sum`, `n_eff`, `onehot`.
#' @export
active_shap_by_group <- function(
    group_prefix,
    group_factor,
    shap_feat_mat,
    w = NULL,
    min_n_eff = 100
) {
  stopifnot(is.factor(group_factor))
  stopifnot(is.matrix(shap_feat_mat), nrow(shap_feat_mat) == length(group_factor))
  min_n_eff <- as.numeric(min_n_eff)

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
  keep <- !is.na(df$shap_active)
  df <- df[keep, , drop = FALSE]

  if (is.null(w)) {
    df$w <- 1
  } else {
    stopifnot(length(w) == length(group_factor))
    df$w <- as.numeric(w)[keep]
  }

  # per-level summaries
  split_idx <- split(seq_len(nrow(df)), df$level)

  res_list <- lapply(names(split_idx), function(lv) {
    ii <- split_idx[[lv]]
    ww <- df$w[ii]
    xx <- df$shap_active[ii]

    n_raw <- length(ii)
    w_sum <- sum(ww, na.rm = TRUE)
    n_eff <- .n_eff(ww)

    data.frame(
      feature = group_prefix,
      level = lv,
      shap_active = if (is.null(w)) mean(xx) else .wmean(xx, ww),
      n_raw = n_raw,
      w_sum = w_sum,
      n_eff = n_eff,
      onehot = paste0(group_prefix, lv),
      stringsAsFactors = FALSE
    )
  })

  res <- do.call(rbind, res_list)

  # filter by ESS
  res <- res[!is.na(res$n_eff) & res$n_eff >= min_n_eff, , drop = FALSE]
  res
}

#' Main SHAP direction across all variables (active SHAP; weighted)
#'
#' For categorical variables, returns level-wise active SHAP summaries.
#' For numeric variables (e.g., educ, rucc), returns a single row with level=NA and shap_active=NA.
#'
#' @param df Cleaned modeling dataframe returned by `build_xgb_design()` (contains factor cols + numeric cols).
#' @param shap_feat n x p SHAP matrix excluding bias.
#' @param w Optional sample weights (length n).
#' @param cat_cols Categorical variables to compute active SHAP (default: gun_own, partisan, race, gender).
#' @param num_cols Numeric variables to include with NA direction (default: educ, rucc).
#' @param min_n_eff Minimum effective sample size filter for categorical levels.
#' @return A data.frame stacking all variables' direction summaries.
#' @export
shap_direction_main_active <- function(
    df,
    shap_feat,
    w = NULL,
    cat_cols = c("gun_own", "partisan", "race", "gender"),
    num_cols = c("educ", "rucc"),
    min_n_eff = 100
) {
  stopifnot(is.data.frame(df))
  stopifnot(is.matrix(shap_feat), nrow(shap_feat) == nrow(df))
  if (!is.null(w)) stopifnot(length(w) == nrow(df))

  out_cat <- do.call(
    rbind,
    lapply(cat_cols, function(nm) {
      if (!nm %in% names(df)) return(NULL)
      active_shap_by_group(nm, as.factor(df[[nm]]), shap_feat, w = w, min_n_eff = min_n_eff)
    })
  )

  # numeric vars: include placeholder rows (direction not level-defined)
  out_num <- do.call(
    rbind,
    lapply(num_cols, function(nm) {
      if (!nm %in% names(df)) return(NULL)
      data.frame(
        feature = nm,
        level = NA_character_,
        shap_active = NA_real_,
        n_raw = nrow(df),
        w_sum = if (is.null(w)) nrow(df) else sum(w, na.rm = TRUE),
        n_eff = if (is.null(w)) nrow(df) else .n_eff(w),
        onehot = NA_character_,
        stringsAsFactors = FALSE
      )
    })
  )

  out <- rbind(out_cat, out_num)
  rownames(out) <- NULL
  out
}
