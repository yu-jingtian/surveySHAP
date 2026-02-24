#' Full dummy contrasts for factor (identity coding)
#'
#' Creates K x K identity contrasts so that each factor level gets its own
#' one-hot column (no dropped reference).
#'
#' @param f A factor.
#' @return A contrast matrix suitable for `contrasts<-`.
#' @keywords internal
full_dummy_contrasts <- function(f) {
  stats::contrasts(f, contrasts = FALSE)
}

#' Build the XGBoost modeling frame and sparse design matrix (binomial-style)
#'
#' Outcome is a sum score 0--K (default K=6). We model the implied proportion
#' y/K with a logistic objective; SHAP values are additive on the logit scale.
#'
#' Categorical predictors are one-hot encoded with identity contrasts (no dropped reference).
#' Ordinal predictors (educ, rucc) are treated as numeric columns in the design matrix.
#'
#' Sample weights (if provided) are passed through and can be normalized to mean 1.
#'
#' @param data A data.frame containing outcome and covariates.
#' @param y_col Outcome column name (default: "gun_control").
#' @param trials Integer number of trials K (default 6).
#' @param cat_cols Categorical covariates to one-hot encode.
#' @param num_cols Numeric covariates to keep as numeric columns.
#' @param weight_col Optional weight column name (default: "weight"). If missing, weights are NULL.
#' @param normalize_weights Logical; if TRUE, divide weights by mean(weights).
#'
#' @return A list with `df` (cleaned data), `X` (dgCMatrix), `y_count`, `y` (proportion),
#'   `w` (weights or NULL), and `feature_names` (character).
#' @export
build_xgb_design <- function(
    data,
    y_col = "gun_control",
    trials = 6L,
    cat_cols = c("gun_own", "partisan", "race", "gender"),
    num_cols = c("educ", "rucc"),
    weight_col = "weight",
    normalize_weights = TRUE
) {
  stopifnot(is.data.frame(data))
  stopifnot(is.character(y_col), length(y_col) == 1)
  trials <- as.integer(trials)
  if (trials <= 0L) stop("`trials` must be positive.")

  all_cols <- unique(c(y_col, cat_cols, num_cols, weight_col))
  all_cols <- all_cols[all_cols %in% names(data)]
  missing_cols <- setdiff(c(y_col, cat_cols, num_cols), names(data))
  if (length(missing_cols) > 0) {
    stop("Missing columns in `data`: ", paste(missing_cols, collapse = ", "))
  }

  df <- data[, all_cols, drop = FALSE]
  names(df)[names(df) == y_col] <- "y_count"
  df$y_count <- as.numeric(df$y_count)

  # weights (optional)
  w <- NULL
  if (!is.null(weight_col) && weight_col %in% names(df)) {
    w <- as.numeric(df[[weight_col]])
  }

  # factorize categorical covariates
  for (nm in cat_cols) {
    df[[nm]] <- as.factor(df[[nm]])
  }

  # numeric covariates (leave numeric)
  for (nm in num_cols) {
    df[[nm]] <- as.numeric(df[[nm]])
  }

  # complete cases on outcome + predictors (+ weights if present)
  cc_cols <- c("y_count", cat_cols, num_cols)
  if (!is.null(w)) cc_cols <- c(cc_cols, weight_col)
  df <- df[stats::complete.cases(df[, cc_cols, drop = FALSE]), , drop = FALSE]
  df <- droplevels(df)

  # align weights after filtering
  if (!is.null(w)) {
    w <- as.numeric(df[[weight_col]])
    # drop non-positive weights
    ok_w <- !is.na(w) & w > 0
    df <- df[ok_w, , drop = FALSE]
    w <- w[ok_w]
    if (normalize_weights) {
      w <- w / mean(w)
    }
  }

  # y as proportion in [0,1]
  y_count <- df$y_count
  y <- y_count / trials

  # contrasts.arg: identity coding for each factor
  contr_list <- lapply(cat_cols, function(nm) full_dummy_contrasts(df[[nm]]))
  names(contr_list) <- cat_cols

  # build sparse design: include numeric columns directly
  rhs <- c(cat_cols, num_cols)
  fml <- stats::as.formula(paste("~", paste(rhs, collapse = " + "), "- 1"))
  X <- Matrix::sparse.model.matrix(
    fml,
    data = df,
    contrasts.arg = contr_list
  )

  if (nrow(X) != length(y)) stop("Row mismatch between X and y.")
  if (ncol(X) == 0) stop("Design matrix has 0 columns. Check factor levels / inputs.")

  list(
    df = df,
    X = X,
    y_count = y_count,
    y = y,
    w = w,
    trials = trials,
    feature_names = colnames(X)
  )
}
