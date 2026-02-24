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

#' Build the XGBoost modeling frame and sparse design matrix
#'
#' Default behavior for the updated data structure:
#' - outcome `gun_control` is a 0--6 count (sum of 6 binary items)
#' - for XGBoost fitting, we model the implied proportion `gun_control / trials`
#'   (default `trials = 6`) with a logistic link (`reg:logistic`)
#' - covariates are treated as factors *except* `educ` and `rucc`, which are
#'   ordinal and treated as numeric by default
#' - rows with missing values are removed
#' - one-hot encoding uses identity contrasts (no reference level dropped)
#' - optional survey weights are carried through for weighted training and
#'   weighted SHAP aggregation
#'
#' @param data A data.frame containing outcome and covariates.
#' @param y_col Outcome column name (default: "gun_control").
#' @param x_cols Covariate column names.
#'   Default: c("gun_own","partisan","race","gender","educ","rucc").
#' @param weight_col Optional weight column name (default: "weight"). If NULL or
#'   not present, weights are not used.
#' @param outcome One of c("binom_prop","count"). If "binom_prop" (default),
#'   the returned `y` is `gun_control / trials`. If "count", the returned `y` is
#'   the raw count (0--trials).
#' @param trials Integer number of trials for the summed index (default: 6).
#' @param normalize_weights Logical; if TRUE (default) and weights are present,
#'   weights are rescaled to have mean 1 (preserving relative weights).
#' @return A list with:
#'   - `df`: cleaned data.frame (includes covariates, outcome, and weights if present)
#'   - `X`: dgCMatrix design matrix
#'   - `y`: training label (depends on `outcome`)
#'   - `y_count`: raw count outcome (0--trials)
#'   - `trials`: integer trials
#'   - `w`: normalized weights (or NULL)
#'   - `feature_names`: character column names for `X`
#' @export
build_xgb_design <- function(
    data,
    y_col = "gun_control",
    x_cols = c("gun_own", "partisan", "race", "gender", "educ", "rucc"),
    weight_col = "weight",
    outcome = c("binom_prop", "count"),
    trials = 6L,
    normalize_weights = TRUE
) {
  stopifnot(is.data.frame(data))
  stopifnot(is.character(y_col), length(y_col) == 1)
  stopifnot(is.character(x_cols), length(x_cols) >= 1)
  outcome <- match.arg(outcome)
  trials <- as.integer(trials)
  if (trials <= 0L) stop("`trials` must be a positive integer.")

  needed <- c(y_col, x_cols)
  if (!is.null(weight_col)) needed <- c(needed, weight_col)
  missing_cols <- setdiff(needed, names(data))
  if (length(missing_cols) > 0) {
    stop("Missing columns in `data`: ", paste(missing_cols, collapse = ", "))
  }

  # Construct modeling df using base subsetting (package-safe)
  keep <- c(y_col, x_cols, if (!is.null(weight_col)) weight_col)
  df <- data[, keep, drop = FALSE]
  names(df)[names(df) == y_col] <- "y_count"

  # outcome as numeric count
  df$y_count <- as.numeric(df$y_count)

  # By default, treat ordinal covariates as numeric so the model can exploit ordering.
  # Categorical covariates are one-hot encoded with full (identity) contrasts.
  ordinal_numeric <- intersect(x_cols, c("educ", "rucc"))
  factor_cols <- setdiff(x_cols, ordinal_numeric)

  for (nm in factor_cols) {
    df[[nm]] <- as.factor(df[[nm]])
  }
  for (nm in ordinal_numeric) {
    df[[nm]] <- as.numeric(df[[nm]])
  }

  # weights (optional)
  w <- NULL
  if (!is.null(weight_col)) {
    w <- as.numeric(df[[weight_col]])
  }

  # complete cases + drop unused levels
  df <- df[stats::complete.cases(df), , drop = FALSE]
  if (length(factor_cols) > 0) {
    df[factor_cols] <- lapply(df[factor_cols], droplevels)
  }

  if (!is.null(weight_col)) {
    w <- as.numeric(df[[weight_col]])
    if (any(!is.finite(w)) || any(w < 0)) stop("`weight_col` must be finite and nonnegative.")
    if (isTRUE(normalize_weights)) {
      m <- mean(w)
      if (!is.finite(m) || m <= 0) stop("Invalid weights: mean(weight) must be > 0.")
      w <- w / m
    }
  }

  # contrasts.arg: identity coding for each factor (numeric cols ignored)
  contr_list <- NULL
  if (length(factor_cols) > 0) {
    contr_list <- lapply(factor_cols, function(nm) full_dummy_contrasts(df[[nm]]))
    names(contr_list) <- factor_cols
  }

  # one-hot sparse design matrix (no intercept, no dropped ref)
  fml <- stats::as.formula(paste("~", paste(x_cols, collapse = " + "), "- 1"))
  X <- Matrix::sparse.model.matrix(
    fml,
    data = df,
    contrasts.arg = contr_list
  )

  y_count <- df$y_count
  y <- if (outcome == "binom_prop") y_count / trials else y_count

  if (nrow(X) != length(y)) stop("Row mismatch between X and y.")
  if (ncol(X) == 0) stop("Design matrix has 0 columns. Check factor levels / inputs.")

  list(
    df = df,
    X = X,
    y = y,
    y_count = y_count,
    trials = trials,
    w = w,
    feature_names = colnames(X),
    outcome = outcome
  )
}
