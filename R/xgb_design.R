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
#' Mirrors the analysis script:
#' - outcome `gun_control` is numeric
#' - covariates are treated as factors
#' - rows with missing values are removed
#' - one-hot encoding uses identity contrasts (no reference level dropped)
#'
#' @param data A data.frame containing outcome and covariates.
#' @param y_col Outcome column name (default: "gun_control").
#' @param x_cols Covariate column names (default: c("gun_own","partisan","race","gender","college")).
#' @return A list with `df` (cleaned data), `X` (dgCMatrix), `y` (numeric),
#'   and `feature_names` (character).
#' @export
build_xgb_design <- function(
    data,
    y_col = "gun_control",
    x_cols = c("gun_own", "partisan", "race", "gender", "college")
) {
  stopifnot(is.data.frame(data))
  stopifnot(is.character(y_col), length(y_col) == 1)
  stopifnot(is.character(x_cols), length(x_cols) >= 1)

  missing_cols <- setdiff(c(y_col, x_cols), names(data))
  if (length(missing_cols) > 0) {
    stop("Missing columns in `data`: ", paste(missing_cols, collapse = ", "))
  }

  # Construct modeling df using base subsetting (package-safe)
  df <- data[, c(y_col, x_cols), drop = FALSE]
  names(df)[1] <- "y"
  df$y <- as.numeric(df$y)

  # factorize covariates
  for (nm in x_cols) {
    df[[nm]] <- as.factor(df[[nm]])
  }

  # complete cases + drop unused levels
  df <- df[stats::complete.cases(df), , drop = FALSE]
  df <- droplevels(df)

  # contrasts.arg: identity coding for each factor
  contr_list <- lapply(x_cols, function(nm) full_dummy_contrasts(df[[nm]]))
  names(contr_list) <- x_cols

  # one-hot sparse design matrix (no intercept, no dropped ref)
  fml <- stats::as.formula(paste("~", paste(x_cols, collapse = " + "), "- 1"))
  X <- Matrix::sparse.model.matrix(
    fml,
    data = df,
    contrasts.arg = contr_list
  )

  y <- df$y
  if (nrow(X) != length(y)) stop("Row mismatch between X and y.")
  if (ncol(X) == 0) stop("Design matrix has 0 columns. Check factor levels / inputs.")

  list(
    df = df,
    X = X,
    y = y,
    feature_names = colnames(X)
  )
}
