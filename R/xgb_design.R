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
#' Outcome is the numeric sum score \code{0,1,\u2026,6} (default: \code{gun_control}).
#'
#' Categorical predictors are one-hot encoded with identity contrasts (no dropped reference).
#' Education and RUCC are discretized into two binary factors and used in the model:
#' \code{college \in \{Non-college, College\}} and \code{metro \in \{Big-metro, Non-metro\}}.
#'
#' Sample weights (if provided) are passed through and can be normalized to mean 1.
#'
#' @param data A data.frame containing outcome and covariates.
#' @param y_col Outcome column name (default: "gun_control").
#' @param cat_cols Categorical covariates to one-hot encode.
#' @param weight_col Optional weight column name (default: "weight"). If missing, weights are NULL.
#' @param normalize_weights Logical; if TRUE, divide weights by mean(weights).
#'
#' @return A list with `df` (cleaned data), `X` (dgCMatrix), `y` (numeric),
#'   `w` (weights or NULL), and `feature_names` (character).
#' @export
build_xgb_design <- function(
    data,
    y_col = "gun_control",
    cat_cols = c("gun_own", "partisan", "race", "gender", "college", "metro"),
    weight_col = "weight",
    normalize_weights = TRUE
) {
  stopifnot(is.data.frame(data))
  stopifnot(is.character(y_col), length(y_col) == 1)

  # ---------- discretize educ/rucc and use them in the model ----------
  if (!"college" %in% names(data)) {
    if (!"educ" %in% names(data)) stop("Missing column in `data`: educ (needed to build `college`).")
    data$college <- factor(
      ifelse(data$educ %in% c(1, 2, 3), "Non-college",
             ifelse(data$educ %in% c(4, 5, 6), "College", NA)),
      levels = c("Non-college", "College")
    )
  }
  if (!"metro" %in% names(data)) {
    if (!"rucc" %in% names(data)) stop("Missing column in `data`: rucc (needed to build `metro`).")
    data$metro <- factor(
      ifelse(data$rucc == 1, "Big-metro", "Non-metro"),
      levels = c("Big-metro", "Non-metro")
    )
  }

  all_cols <- unique(c(y_col, cat_cols, weight_col))
  all_cols <- all_cols[all_cols %in% names(data)]
  missing_cols <- setdiff(c(y_col, cat_cols), names(data))
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

  # complete cases on outcome + predictors (+ weights if present)
  cc_cols <- c("y_count", cat_cols)
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

  # y as numeric (0..6)
  y <- df$y_count

  # contrasts.arg: identity coding for each factor
  contr_list <- lapply(cat_cols, function(nm) full_dummy_contrasts(df[[nm]]))
  names(contr_list) <- cat_cols

  # build sparse design: include numeric columns directly
  rhs <- c(cat_cols)
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
    y = y,
    w = w,
    feature_names = colnames(X)
  )
}
