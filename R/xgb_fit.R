#' Fit an XGBoost regression model for survey outcome
#'
#' @param X A sparse matrix (dgCMatrix) of features.
#' @param y Numeric outcome vector.
#' @param params XGBoost parameter list. If NULL, uses defaults from your script.
#' @param nrounds Number of boosting rounds.
#' @param seed Random seed.
#' @param verbose Verbosity passed to xgb.train.
#' @return A list with `model` (xgb.Booster) and `dall` (xgb.DMatrix).
#' @export
fit_survey_xgb <- function(
    X,
    y,
    params = NULL,
    nrounds = 300,
    seed = 123,
    verbose = 0
) {
  if (!inherits(X, "dgCMatrix")) {
    stop("`X` must be a dgCMatrix (sparse matrix).")
  }
  stopifnot(is.numeric(y), length(y) == nrow(X))

  if (is.null(params)) {
    params <- list(
      objective = "reg:squarederror",
      eval_metric = "rmse",
      max_depth = 4,
      eta = 0.05,
      subsample = 0.8,
      colsample_bytree = 0.8,
      tree_method = "hist"
    )
  }

  dall <- xgboost::xgb.DMatrix(data = X, label = y)

  set.seed(seed)
  model <- xgboost::xgb.train(
    params = params,
    data = dall,
    nrounds = nrounds,
    verbose = verbose
  )

  list(model = model, dall = dall)
}
