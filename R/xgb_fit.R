#' Fit an XGBoost model for the survey outcome
#'
#' Supports optional sample weights (e.g., survey weights).
#'
#' For the updated data structure, the recommended label is the implied
#' proportion `gun_control / trials` with a logistic objective ("reg:logistic").
#'
#' @param X A sparse matrix (\code{dgCMatrix}) of features.
#' @param y Numeric outcome vector used for training (e.g., proportion in [0,1]).
#' @param w Optional numeric vector of sample weights (length n). If provided,
#'   passed to \code{xgboost::xgb.DMatrix(weight = ...)}.
#' @param params XGBoost parameter list. If \code{params$seed} is not provided and
#'   \code{seed} is not \code{NULL}, the function sets \code{params$seed = seed}.
#' @param nrounds Number of boosting rounds.
#' @param seed Optional integer seed used to set \code{params$seed} for reproducible fitting.
#' @param verbose Verbosity passed to \code{xgboost::xgb.train()}.
#' @return A list with \code{model} (\code{xgb.Booster}) and \code{dall} (\code{xgb.DMatrix}).
#'   For backward/forward compatibility, \code{dtrain} is also returned as an alias of \code{dall}.
#' @export
fit_survey_xgb <- function(X,
                           y,
                           w = NULL,
                           params = list(
                             objective = "reg:logistic",
                             eval_metric = "logloss",
                             max_depth = 4,
                             eta = 0.05,
                             subsample = 0.8,
                             colsample_bytree = 0.8
                           ),
                           nrounds = 200,
                           seed = NULL,
                           verbose = 0) {

  if (!is.null(seed)) {
    if (is.null(params$seed)) params$seed <- as.integer(seed)
  }
  if (is.null(params$verbosity)) params$verbosity <- as.integer(verbose)

  if (!is.null(w)) {
    w <- as.numeric(w)
    if (length(w) != nrow(X)) stop("`w` must have length nrow(X).")
    if (any(!is.finite(w)) || any(w < 0)) stop("`w` must be finite and nonnegative.")
  }

  dall <- xgboost::xgb.DMatrix(data = X, label = y, weight = w)

  model <- xgboost::xgb.train(
    params = params,
    data = dall,
    nrounds = nrounds,
    verbose = verbose
  )

  # Return BOTH names to avoid breaking any existing code
  list(
    model = model,
    dall  = dall,
    dtrain = dall
  )
}
