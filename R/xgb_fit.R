#' Fit an XGBoost regression model for survey outcome
#'
#' @param X A sparse matrix (\code{dgCMatrix}) of features.
#' @param y Numeric outcome vector.
#' @param params XGBoost parameter list. If \code{params$seed} is not provided and
#'   \code{seed} is not \code{NULL}, the function sets \code{params$seed = seed}.
#' @param nrounds Number of boosting rounds.
#' @param seed Optional integer seed used to set \code{params$seed} for reproducible fitting.
#' @param verbose Verbosity passed to \code{xgboost::xgb.train()}.
#' @param nthread Optional integer; number of threads used by XGBoost.
#'   If provided and \code{params$nthread} is not set, this will set it.
#' @return A list with \code{model} (\code{xgb.Booster}) and \code{dall} (\code{xgb.DMatrix}).
#'   For backward/forward compatibility, \code{dtrain} is also returned as an alias of \code{dall}.
#' @export
fit_survey_xgb <- function(X,
                           y,
                           params = list(
                             objective = "reg:squarederror",
                             max_depth = 4,
                             eta = 0.05,
                             subsample = 0.8,
                             colsample_bytree = 0.8
                           ),
                           nrounds = 200,
                           seed = NULL,
                           verbose = 0,
                           nthread = NULL) {

  if (!is.null(seed)) {
    if (is.null(params$seed)) params$seed <- as.integer(seed)
  }
  if (is.null(params$verbosity)) params$verbosity <- as.integer(verbose)

  if (!is.null(nthread)) {
    if (is.null(params$nthread)) params$nthread <- as.integer(nthread)
  }

  dall <- xgboost::xgb.DMatrix(data = X, label = y)

  model <- xgboost::xgb.train(
    params = params,
    data = dall,
    nrounds = nrounds,
    verbose = verbose
  )

  list(
    model = model,
    dall  = dall,
    dtrain = dall
  )
}
