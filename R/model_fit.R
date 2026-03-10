#' Fit one of the four supported survey models
#'
#' Supported models are \code{"lm"}, \code{"glm"}, \code{"xgb_numeric"},
#' and \code{"xgb_logistic"}. Linear-model fits include all main effects and
#' all two-way interactions between different variable groups.
#'
#' @param design A design object from [build_survey_design()].
#' @param model One of \code{"lm"}, \code{"glm"}, \code{"xgb_numeric"}, or
#'   \code{"xgb_logistic"}.
#' @param xgb_params Optional list of xgboost parameters.
#' @param nrounds Number of xgboost boosting rounds.
#' @param seed Optional integer seed.
#' @param verbose xgboost verbosity.
#'
#' @return A fitted-model object used by downstream SHAP functions.
#' @export
fit_survey_model <- function(design,
                             model = c("lm", "glm", "xgb_numeric", "xgb_logistic"),
                             xgb_params = NULL,
                             nrounds = 200,
                             seed = NULL,
                             verbose = 0) {
  model <- match.arg(model)
  X <- design$X
  w <- design$w

  if (model == "lm") {
    fit <- stats::lm.wfit(x = X, y = design$y_count, w = if (is.null(w)) rep(1, nrow(X)) else w)
    fit$coefficients[is.na(fit$coefficients)] <- 0
    return(list(model_type = model, fit = fit, design = design))
  }

  if (model == "glm") {
    fit <- stats::glm.fit(
      x = X,
      y = design$y_prop,
      weights = if (is.null(w)) rep(6, nrow(X)) else 6 * w,
      family = stats::quasibinomial()
    )
    fit$coefficients[is.na(fit$coefficients)] <- 0
    return(list(model_type = model, fit = fit, design = design))
  }

  if (!requireNamespace("xgboost", quietly = TRUE)) {
    stop("Package `xgboost` is required for xgb models.")
  }
  params <- xgb_params
  if (is.null(params)) {
    params <- if (model == "xgb_numeric") {
      list(objective = "reg:squarederror", eval_metric = "rmse", max_depth = 4,
           eta = 0.05, subsample = 0.8, colsample_bytree = 0.8)
    } else {
      list(objective = "reg:logistic", eval_metric = "rmse", max_depth = 4,
           eta = 0.05, subsample = 0.8, colsample_bytree = 0.8)
    }
  }
  if (!is.null(seed) && is.null(params$seed)) params$seed <- as.integer(seed)
  dtrain <- xgboost::xgb.DMatrix(
    data = Matrix::Matrix(cbind(design$X_main, design$X_int), sparse = TRUE),
    label = if (model == "xgb_numeric") design$y_count else design$y_prop,
    weight = w
  )
  booster <- xgboost::xgb.train(params = params, data = dtrain, nrounds = nrounds, verbose = verbose)
  list(
    model_type = model,
    fit = booster,
    dtrain = dtrain,
    design = design,
    xgb_feature_names = c(colnames(design$X_main), colnames(design$X_int))
  )
}
