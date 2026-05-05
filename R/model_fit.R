#' Fit one of the four supported survey models
#'
#' Supported models are \code{"lm"}, \code{"glm"}, \code{"xgb_numeric"},
#' and \code{"xgb_logistic"}.
#'
#' For \code{lm}/\code{glm}, the fit is performed using regular treatment
#' coding, with the first level of each factor used as the reference level.
#' The returned object stores coefficients mapped back onto the full one-hot
#' main-effect and dummy-by-dummy interaction design. Reference-level terms are
#' assigned coefficient zero.
#'
#' For xgboost models, only the main one-hot feature matrix is used as input,
#' matching the original repo design. Interactions are handled by native
#' TreeSHAP interaction values rather than explicit interaction columns.
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
  w <- design$w

  if (model %in% c("lm", "glm")) {
    X_lin <- design$X_linear
    w_fit <- if (is.null(w)) rep(1, nrow(X_lin)) else w

    if (model == "lm") {
      fit <- stats::lm.wfit(
        x = X_lin,
        y = design$y_count,
        w = w_fit
      )
    } else {
      fit <- stats::glm.fit(
        x = X_lin,
        y = design$y_prop,
        weights = 6 * w_fit,
        family = stats::quasibinomial()
      )
    }

    gamma <- stats::coef(fit)
    gamma[is.na(gamma)] <- 0
    gamma <- setNames(as.numeric(gamma), colnames(X_lin))

    beta_full <- setNames(
      numeric(1 + ncol(design$X_main) + ncol(design$X_int)),
      c("(Intercept)", colnames(design$X_main), colnames(design$X_int))
    )

    common_terms <- intersect(names(beta_full), names(gamma))
    beta_full[common_terms] <- gamma[common_terms]

    return(list(
      model_type = model,
      fit = fit,
      gamma = gamma,
      beta_full = beta_full,
      design = design,
      scale = if (model == "lm") "count" else "logit"
    ))
  }

  if (!requireNamespace("xgboost", quietly = TRUE)) {
    stop("Package `xgboost` is required for xgb models.")
  }

  params <- xgb_params
  if (is.null(params)) {
    params <- if (model == "xgb_numeric") {
      list(
        objective = "reg:squarederror",
        eval_metric = "rmse",
        max_depth = 4,
        eta = 0.05,
        subsample = 0.8,
        colsample_bytree = 0.8
      )
    } else {
      list(
        objective = "reg:logistic",
        eval_metric = "rmse",
        max_depth = 4,
        eta = 0.05,
        subsample = 0.8,
        colsample_bytree = 0.8
      )
    }
  }

  if (!is.null(seed) && is.null(params$seed)) {
    params$seed <- as.integer(seed)
  }

  ## xgboost uses ONLY X_main, matching the original repo.
  dtrain <- xgboost::xgb.DMatrix(
    data = Matrix::Matrix(design$X_xgb, sparse = TRUE),
    label = if (model == "xgb_numeric") design$y_count else design$y_prop,
    weight = w
  )

  booster <- xgboost::xgb.train(
    params = params,
    data = dtrain,
    nrounds = nrounds,
    verbose = verbose
  )

  list(
    model_type = model,
    fit = booster,
    dtrain = dtrain,
    design = design,
    xgb_feature_names = colnames(design$X_xgb)
  )
}
