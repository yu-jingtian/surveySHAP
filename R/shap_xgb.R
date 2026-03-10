#' Compute xgboost main SHAP values
#'
#' Uses xgboost native TreeSHAP. For logistic xgboost, SHAP values are
#' requested on the raw margin scale so that summaries are on the logit scale.
#'
#' @param fit_obj A fitted object from [fit_survey_model()] with model equal
#'   to \code{"xgb_numeric"} or \code{"xgb_logistic"}.
#'
#' @return A list with the main-effect SHAP matrix and the bias term.
#' @export
compute_shap_xgb_main <- function(fit_obj) {
  stopifnot(fit_obj$model_type %in% c("xgb_numeric", "xgb_logistic"))
  pred_margin <- fit_obj$model_type == "xgb_logistic"
  shap_full <- stats::predict(
    fit_obj$fit,
    fit_obj$dtrain,
    predcontrib = TRUE,
    outputmargin = pred_margin
  )
  shap_full <- as.matrix(shap_full)
  shap <- shap_full[, -ncol(shap_full), drop = FALSE]
  colnames(shap) <- fit_obj$xgb_feature_names
  list(shap = shap, bias = shap_full[, ncol(shap_full)], scale = if (pred_margin) "logit" else "count")
}

#' Compute xgboost interaction SHAP values
#'
#' Uses xgboost native interaction SHAP. The returned object keeps only
#' feature-by-feature interactions and excludes the bias dimension.
#'
#' @param fit_obj A fitted xgboost object from [fit_survey_model()].
#'
#' @return A list with an interaction array and feature names.
#' @export
compute_shap_xgb_interaction <- function(fit_obj) {
  stopifnot(fit_obj$model_type %in% c("xgb_numeric", "xgb_logistic"))
  pred_margin <- fit_obj$model_type == "xgb_logistic"
  arr <- stats::predict(
    fit_obj$fit,
    fit_obj$dtrain,
    predinteraction = TRUE,
    outputmargin = pred_margin
  )
  p1 <- dim(arr)[2]
  p <- p1 - 1L
  arr <- arr[, 1:p, 1:p, drop = FALSE]
  dimnames(arr) <- list(NULL, fit_obj$xgb_feature_names, fit_obj$xgb_feature_names)
  list(interaction = arr, feature_names = fit_obj$xgb_feature_names,
       scale = if (pred_margin) "logit" else "count")
}
