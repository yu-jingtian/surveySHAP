#' Compute xgboost main SHAP values
#'
#' Uses xgboost native TreeSHAP via \code{predcontrib = TRUE}. For
#' \code{xgb_numeric}, SHAP is returned on the count scale. For
#' \code{xgb_logistic}, SHAP is returned on the logit scale by using
#' \code{outputmargin = TRUE}.
#'
#' @param fit_obj A fitted xgboost object from [fit_survey_model()].
#'
#' @return A list with the SHAP matrix, bias term, and scale label.
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

  list(
    shap = shap,
    bias = shap_full[, ncol(shap_full)],
    scale = if (pred_margin) "logit" else "count"
  )
}

#' Compute xgboost interaction SHAP values
#'
#' Uses xgboost native TreeSHAP interaction values. The returned array contains
#' only feature-by-feature interactions and excludes the bias dimension.
#'
#' xgboost is fit only on the main one-hot feature matrix, so this interaction
#' array is over the original level indicators, matching the original repo logic.
#'
#' @param fit_obj A fitted xgboost object from [fit_survey_model()].
#'
#' @return A list with an interaction array, feature names, and scale label.
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

  arr <- arr[, seq_len(p), seq_len(p), drop = FALSE]
  dimnames(arr) <- list(NULL, fit_obj$xgb_feature_names, fit_obj$xgb_feature_names)

  list(
    interaction = arr,
    feature_names = fit_obj$xgb_feature_names,
    scale = if (pred_margin) "logit" else "count"
  )
}