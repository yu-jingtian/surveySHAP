#' Run the full XGBoost + SHAP workflow (main + interaction)
#'
#' This is a convenience wrapper that replicates the script outputs as data.frames.
#'
#' @param data Input data.frame.
#' @param interaction_subsample_n Subsample size for interaction SHAP.
#' @return A list with fitted model and summary tables.
#' @export
run_survey_shap <- function(data, interaction_subsample_n = 5000L) {
  design <- build_xgb_design(data)
  fit <- fit_survey_xgb(design$X, design$y)

  main <- compute_shap_main(fit$model, fit$dall, feature_names = design$feature_names)
  strength_main <- shap_strength_main_group(main$shap_feat, design$feature_names)
  direction_main <- shap_direction_main_active(design$df, main$shap_feat)

  inter <- compute_shap_interaction(fit$model, design$X, subsample_n = interaction_subsample_n)
  strength_int <- shap_strength_interaction_group(inter$shap_int_feat, design$feature_names)
  direction_int <- shap_direction_interaction_active(inter$shap_int_feat, inter$X_sub, design$feature_names)

  list(
    model = fit$model,
    design = design,
    shap_main = main,
    strength_main_group = strength_main,
    direction_main_active = direction_main,
    shap_interaction = inter,
    strength_interaction_group = strength_int,
    direction_interaction_active = direction_int
  )
}
