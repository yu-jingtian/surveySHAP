#' Run the full XGBoost + SHAP workflow (main + interaction)
#'
#' Convenience wrapper that replicates the script outputs as data.frames.
#'
#' @param data Input data.frame.
#' @param interaction_subsample_n Subsample size for interaction SHAP.
#' @param seed Optional integer seed. If provided, controls interaction subsampling and
#'   makes the overall pipeline more reproducible.
#' @return A list with fitted model and summary tables.
#' @export
run_survey_shap <- function(data,
                            interaction_subsample_n = 5000L,
                            seed = NULL) {

  # Keep current behavior if seed is NULL; otherwise control randomness
  if (!is.null(seed)) {
    seed <- as.integer(seed)
    set.seed(seed)
  }

  design <- build_xgb_design(data)

  # fit_survey_xgb() in your repo already has a seed argument; keep call compatible:
  if (is.null(seed)) {
    fit <- fit_survey_xgb(design$X, design$y)
  } else {
    fit <- fit_survey_xgb(design$X, design$y, seed = seed)
  }

  # IMPORTANT: compute_shap_main signature in your repo is (model, dmat, feature_names=)
  main <- compute_shap_main(fit$model, fit$dall, feature_names = design$feature_names)

  strength_main <- shap_strength_main_group(main$shap_feat, design$feature_names)
  direction_main <- shap_direction_main_active(design$df, main$shap_feat)

  # IMPORTANT: compute_shap_interaction signature in your repo supports seed already
  if (is.null(seed)) {
    inter <- compute_shap_interaction(fit$model, design$X, subsample_n = interaction_subsample_n)
  } else {
    inter <- compute_shap_interaction(fit$model, design$X, subsample_n = interaction_subsample_n, seed = seed)
  }

  strength_int <- shap_strength_interaction_group(inter$shap_int_feat, design$feature_names)

  # IMPORTANT: shap_direction_interaction_active signature in your repo:
  #   (shap_int_feat, X_sub, feature_names, feat_group=NULL)
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
