#' Run the full XGBoost + SHAP workflow (main + interaction)
#'
#' Convenience wrapper that replicates the script outputs as data.frames.
#'
#' @param data Input data.frame.
#' @param interaction_subsample_n Subsample size for interaction SHAP.
#' @param seed Optional integer seed. If provided, controls interaction subsampling and
#'   makes the overall pipeline more reproducible.
#' @param min_n_eff_main Minimum effective sample size filter for one-way direction levels.
#' @param min_n_eff_interaction Minimum effective sample size filter for interaction direction rows.
#' @return A list with fitted model and summary tables.
#' @export
run_survey_shap <- function(data,
                            interaction_subsample_n = 5000L,
                            seed = NULL,
                            min_n_eff_main = 100,
                            min_n_eff_interaction = 100) {

  if (!is.null(seed)) {
    seed <- as.integer(seed)
    set.seed(seed)
  }

  design <- build_xgb_design(data)

  # fit (weighted if available)
  if (is.null(seed)) {
    fit <- fit_survey_xgb(design$X, design$y, w = design$w)
  } else {
    fit <- fit_survey_xgb(design$X, design$y, w = design$w, seed = seed)
  }

  main <- compute_shap_main(fit$model, fit$dall, feature_names = design$feature_names)

  strength_main <- shap_strength_main_group(main$shap_feat, design$feature_names, w = design$w)
  direction_main <- shap_direction_main_active(design$df, main$shap_feat, w = design$w, min_n_eff = min_n_eff_main)

  if (is.null(seed)) {
    inter <- compute_shap_interaction(fit$model, design$X, subsample_n = interaction_subsample_n, w = design$w)
  } else {
    inter <- compute_shap_interaction(fit$model, design$X, subsample_n = interaction_subsample_n, seed = seed, w = design$w)
  }

  strength_int <- shap_strength_interaction_group(inter$shap_int_feat, design$feature_names, w_sub = inter$w_sub)

  direction_int <- shap_direction_interaction_active(
    inter$shap_int_feat,
    inter$X_sub,
    design$feature_names,
    w_sub = inter$w_sub,
    min_n_eff = min_n_eff_interaction
  )

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
