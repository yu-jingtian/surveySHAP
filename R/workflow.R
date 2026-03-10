#' Run the full surveySHAP pipeline
#'
#' Fits one of the four supported models, computes SHAP values, summarizes
#' one-way and two-way strength/direction, and optionally writes four CSV files.
#'
#' @param data Input data.frame.
#' @param model One of \code{"lm"}, \code{"glm"}, \code{"xgb_numeric"}, or
#'   \code{"xgb_logistic"}.
#' @param output_dir Optional directory for writing the four CSV outputs.
#' @param seed Optional integer seed.
#' @param min_n_eff_main Minimum effective sample size for main-direction rows.
#' @param min_n_eff_interaction Minimum effective sample size for interaction-direction rows.
#' @param xgb_params Optional xgboost parameter list.
#' @param nrounds Number of boosting rounds for xgboost models.
#' @param verbose xgboost verbosity.
#'
#' @return A list containing the fitted model, SHAP objects, and four summary tables.
#' @export
run_survey_shap <- function(data,
                            model = c("lm", "glm", "xgb_numeric", "xgb_logistic"),
                            output_dir = NULL,
                            seed = NULL,
                            min_n_eff_main = 100,
                            min_n_eff_interaction = 100,
                            xgb_params = NULL,
                            nrounds = 200,
                            verbose = 0) {
  model <- match.arg(model)
  if (!is.null(seed)) set.seed(as.integer(seed))

  design <- build_survey_design(data)
  fit_obj <- fit_survey_model(
    design = design,
    model = model,
    xgb_params = xgb_params,
    nrounds = nrounds,
    seed = seed,
    verbose = verbose
  )

  if (model %in% c("lm", "glm")) {
    shap_obj <- compute_shap_linear(fit_obj)
    attr(shap_obj$shap_main, "X_main") <- design$X_main
    main_sum <- summarize_main_shap(shap_obj$shap_main, design$main_map, w = design$w, min_n_eff = min_n_eff_main)
    int_sum <- summarize_interaction_shap(shap_obj$shap_interaction, design, model = model, w = design$w, min_n_eff = min_n_eff_interaction)
  } else {
    shap_main <- compute_shap_xgb_main(fit_obj)
    attr(shap_main$shap, "X_main") <- design$X_main
    shap_int <- compute_shap_xgb_interaction(fit_obj)
    main_sum <- summarize_main_shap(shap_main$shap[, seq_len(ncol(design$X_main)), drop = FALSE], design$main_map, w = design$w, min_n_eff = min_n_eff_main)
    int_sum <- summarize_interaction_shap(shap_int$interaction, design, model = model, w = design$w, min_n_eff = min_n_eff_interaction)
    shap_obj <- list(main = shap_main, interaction = shap_int)
  }

  res <- list(
    model = model,
    fit = fit_obj,
    design = design,
    shap = shap_obj,
    main_strength = main_sum$strength,
    main_direction = main_sum$direction,
    interaction_strength = int_sum$strength,
    interaction_direction = int_sum$direction
  )

  if (!is.null(output_dir)) {
    dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
    utils::write.csv(res$main_strength, file.path(output_dir, "main_strength.csv"), row.names = FALSE)
    utils::write.csv(res$main_direction, file.path(output_dir, "main_direction.csv"), row.names = FALSE)
    utils::write.csv(res$interaction_strength, file.path(output_dir, "interaction_strength.csv"), row.names = FALSE)
    utils::write.csv(res$interaction_direction, file.path(output_dir, "interaction_direction.csv"), row.names = FALSE)
  }

  res
}
