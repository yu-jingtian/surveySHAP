#' Run the full surveySHAP pipeline
#'
#' Fits one of the four supported models, computes SHAP values, summarizes
#' one-way and two-way direction/strength, and optionally writes four CSV files:
#' main strength, main direction, interaction strength, and interaction direction.
#'
#' @param data A data.frame containing the survey data.
#' @param model One of \code{"lm"}, \code{"glm"}, \code{"xgb_numeric"},
#'   or \code{"xgb_logistic"}.
#' @param output_dir Optional output directory for CSV files.
#' @param y_col Outcome column name.
#' @param weight_col Weight column name.
#' @param group_vars Group variables used in the model.
#' @param normalize_weights Logical; normalize weights to mean 1.
#' @param xgb_params Optional xgboost parameter list.
#' @param nrounds Number of xgboost boosting rounds.
#' @param seed Optional integer seed.
#' @param verbose xgboost verbosity.
#' @param min_n_eff Minimum effective sample size for direction outputs.
#'
#' @return A list containing fitted model, SHAP objects, and summary tables.
#' @export
run_survey_shap <- function(data,
                            model = c("lm", "glm", "xgb_numeric", "xgb_logistic"),
                            output_dir = NULL,
                            y_col = "gun_control",
                            weight_col = "weight",
                            group_vars = .default_groups(),
                            normalize_weights = TRUE,
                            xgb_params = NULL,
                            nrounds = 200,
                            seed = NULL,
                            verbose = 0,
                            min_n_eff = 100) {
  model <- match.arg(model)

  design <- build_survey_design(
    data = data,
    y_col = y_col,
    weight_col = weight_col,
    group_vars = group_vars,
    normalize_weights = normalize_weights
  )

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

    shap_main_mat <- shap_obj$shap_main
    attr(shap_main_mat, "X_main") <- design$X_main

    main_sum <- summarize_main_shap(
      shap_main = shap_main_mat,
      main_map = design$main_map,
      model = model,
      w = design$w,
      min_n_eff = min_n_eff
    )

    int_sum <- summarize_interaction_shap(
      interaction_obj = shap_obj$shap_interaction,
      design = design,
      model = model,
      w = design$w,
      min_n_eff = min_n_eff
    )

    out <- list(
      model = model,
      fit = fit_obj,
      design = design,
      shap_main = shap_obj$shap_main,
      shap_interaction = shap_obj$shap_interaction,
      main_strength = main_sum$strength,
      main_direction = main_sum$direction,
      interaction_strength = int_sum$strength,
      interaction_direction = int_sum$direction,
      scale = shap_obj$scale
    )
  } else {
    shap_main <- compute_shap_xgb_main(fit_obj)
    shap_int <- compute_shap_xgb_interaction(fit_obj)

    shap_main_mat <- shap_main$shap
    attr(shap_main_mat, "X_main") <- design$X_main

    main_sum <- summarize_main_shap(
      shap_main = shap_main_mat,
      main_map = design$main_map,
      model = model,
      w = design$w,
      min_n_eff = min_n_eff
    )

    int_sum <- summarize_interaction_shap(
      interaction_obj = shap_int$interaction,
      design = design,
      model = model,
      w = design$w,
      min_n_eff = min_n_eff
    )

    out <- list(
      model = model,
      fit = fit_obj,
      design = design,
      shap_main = shap_main$shap,
      shap_interaction = shap_int$interaction,
      main_strength = main_sum$strength,
      main_direction = main_sum$direction,
      interaction_strength = int_sum$strength,
      interaction_direction = int_sum$direction,
      scale = shap_main$scale
    )
  }

  if (!is.null(output_dir)) {
    if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

    utils::write.csv(out$main_strength,
                     file = file.path(output_dir, "main_strength.csv"),
                     row.names = FALSE)
    utils::write.csv(out$main_direction,
                     file = file.path(output_dir, "main_direction.csv"),
                     row.names = FALSE)
    utils::write.csv(out$interaction_strength,
                     file = file.path(output_dir, "interaction_strength.csv"),
                     row.names = FALSE)
    utils::write.csv(out$interaction_direction,
                     file = file.path(output_dir, "interaction_direction.csv"),
                     row.names = FALSE)
  }

  out
}