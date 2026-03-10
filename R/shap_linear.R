#' Compute exact SHAP values for lm/glm fits
#'
#' For \code{lm}, SHAP values are computed on the count scale. For
#' \code{glm} with binomial-family mean structure, SHAP values are computed
#' on the logit scale.
#'
#' Main SHAP uses
#' \deqn{\phi_j = \beta_j (x_j - \mu_j)}
#'
#' Interaction SHAP uses
#' \deqn{\phi_{jk} = \beta_{jk} (x_j x_k - \mu_{jk})}
#'
#' where the \eqn{\beta}'s are reconstructed from a sum-to-zero constrained
#' fit, so that no factor level is treated as an artificial reference level.
#'
#' @param fit_obj A fitted object from [fit_survey_model()] with model equal
#'   to \code{"lm"} or \code{"glm"}.
#'
#' @return A list with main and interaction SHAP matrices and associated maps.
#' @export
compute_shap_linear <- function(fit_obj) {
  stopifnot(fit_obj$model_type %in% c("lm", "glm"))

  design <- fit_obj$design
  beta_full <- fit_obj$beta_full
  X_main <- design$X_main
  X_int <- design$X_int
  w <- design$w
  ww <- if (is.null(w)) rep(1, nrow(X_main)) else w

  ## main SHAP
  mu_main <- vapply(seq_len(ncol(X_main)), function(j) .wmean(X_main[, j], ww), numeric(1))
  shap_main <- sweep(X_main, 2, mu_main, FUN = "-")

  beta_main <- beta_full[colnames(X_main)]
  beta_main[is.na(beta_main)] <- 0

  shap_main <- sweep(shap_main, 2, beta_main, FUN = "*")
  colnames(shap_main) <- colnames(X_main)

  ## interaction SHAP
  if (ncol(X_int) > 0) {
    mu_int <- vapply(seq_len(ncol(X_int)), function(j) .wmean(X_int[, j], ww), numeric(1))
    shap_int <- sweep(X_int, 2, mu_int, FUN = "-")

    beta_int <- beta_full[colnames(X_int)]
    beta_int[is.na(beta_int)] <- 0

    shap_int <- sweep(shap_int, 2, beta_int, FUN = "*")
    colnames(shap_int) <- colnames(X_int)
  } else {
    shap_int <- matrix(0, nrow(X_main), 0)
  }

  list(
    shap_main = shap_main,
    shap_interaction = shap_int,
    main_map = design$main_map,
    int_map = design$int_map,
    baseline = unname(beta_full["(Intercept)"]),
    scale = fit_obj$scale
  )
}