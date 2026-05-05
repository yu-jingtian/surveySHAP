#' Compute exact SHAP values for lm/glm fits
#'
#' For \code{lm}, SHAP values are computed on the count scale. For
#' \code{glm} with binomial-family mean structure, SHAP values are computed
#' on the logit scale.
#'
#' Main SHAP uses
#' \deqn{\phi_j = \beta_j (x_j - \mu_j)}
#'
#' Interaction SHAP uses the pairwise SHAP-interaction form
#' \deqn{\phi_{jk}^{\mathrm{int}} = \tfrac{1}{2}\,\beta_{jk}(x_j - \mu_j)(x_k - \mu_k)}
#'
#' where the \eqn{\beta}'s come from a regular treatment-coded linear fit and
#' are mapped back to the full one-hot main-effect and dummy-by-dummy
#' interaction design. Reference-level terms have coefficient zero.
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

  ## Main SHAP.
  mu_main <- vapply(seq_len(ncol(X_main)), function(j) .wmean(X_main[, j], ww), numeric(1))
  shap_main <- sweep(X_main, 2, mu_main, FUN = "-")

  beta_main <- beta_full[colnames(X_main)]
  beta_main[is.na(beta_main)] <- 0

  shap_main <- sweep(shap_main, 2, beta_main, FUN = "*")
  colnames(shap_main) <- colnames(X_main)

  ## Interaction SHAP.
  ## For each dummy-pair interaction column corresponding to (j, k), use the
  ## pairwise SHAP-interaction form
  ##   0.5 * beta_jk * (x_j - mu_j) * (x_k - mu_k)
  ## rather than the centered explicit interaction-term contribution
  ##   beta_jk * (x_j x_k - mu_jk).
  if (ncol(X_int) > 0) {
    shap_int <- matrix(0, nrow = nrow(X_main), ncol = ncol(X_int))
    colnames(shap_int) <- colnames(X_int)

    beta_int <- beta_full[colnames(X_int)]
    beta_int[is.na(beta_int)] <- 0

    main_lookup <- setNames(seq_len(ncol(X_main)), colnames(X_main))

    for (i in seq_len(nrow(design$int_map))) {
      col1 <- paste0(
        design$int_map$feature1[i], "__", safe_level(design$int_map$level1[i])
      )
      col2 <- paste0(
        design$int_map$feature2[i], "__", safe_level(design$int_map$level2[i])
      )

      j1 <- unname(main_lookup[col1])
      j2 <- unname(main_lookup[col2])

      if (is.na(j1) || is.na(j2)) {
        stop("Could not match interaction level pair back to X_main columns.")
      }

      shap_int[, i] <- 0.5 * beta_int[i] *
        (X_main[, j1] - mu_main[j1]) *
        (X_main[, j2] - mu_main[j2])
    }
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
