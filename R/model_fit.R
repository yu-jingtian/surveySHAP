#' Fit one of the four supported survey models
#'
#' Supported models are \code{"lm"}, \code{"glm"}, \code{"xgb_numeric"},
#' and \code{"xgb_logistic"}.
#'
#' For \code{lm}/\code{glm}, the fit is performed on a sum-to-zero constrained
#' basis so that no factor level is treated as an artificial reference group.
#' The fitted object returned by this function includes reconstructed full
#' level-by-level coefficients for both main effects and interactions.
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
    beta_full["(Intercept)"] <- if ("(Intercept)" %in% names(gamma)) gamma["(Intercept)"] else 0

    ## main effects
    for (g in design$group_vars) {
      levs <- design$group_levels[[g]]
      L <- length(levs)
      sc_names <- if (L > 1) {
        paste0(g, "__SC__", safe_level(levs[seq_len(L - 1)]))
      } else {
        character(0)
      }
      gamma_g <- if (length(sc_names) > 0) gamma[sc_names] else numeric(0)
      gamma_g[is.na(gamma_g)] <- 0
      beta_g <- .reconstruct_main_sum_to_zero(gamma_g, levs = levs)

      full_names <- paste0(g, "__", safe_level(levs))
      beta_full[full_names] <- as.numeric(beta_g)
    }

    ## interactions
    pair_df <- utils::combn(design$group_vars, 2, simplify = FALSE)
    for (pair in pair_df) {
      g1 <- pair[1]
      g2 <- pair[2]
      levs1 <- design$group_levels[[g1]]
      levs2 <- design$group_levels[[g2]]
      L1 <- length(levs1)
      L2 <- length(levs2)

      if (L1 < 2 || L2 < 2) {
        B <- matrix(0, nrow = L1, ncol = L2, dimnames = list(levs1, levs2))
      } else {
        gamma_names <- expand.grid(
          i = seq_len(L1 - 1),
          j = seq_len(L2 - 1),
          KEEP.OUT.ATTRS = FALSE,
          stringsAsFactors = FALSE
        )
        sc_cols <- paste0(
          g1, "__SC__", safe_level(levs1[gamma_names$i]),
          "___",
          g2, "__SC__", safe_level(levs2[gamma_names$j])
        )
        gamma_vec <- gamma[sc_cols]
        gamma_vec[is.na(gamma_vec)] <- 0
        gamma_mat <- matrix(gamma_vec, nrow = L1 - 1, ncol = L2 - 1, byrow = FALSE)
        B <- .reconstruct_interaction_sum_to_zero(gamma_mat, levs1 = levs1, levs2 = levs2)
      }

      for (i in seq_len(L1)) {
        for (j in seq_len(L2)) {
          full_name <- paste0(
            g1, "__", safe_level(levs1[i]),
            "___",
            g2, "__", safe_level(levs2[j])
          )
          beta_full[full_name] <- B[i, j]
        }
      }
    }

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

  ## IMPORTANT:
  ## xgboost uses ONLY X_main, matching the original repo
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