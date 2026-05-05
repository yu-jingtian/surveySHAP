#' Build the survey design used by all four model types
#'
#' Creates a cleaned modeling frame, full one-hot main-effect design,
#' full dummy-by-dummy interaction design restricted to pairs of different
#' variable groups, and a regular treatment-coded linear design used by
#' \code{lm}/\code{glm}.
#'
#' For linear models, the first level of each factor is treated as the
#' reference level. For xgboost models, only the main one-hot feature matrix is
#' used as model input. Interactions for xgboost are obtained from native
#' TreeSHAP interaction values, not by feeding explicit interaction columns
#' into the model.
#'
#' @param data A data.frame containing the survey variables.
#' @param y_col Outcome column name. Default is \code{"gun_control"}.
#' @param weight_col Optional weight column name. Default is \code{"weight"}.
#' @param group_vars Variable groups used in the model.
#' @param normalize_weights Logical; normalize weights to mean 1.
#'
#' @return A list containing the cleaned data, weights, outcomes, one-hot
#'   design objects for SHAP summaries, and the treatment-coded linear design
#'   used for \code{lm}/\code{glm}.
#' @export
build_survey_design <- function(data,
                                y_col = "gun_control",
                                weight_col = "weight",
                                group_vars = .default_groups(),
                                normalize_weights = TRUE) {
  prep <- prepare_survey_data(
    data = data,
    y_col = y_col,
    weight_col = weight_col,
    group_vars = group_vars,
    normalize_weights = normalize_weights
  )

  df <- prep$df
  w <- prep$w

  main_blocks <- list()
  main_map <- list()
  linear_main_blocks <- list()
  group_levels <- list()

  for (g in group_vars) {
    levs <- levels(df[[g]])
    group_levels[[g]] <- levs

    mm <- stats::model.matrix(~ . - 1, data = setNames(data.frame(df[[g]]), g))
    colnames(mm) <- paste0(g, "__", safe_level(levs))

    main_blocks[[g]] <- mm
    main_map[[g]] <- data.frame(
      colname = colnames(mm),
      feature = g,
      level = levs,
      stringsAsFactors = FALSE
    )

    ## Regular treatment/reference coding for lm/glm:
    ## use the first factor level as reference, matching R's default
    ## contr.treatment convention. The columns keep the corresponding
    ## one-hot names so they can be mapped directly back to the full
    ## indicator-level SHAP summaries.
    linear_main_blocks[[g]] <- .make_treatment_coded_main(mm, g = g, levs = levs)
  }

  X_main <- do.call(cbind, main_blocks)
  main_map <- do.call(rbind, main_map)
  rownames(main_map) <- NULL

  pair_df <- utils::combn(group_vars, 2, simplify = FALSE)

  int_blocks <- list()
  int_map <- list()

  linear_int_blocks <- list()

  k_full <- 0L
  k_linear <- 0L

  for (pair in pair_df) {
    g1 <- pair[1]
    g2 <- pair[2]

    m1 <- main_blocks[[g1]]
    m2 <- main_blocks[[g2]]
    map1 <- main_map[main_map$feature == g1, , drop = FALSE]
    map2 <- main_map[main_map$feature == g2, , drop = FALSE]

    ## Full dummy-by-dummy interaction block used for SHAP summaries.
    for (i in seq_len(ncol(m1))) {
      for (j in seq_len(ncol(m2))) {
        k_full <- k_full + 1L
        z <- m1[, i] * m2[, j]
        cname <- paste0(
          g1, "__", safe_level(map1$level[i]),
          "___",
          g2, "__", safe_level(map2$level[j])
        )
        int_blocks[[k_full]] <- matrix(z, ncol = 1, dimnames = list(NULL, cname))
        int_map[[k_full]] <- data.frame(
          colname = cname,
          feature1 = g1,
          feature2 = g2,
          level1 = map1$level[i],
          level2 = map2$level[j],
          stringsAsFactors = FALSE
        )
      }
    }

    ## Treatment-coded interaction block for lm/glm fitting. Since the first
    ## level of each factor is the reference, only non-reference dummy columns
    ## enter the regular linear-model design.
    lm1 <- linear_main_blocks[[g1]]
    lm2 <- linear_main_blocks[[g2]]

    if (ncol(lm1) > 0 && ncol(lm2) > 0) {
      for (i in seq_len(ncol(lm1))) {
        for (j in seq_len(ncol(lm2))) {
          k_linear <- k_linear + 1L
          cname_linear <- paste0(colnames(lm1)[i], "___", colnames(lm2)[j])
          linear_int_blocks[[k_linear]] <- matrix(
            lm1[, i] * lm2[, j],
            ncol = 1,
            dimnames = list(NULL, cname_linear)
          )
        }
      }
    }
  }

  X_int <- if (length(int_blocks) > 0) {
    do.call(cbind, int_blocks)
  } else {
    matrix(0, nrow(df), 0)
  }

  int_map <- if (length(int_map) > 0) {
    do.call(rbind, int_map)
  } else {
    data.frame(
      colname = character(0),
      feature1 = character(0),
      feature2 = character(0),
      level1 = character(0),
      level2 = character(0),
      stringsAsFactors = FALSE
    )
  }
  rownames(int_map) <- NULL

  X_main_linear <- if (length(linear_main_blocks) > 0) {
    do.call(cbind, linear_main_blocks)
  } else {
    matrix(0, nrow(df), 0)
  }

  X_int_linear <- if (length(linear_int_blocks) > 0) {
    do.call(cbind, linear_int_blocks)
  } else {
    matrix(0, nrow(df), 0)
  }

  X_linear <- cbind(`(Intercept)` = 1, X_main_linear, X_int_linear)

  ## xgboost uses ONLY the main one-hot matrix, matching the original repo setup.
  X_xgb <- X_main

  list(
    df = df,
    w = w,
    y_count = as.numeric(df$y_count),
    y_prop = as.numeric(df$y_count) / 6,
    y_success = as.numeric(df$y_count),
    y_failure = 6 - as.numeric(df$y_count),

    X_main = X_main,
    X_int = X_int,
    X = cbind(`(Intercept)` = 1, X_main, X_int),

    X_main_linear = X_main_linear,
    X_int_linear = X_int_linear,
    X_linear = X_linear,
    X_xgb = X_xgb,

    main_map = main_map,
    int_map = int_map,
    group_levels = group_levels,
    group_vars = group_vars
  )
}
