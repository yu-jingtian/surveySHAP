#' Build the survey design used by all four model types
#'
#' Creates a cleaned modeling frame, full one-hot main-effect design,
#' full dummy-by-dummy interaction design restricted to pairs of different
#' variable groups, and a separate sum-to-zero coded linear design used by
#' \code{lm}/\code{glm}.
#'
#' For xgboost models, only the main one-hot feature matrix is used as model
#' input. Interactions for xgboost are obtained from native TreeSHAP interaction
#' values, not by feeding explicit interaction columns into the model.
#'
#' @param data A data.frame containing the survey variables.
#' @param y_col Outcome column name. Default is \code{"gun_control"}.
#' @param weight_col Optional weight column name. Default is \code{"weight"}.
#' @param group_vars Variable groups used in the model.
#' @param normalize_weights Logical; normalize weights to mean 1.
#'
#' @return A list containing the cleaned data, weights, outcomes, one-hot
#'   design objects for SHAP summaries, and the sum-to-zero coded linear
#'   design used for \code{lm}/\code{glm}.
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
  main_sc_blocks <- list()
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

    main_sc_blocks[[g]] <- .make_sum_coded_main(mm, g = g, levs = levs)
  }

  X_main <- do.call(cbind, main_blocks)
  main_map <- do.call(rbind, main_map)
  rownames(main_map) <- NULL

  pair_df <- utils::combn(group_vars, 2, simplify = FALSE)

  int_blocks <- list()
  int_map <- list()

  int_sc_blocks <- list()
  int_sc_meta <- list()

  k_full <- 0L
  k_sc <- 0L

  for (pair in pair_df) {
    g1 <- pair[1]
    g2 <- pair[2]

    m1 <- main_blocks[[g1]]
    m2 <- main_blocks[[g2]]
    map1 <- main_map[main_map$feature == g1, , drop = FALSE]
    map2 <- main_map[main_map$feature == g2, , drop = FALSE]

    ## full dummy-by-dummy interaction block for lm/glm SHAP summaries
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

    ## reduced sum-to-zero interaction block for lm/glm fitting
    sc1 <- main_sc_blocks[[g1]]
    sc2 <- main_sc_blocks[[g2]]
    levs1 <- group_levels[[g1]]
    levs2 <- group_levels[[g2]]

    for (i in seq_len(ncol(sc1))) {
      for (j in seq_len(ncol(sc2))) {
        k_sc <- k_sc + 1L
        cname_sc <- paste0(
          g1, "__SC__", safe_level(levs1[i]),
          "___",
          g2, "__SC__", safe_level(levs2[j])
        )
        int_sc_blocks[[k_sc]] <- matrix(sc1[, i] * sc2[, j], ncol = 1,
                                        dimnames = list(NULL, cname_sc))
        int_sc_meta[[k_sc]] <- data.frame(
          colname = cname_sc,
          feature1 = g1,
          feature2 = g2,
          i = i,
          j = j,
          stringsAsFactors = FALSE
        )
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

  X_main_sc <- do.call(cbind, main_sc_blocks)

  X_int_sc <- if (length(int_sc_blocks) > 0) {
    do.call(cbind, int_sc_blocks)
  } else {
    matrix(0, nrow(df), 0)
  }

  int_sc_meta <- if (length(int_sc_meta) > 0) {
    do.call(rbind, int_sc_meta)
  } else {
    data.frame(
      colname = character(0),
      feature1 = character(0),
      feature2 = character(0),
      i = integer(0),
      j = integer(0),
      stringsAsFactors = FALSE
    )
  }
  rownames(int_sc_meta) <- NULL

  X_linear <- cbind(`(Intercept)` = 1, X_main_sc, X_int_sc)

  ## IMPORTANT:
  ## xgboost uses ONLY the main one-hot matrix, matching the original repo setup
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

    X_main_sc = X_main_sc,
    X_int_sc = X_int_sc,
    X_linear = X_linear,
    X_xgb = X_xgb,

    main_map = main_map,
    int_map = int_map,
    int_sc_meta = int_sc_meta,
    group_levels = group_levels,
    group_vars = group_vars
  )
}