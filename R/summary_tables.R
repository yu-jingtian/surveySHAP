#' Summarize main-effect SHAP into strength and direction tables
#'
#' @param shap_main An \eqn{n \times p} SHAP matrix for main effects.
#' @param main_map Main-effect column map from [build_survey_design()].
#' @param model One of \code{"lm"}, \code{"glm"}, \code{"xgb_numeric"},
#'   or \code{"xgb_logistic"}.
#' @param w Optional weights.
#' @param min_n_eff Minimum effective sample size for direction rows.
#'
#' @return A list with \code{strength} and \code{direction} data.frames.
#' @export
summarize_main_shap <- function(shap_main,
                                main_map,
                                model = c("lm", "glm", "xgb_numeric", "xgb_logistic"),
                                w = NULL,
                                min_n_eff = 100) {
  model <- match.arg(model)
  ww <- if (is.null(w)) rep(1, nrow(shap_main)) else as.numeric(w)

  groups <- unique(main_map$feature)

  # Strength is always grouped at the variable level
  strength <- do.call(rbind, lapply(groups, function(g) {
    cols <- main_map$colname[main_map$feature == g]
    s <- rowSums(shap_main[, cols, drop = FALSE])
    data.frame(group = g, strength = .wmean(abs(s), ww), stringsAsFactors = FALSE)
  }))
  strength <- strength[order(-strength$strength), , drop = FALSE]

  X_main_attr <- attr(shap_main, "X_main")
  if (is.null(X_main_attr)) {
    stop("`shap_main` must carry attr(., 'X_main') for direction summaries.")
  }

  direction_list <- vector("list", nrow(main_map))

  for (i in seq_len(nrow(main_map))) {
    feature <- main_map$feature[i]
    level   <- main_map$level[i]
    col     <- main_map$colname[i]

    idx <- which(X_main_attr[, col] == 1)
    w0  <- ww[idx]

    # For lm/glm, keep the level-specific definition from the LaTeX:
    # Direction(k) = E[ phi_k | k ]
    #
    # For xgboost, use grouped feature SHAP conditional on the level:
    # Direction(k) = E[ phi_feature | k ]
    # where phi_feature sums over all one-hot columns of the feature.
    if (model %in% c("lm", "glm")) {
      val <- shap_main[idx, col]
    } else {
      feature_cols <- main_map$colname[main_map$feature == feature]
      val <- rowSums(shap_main[idx, feature_cols, drop = FALSE])
    }

    direction_list[[i]] <- data.frame(
      feature = feature,
      level = level,
      direction = .wmean(val, w0),
      n_raw = length(idx),
      w_sum = sum(w0),
      n_eff = .n_eff(w0),
      stringsAsFactors = FALSE
    )
  }

  direction <- do.call(rbind, direction_list)
  direction <- direction[direction$n_eff >= min_n_eff, , drop = FALSE]
  rownames(direction) <- NULL

  list(strength = strength, direction = direction)
}

#' Summarize interaction SHAP into strength and direction tables
#'
#' @param interaction_obj Interaction SHAP representation. For linear models,
#'   this is an \eqn{n \times q} matrix with one column per level pair. For
#'   xgboost, this is an \eqn{n \times p \times p} array over the main one-hot
#'   features only.
#' @param design A design object from [build_survey_design()].
#' @param model One of the four supported model labels.
#' @param w Optional weights.
#' @param min_n_eff Minimum effective sample size for direction rows.
#'
#' @return A list with \code{strength} and \code{direction} data.frames.
#' @export
summarize_interaction_shap <- function(interaction_obj,
                                       design,
                                       model = c("lm", "glm", "xgb_numeric", "xgb_logistic"),
                                       w = NULL,
                                       min_n_eff = 100) {
  model <- match.arg(model)
  ww <- if (is.null(w)) rep(1, nrow(design$df)) else as.numeric(w)

  if (model %in% c("lm", "glm")) {
    shap_int <- interaction_obj
    int_map <- design$int_map

    strength_pairs <- split(
      seq_len(nrow(int_map)),
      feature_pair_label(int_map$feature1, int_map$feature2)
    )

    strength <- do.call(rbind, lapply(names(strength_pairs), function(lbl) {
      idx <- strength_pairs[[lbl]]
      s <- rowSums(shap_int[, idx, drop = FALSE])
      feats <- strsplit(lbl, "__", fixed = TRUE)[[1]]
      data.frame(
        feature1 = feats[1],
        feature2 = feats[2],
        strength = .wmean(abs(s), ww),
        stringsAsFactors = FALSE
      )
    }))
    strength <- strength[order(-strength$strength), , drop = FALSE]

    # For lm/glm, keep the level-pair-specific conditional definition:
    # Direction(k,l) = E[ phi_{k,l} | k,l ]
    direction <- do.call(rbind, lapply(seq_len(nrow(int_map)), function(i) {
      idx <- which(design$X_int[, int_map$colname[i]] == 1)
      w0 <- ww[idx]
      val <- shap_int[idx, i]
      data.frame(
        feature1 = int_map$feature1[i],
        feature2 = int_map$feature2[i],
        level1 = int_map$level1[i],
        level2 = int_map$level2[i],
        direction = .wmean(val, w0),
        n_raw = length(idx),
        w_sum = sum(w0),
        n_eff = .n_eff(w0),
        stringsAsFactors = FALSE
      )
    }))
    direction <- direction[direction$n_eff >= min_n_eff, , drop = FALSE]
    rownames(direction) <- NULL
    return(list(strength = strength, direction = direction))
  }

  ## xgboost: interaction array is over X_main only
  arr <- interaction_obj
  mm <- design$main_map
  p_main <- ncol(design$X_main)

  if (dim(arr)[2] != p_main || dim(arr)[3] != p_main) {
    stop("xgboost interaction array dimension does not match ncol(design$X_main).")
  }

  # Build all cross-feature dummy-column pairs
  pairs <- utils::combn(seq_len(nrow(mm)), 2)
  keep <- mm$feature[pairs[1, ]] != mm$feature[pairs[2, ]]
  pairs <- pairs[, keep, drop = FALSE]

  pair_keys <- feature_pair_label(mm$feature[pairs[1, ]], mm$feature[pairs[2, ]])
  uniq_keys <- unique(pair_keys)

  # Strength for xgboost: aggregate all dummy-pair interaction SHAPs
  # within the feature pair, then take weighted mean absolute value
  strength <- do.call(rbind, lapply(uniq_keys, function(key) {
    sel <- which(pair_keys == key)
    if (length(sel) == 0L) return(NULL)

    tmp <- matrix(0, nrow = nrow(design$X_main), ncol = length(sel))
    for (s in seq_along(sel)) {
      ii <- pairs[1, sel[s]]
      jj <- pairs[2, sel[s]]
      tmp[, s] <- arr[, ii, jj]
    }
    s <- rowSums(tmp)

    feats <- strsplit(key, "__", fixed = TRUE)[[1]]
    data.frame(
      feature1 = feats[1],
      feature2 = feats[2],
      strength = .wmean(abs(s), ww),
      stringsAsFactors = FALSE
    )
  }))
  strength <- strength[order(-strength$strength), , drop = FALSE]

  # Direction for xgboost:
  # For each level pair (k,l), condition on rows in that cell, but use the
  # aggregated feature-pair interaction SHAP, not only the single dummy-pair SHAP.
  direction_pairs <- unique(data.frame(
    feature1 = mm$feature[pairs[1, ]],
    feature2 = mm$feature[pairs[2, ]],
    level1   = mm$level[pairs[1, ]],
    level2   = mm$level[pairs[2, ]],
    stringsAsFactors = FALSE
  ))

  direction <- do.call(rbind, lapply(seq_len(nrow(direction_pairs)), function(r) {
    f1 <- direction_pairs$feature1[r]
    f2 <- direction_pairs$feature2[r]
    l1 <- direction_pairs$level1[r]
    l2 <- direction_pairs$level2[r]

    col1 <- mm$colname[mm$feature == f1 & mm$level == l1]
    col2 <- mm$colname[mm$feature == f2 & mm$level == l2]

    idx <- which(design$X_main[, col1] == 1 & design$X_main[, col2] == 1)
    w0 <- ww[idx]

    cols1 <- which(mm$feature == f1)
    cols2 <- which(mm$feature == f2)

    if (length(idx) == 0L) {
      val <- numeric(0)
    } else {
      tmp <- matrix(0, nrow = length(idx), ncol = length(cols1) * length(cols2))
      cc <- 1L
      for (ii in cols1) {
        for (jj in cols2) {
          if (ii < jj) {
            tmp[, cc] <- arr[idx, ii, jj]
          } else {
            tmp[, cc] <- arr[idx, jj, ii]
          }
          cc <- cc + 1L
        }
      }
      val <- rowSums(tmp)
    }

    data.frame(
      feature1 = f1,
      feature2 = f2,
      level1 = l1,
      level2 = l2,
      direction = .wmean(val, w0),
      n_raw = length(idx),
      w_sum = sum(w0),
      n_eff = .n_eff(w0),
      stringsAsFactors = FALSE
    )
  }))

  direction <- direction[direction$n_eff >= min_n_eff, , drop = FALSE]
  rownames(direction) <- NULL

  list(strength = strength, direction = direction)
}

#' Summarize results from [run_survey_shap()]
#'
#' @param shap_rslt A result list from [run_survey_shap()].
#' @param top_n Number of top and bottom rows to print for direction tables.
#' @param quiet Logical; if TRUE, do not print.
#'
#' @return An object of class \code{surveySHAP_summary}.
#' @export
summarize_shap_rslt <- function(shap_rslt, top_n = 5L, quiet = FALSE) {
  out <- list(
    main_strength = shap_rslt$main_strength,
    interaction_strength = shap_rslt$interaction_strength,
    main_direction_top = utils::head(shap_rslt$main_direction[order(-shap_rslt$main_direction$direction), , drop = FALSE], top_n),
    main_direction_bottom = utils::head(shap_rslt$main_direction[order(shap_rslt$main_direction$direction), , drop = FALSE], top_n),
    interaction_direction_top = utils::head(shap_rslt$interaction_direction[order(-shap_rslt$interaction_direction$direction), , drop = FALSE], top_n),
    interaction_direction_bottom = utils::head(shap_rslt$interaction_direction[order(shap_rslt$interaction_direction$direction), , drop = FALSE], top_n)
  )
  class(out) <- "surveySHAP_summary"
  if (!quiet) print(out)
  out
}

#' @export
print.surveySHAP_summary <- function(x, ...) {
  cat("\n=== surveySHAP summary ===\n\n")
  cat("One-way strength:\n")
  print(x$main_strength)
  cat("\nTwo-way strength:\n")
  print(x$interaction_strength)
  cat("\nOne-way direction: TOP\n")
  print(x$main_direction_top)
  cat("\nOne-way direction: BOTTOM\n")
  print(x$main_direction_bottom)
  cat("\n\nTwo-way direction: TOP\n")
  print(x$interaction_direction_top)
  cat("\nTwo-way direction: BOTTOM\n")
  print(x$interaction_direction_bottom)
  invisible(x)
}