#' Bootstrap a single strength target
#'
#' @param data Input data.frame.
#' @param target Either \code{"main"} or \code{"interaction"}.
#' @param feature For main strength, a single feature name. For interaction
#'   strength, a character vector of length 2 giving \code{feature1} and \code{feature2}.
#' @param model Model choice passed to [run_survey_shap()].
#' @param B Number of bootstrap resamples.
#' @param conf Confidence level.
#' @param seed Optional integer seed.
#'
#' @return A one-row data.frame with estimate, bootstrap mean, sd, and percentile CI.
#' @export
boot_strength <- function(data,
                          target = c("main", "interaction"),
                          feature,
                          model = c("lm", "glm", "xgb_numeric", "xgb_logistic"),
                          B = 100,
                          conf = 0.95,
                          seed = NULL) {
  target <- match.arg(target)
  model <- match.arg(model)
  if (!is.null(seed)) set.seed(as.integer(seed))
  n <- nrow(data)

  extract_one <- function(dat) {
    res <- run_survey_shap(dat, model = model)
    if (target == "main") {
      val <- res$main_strength$strength[res$main_strength$group == feature]
    } else {
      f <- sort(feature)
      idx <- res$interaction_strength$feature1 == f[1] & res$interaction_strength$feature2 == f[2]
      val <- res$interaction_strength$strength[idx]
    }
    if (length(val) == 0) NA_real_ else as.numeric(val[1])
  }

  est <- extract_one(data)
  boots <- vapply(seq_len(B), function(b) {
    idx <- sample.int(n, n, replace = TRUE)
    extract_one(data[idx, , drop = FALSE])
  }, numeric(1))

  alpha <- (1 - conf) / 2
  qs <- stats::quantile(boots, c(alpha, 1 - alpha), na.rm = TRUE, names = FALSE)
  data.frame(
    estimate = est,
    mean_boot = mean(boots, na.rm = TRUE),
    sd_boot = stats::sd(boots, na.rm = TRUE),
    ci_lo = qs[1],
    ci_hi = qs[2],
    stringsAsFactors = FALSE
  )
}

#' Bootstrap a single direction target
#'
#' @param data Input data.frame.
#' @param target Either \code{"main"} or \code{"interaction"}.
#' @param feature For main direction, a single feature name. For interaction
#'   direction, a character vector of length 2.
#' @param level For main direction, a single level name. For interaction
#'   direction, a character vector of length 2.
#' @param model Model choice passed to [run_survey_shap()].
#' @param B Number of bootstrap resamples.
#' @param conf Confidence level.
#' @param seed Optional integer seed.
#'
#' @return A one-row data.frame with estimate, bootstrap mean, sd, and percentile CI.
#' @export
boot_direction <- function(data,
                           target = c("main", "interaction"),
                           feature,
                           level,
                           model = c("lm", "glm", "xgb_numeric", "xgb_logistic"),
                           B = 100,
                           conf = 0.95,
                           seed = NULL) {
  target <- match.arg(target)
  model <- match.arg(model)
  if (!is.null(seed)) set.seed(as.integer(seed))
  n <- nrow(data)

  extract_one <- function(dat) {
    res <- run_survey_shap(dat, model = model, min_n_eff_main = 0, min_n_eff_interaction = 0)
    if (target == "main") {
      idx <- res$main_direction$feature == feature & res$main_direction$level == level
      val <- res$main_direction$direction[idx]
    } else {
      ord <- order(feature)
      f <- feature[ord]; l <- level[ord]
      idx <- res$interaction_direction$feature1 == f[1] &
        res$interaction_direction$feature2 == f[2] &
        res$interaction_direction$level1 == l[1] &
        res$interaction_direction$level2 == l[2]
      val <- res$interaction_direction$direction[idx]
    }
    if (length(val) == 0) NA_real_ else as.numeric(val[1])
  }

  est <- extract_one(data)
  boots <- vapply(seq_len(B), function(b) {
    idx <- sample.int(n, n, replace = TRUE)
    extract_one(data[idx, , drop = FALSE])
  }, numeric(1))

  alpha <- (1 - conf) / 2
  qs <- stats::quantile(boots, c(alpha, 1 - alpha), na.rm = TRUE, names = FALSE)
  data.frame(
    estimate = est,
    mean_boot = mean(boots, na.rm = TRUE),
    sd_boot = stats::sd(boots, na.rm = TRUE),
    ci_lo = qs[1],
    ci_hi = qs[2],
    stringsAsFactors = FALSE
  )
}
