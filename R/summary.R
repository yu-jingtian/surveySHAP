#' Summarize SHAP results from run_survey_shap()
#'
#' Produces compact summaries for the main outputs of `run_survey_shap()`:
#' group-level strength (one-way and two-way) and directional effects.
#'
#' Direction tables are sorted by signed SHAP values and truncated to
#' top/bottom `top_n`. Direction filters use effective sample size (ESS):
#' `min_n_eff_main` for one-way level summaries, and `min_n_eff_interaction`
#' for interaction direction rows.
#'
#' @param shap_rslt A list returned by `run_survey_shap()`.
#' @param top_n Number of top and bottom rows to return for direction tables.
#' @param min_n_eff_main Minimum ESS for one-way direction level results.
#' @param min_n_eff_interaction Minimum ESS for interaction direction results.
#' @param quiet If TRUE, do not print; only return the summary object.
#'
#' @return An object of class `surveySHAP_summary` (a named list).
#' @export
summarize_shap_rslt <- function(
    shap_rslt,
    top_n = 5L,
    min_n_eff_main = 100,
    min_n_eff_interaction = 100,
    quiet = FALSE
) {
  if (!is.list(shap_rslt)) {
    stop("`shap_rslt` must be the list returned by `run_survey_shap()`.")
  }

  req <- c(
    "strength_main_group",
    "direction_main_active",
    "strength_interaction_group",
    "direction_interaction_active"
  )
  miss <- setdiff(req, names(shap_rslt))
  if (length(miss) > 0) {
    stop("`shap_rslt` is missing: ", paste(miss, collapse = ", "))
  }

  top_n <- as.integer(top_n)
  min_n_eff_main <- as.numeric(min_n_eff_main)
  min_n_eff_interaction <- as.numeric(min_n_eff_interaction)

  strength_main <- shap_rslt$strength_main_group
  strength_int  <- shap_rslt$strength_interaction_group

  # ---- one-way direction (complete) ----
  dir_main <- shap_rslt$direction_main_active
  req_main <- c("feature", "level", "shap_active", "n_raw", "w_sum", "n_eff")
  miss_main <- setdiff(req_main, names(dir_main))
  if (length(miss_main) > 0) {
    stop("`direction_main_active` is missing columns: ", paste(miss_main, collapse = ", "))
  }

  direction_main_complete <- dir_main[, req_main, drop = FALSE]
  before_main <- nrow(direction_main_complete)

  # filter for printing (exclude NA shap_active rows, and ESS threshold)
  dir_main_f <- direction_main_complete[
    !is.na(direction_main_complete$shap_active) &
      !is.na(direction_main_complete$n_eff) &
      direction_main_complete$n_eff >= min_n_eff_main,
    ,
    drop = FALSE
  ]
  after_main <- nrow(dir_main_f)

  direction_main_top <- utils::head(dir_main_f[order(-dir_main_f$shap_active), , drop = FALSE], top_n)
  direction_main_bottom <- utils::head(dir_main_f[order(dir_main_f$shap_active), , drop = FALSE], top_n)

  # ---- interaction direction (complete) ----
  dir_int <- shap_rslt$direction_interaction_active
  req_int <- c("type", "pair", "group_i", "level_i", "group_j", "level_j", "direction", "n_raw", "w_sum", "n_eff")
  miss_int <- setdiff(req_int, names(dir_int))
  if (length(miss_int) > 0) {
    stop("`direction_interaction_active` is missing columns: ", paste(miss_int, collapse = ", "))
  }

  direction_interaction_complete <- dir_int[, req_int, drop = FALSE]
  before_int <- nrow(direction_interaction_complete)

  dir_int_f <- direction_interaction_complete[
    !is.na(direction_interaction_complete$direction) &
      !is.na(direction_interaction_complete$n_eff) &
      direction_interaction_complete$n_eff >= min_n_eff_interaction,
    ,
    drop = FALSE
  ]
  after_int <- nrow(dir_int_f)

  direction_int_top <- utils::head(dir_int_f[order(-dir_int_f$direction), , drop = FALSE], top_n)
  direction_int_bottom <- utils::head(dir_int_f[order(dir_int_f$direction), , drop = FALSE], top_n)

  out <- list(
    strength_main_group = strength_main,
    strength_interaction_group = strength_int,

    direction_main_complete = direction_main_complete,
    direction_interaction_complete = direction_interaction_complete,

    direction_main_top = direction_main_top,
    direction_main_bottom = direction_main_bottom,
    direction_interaction_top = direction_int_top,
    direction_interaction_bottom = direction_int_bottom,

    filters = list(
      one_way = list(
        min_n_eff_main = min_n_eff_main,
        before = before_main,
        after = after_main,
        removed = before_main - after_main
      ),
      two_way = list(
        min_n_eff_interaction = min_n_eff_interaction,
        before = before_int,
        after = after_int,
        removed = before_int - after_int
      )
    )
  )
  class(out) <- "surveySHAP_summary"

  if (!quiet) print(out)
  out
}

#' @export
print.surveySHAP_summary <- function(x, ...) {
  cat("\n=== surveySHAP summary ===\n\n")

  cat("One-way strength (group):\n")
  print(x$strength_main_group)
  cat("\n")

  cat("Two-way strength (group pairs):\n")
  print(x$strength_interaction_group)
  cat("\n")

  cat("Filters:\n")
  cat("  One-way direction: kept ",
      x$filters$one_way$after, "/", x$filters$one_way$before,
      " (min_n_eff_main = ", x$filters$one_way$min_n_eff_main, ")\n", sep = "")
  cat("  Two-way direction: kept ",
      x$filters$two_way$after, "/", x$filters$two_way$before,
      " (min_n_eff_interaction = ", x$filters$two_way$min_n_eff_interaction, ")\n\n", sep = "")

  cat("One-way direction: TOP (positive)\n")
  print(x$direction_main_top)
  cat("\nOne-way direction: BOTTOM (negative)\n")
  print(x$direction_main_bottom)

  cat("\n\nTwo-way direction: TOP (positive)\n")
  print(x$direction_interaction_top)
  cat("\nTwo-way direction: BOTTOM (negative)\n")
  print(x$direction_interaction_bottom)

  invisible(x)
}
