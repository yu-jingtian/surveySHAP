#' Summarize SHAP results from run_survey_shap()
#'
#' Produces compact summaries for the main outputs of `run_survey_shap()`:
#' group-level strength (one-way and two-way) and directional effects
#' (top/bottom levels and pairs).
#'
#' Direction tables are sorted by signed SHAP values and truncated to
#' top/bottom `top_n`. Two-way direction results are filtered by
#' `min_n_interaction` to avoid unstable estimates from rare co-occurrences.
#'
#' @param shap_rslt A list returned by `run_survey_shap()`.
#' @param top_n Number of top and bottom rows to return for direction tables.
#' @param min_n_main Minimum per-level sample size (`n`) for one-way direction results.
#' @param min_n_interaction Minimum `n_active` for two-way direction results.
#' @param quiet If TRUE, do not print; only return the summary object.
#'
#' @return An object of class `surveySHAP_summary` (a named list) containing:
#' \describe{
#'   \item{strength_main_group}{Group-level one-way strength table.}
#'   \item{strength_interaction_group}{Group-level two-way strength table.}
#'   \item{direction_main_top}{Top `top_n` one-way directional rows (after filtering).}
#'   \item{direction_main_bottom}{Bottom `top_n` one-way directional rows (after filtering).}
#'   \item{direction_interaction_top}{Top `top_n` interaction directional rows (after filtering).}
#'   \item{direction_interaction_bottom}{Bottom `top_n` interaction directional rows (after filtering).}
#'   \item{filters}{A list describing how many rows were removed by sample-size filters.}
#' }
#' @export
summarize_shap_rslt <- function(
    shap_rslt,
    top_n = 5L,
    min_n_main = 50L,
    min_n_interaction = 100L,
    quiet = FALSE
) {
  # ---- validation ----
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
  min_n_main <- as.integer(min_n_main)
  min_n_interaction <- as.integer(min_n_interaction)

  # ---- strength tables (already sorted upstream) ----
  strength_main <- shap_rslt$strength_main_group
  strength_int  <- shap_rslt$strength_interaction_group

  # ---- one-way direction ----
  dir_main <- shap_rslt$direction_main_active
  before_main <- nrow(dir_main)

  dir_main_f <- dir_main[dir_main$n >= min_n_main, , drop = FALSE]
  after_main <- nrow(dir_main_f)

  direction_main_top <- utils::head(
    dir_main_f[order(-dir_main_f$shap_active), , drop = FALSE],
    top_n
  )
  direction_main_bottom <- utils::head(
    dir_main_f[order(dir_main_f$shap_active), , drop = FALSE],
    top_n
  )

  # ---- two-way direction ----
  dir_int <- shap_rslt$direction_interaction_active
  before_int <- nrow(dir_int)

  dir_int_f <- dir_int[dir_int$n_active >= min_n_interaction, , drop = FALSE]
  after_int <- nrow(dir_int_f)

  direction_int_top <- utils::head(
    dir_int_f[order(-dir_int_f$direction_active), , drop = FALSE],
    top_n
  )
  direction_int_bottom <- utils::head(
    dir_int_f[order(dir_int_f$direction_active), , drop = FALSE],
    top_n
  )

  out <- list(
    strength_main_group = strength_main,
    strength_interaction_group = strength_int,
    direction_main_top = direction_main_top,
    direction_main_bottom = direction_main_bottom,
    direction_interaction_top = direction_int_top,
    direction_interaction_bottom = direction_int_bottom,
    filters = list(
      one_way = list(
        min_n_main = min_n_main,
        before = before_main,
        after = after_main,
        removed = before_main - after_main
      ),
      two_way = list(
        min_n_interaction = min_n_interaction,
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
      " (min_n_main = ", x$filters$one_way$min_n_main, ")\n", sep = "")
  cat("  Two-way direction: kept ",
      x$filters$two_way$after, "/", x$filters$two_way$before,
      " (min_n_interaction = ", x$filters$two_way$min_n_interaction, ")\n\n", sep = "")

  cat("One-way direction: TOP (positive)\n")
  print(x$direction_main_top)
  cat("\nOne-way direction: BOTTOM (negative)\n")
  print(x$direction_main_bottom)

  cat("\n\nTwo-way direction (active-active): TOP (positive)\n")
  print(x$direction_interaction_top)
  cat("\nTwo-way direction (active-active): BOTTOM (negative)\n")
  print(x$direction_interaction_bottom)

  invisible(x)
}
