#' Internal: normalize one-way direction table
#'
#' Expected input columns (current pipeline):
#'   level, shap_active, n, onehot
#' Output columns:
#'   feature, level, shap_active, n
.normalize_direction_main <- function(df) {
  req <- c("level", "shap_active", "n", "onehot")
  miss <- setdiff(req, names(df))
  if (length(miss) > 0) {
    stop("direction_main is missing required columns: ", paste(miss, collapse = ", "))
  }

  # feature = onehot with the level suffix removed (robust to edge cases)
  # default: strip suffix by nchar(level); verify suffix matches to avoid nonsense
  onehot <- as.character(df$onehot)
  level  <- as.character(df$level)

  feature <- rep(NA_character_, length(onehot))
  ok <- !is.na(onehot) & !is.na(level) & endsWith(onehot, level)

  feature[ok] <- substr(onehot[ok], 1, nchar(onehot[ok]) - nchar(level[ok]))
  # If not ok, try a safer heuristic: split at first punctuation/space boundary
  # but keep NA if ambiguous (better than silently wrong)
  # (you can remove this fallback if you prefer strict parsing)
  bad <- which(!ok)
  if (length(bad) > 0) {
    # attempt: feature is up to first capital letter after underscore groups, etc.
    # in your data, most onehot are like "partisanDem." or "gun_ownNot sure"
    # We'll treat the feature as up to the first character of 'level' occurrence.
    for (i in bad) {
      pos <- regexpr(level[i], onehot[i], fixed = TRUE)[1]
      if (!is.na(pos) && pos > 1) feature[i] <- substr(onehot[i], 1, pos - 1)
    }
  }

  out <- df
  out$feature <- feature

  out <- out[, c("feature", "level", "shap_active", "n"), drop = FALSE]
  out
}

#' Internal: normalize two-way direction table
#'
#' Expected input columns (current pipeline):
#'   feat_i, feat_j, group_i, group_j, direction_active, n_active
#' Output columns:
#'   feature_i, level_i, feature_j, level_j, direction_active, n_active
.normalize_direction_int <- function(df) {
  req <- c("feat_i", "feat_j", "group_i", "group_j", "direction_active", "n_active")
  miss <- setdiff(req, names(df))
  if (length(miss) > 0) {
    stop("direction_int is missing required columns: ", paste(miss, collapse = ", "))
  }

  feat_i  <- as.character(df$feat_i)
  feat_j  <- as.character(df$feat_j)
  group_i <- as.character(df$group_i)
  group_j <- as.character(df$group_j)

  # feature_i/j are exactly group_i/j
  feature_i <- group_i
  feature_j <- group_j

  # level_i/j = feat_i/j with group prefix stripped
  # using ^prefix match to be safe
  level_i <- sub(paste0("^", gsub("([\\W])", "\\\\\\1", group_i)), "", feat_i)
  level_j <- sub(paste0("^", gsub("([\\W])", "\\\\\\1", group_j)), "", feat_j)

  out <- df
  out$feature_i <- feature_i
  out$level_i   <- level_i
  out$feature_j <- feature_j
  out$level_j   <- level_j

  out <- out[, c("feature_i", "level_i", "feature_j", "level_j", "direction_active", "n_active"), drop = FALSE]
  out
}

#' Summary method for surveySHAP objects
#'
#' This method assumes your existing code already computes:
#'   strength_main, strength_int, direction_main, direction_int
#' as data.frames.
#'
#' You only need to ensure those objects exist in this function.
summary.surveySHAP <- function(object,
                               top_n = 5,
                               min_n_main = 100,
                               min_n_interaction = 100,
                               ...) {
  # ---- YOUR EXISTING COMPUTATION BLOCK GOES HERE ----
  # The following four must be produced by your current pipeline:
  #
  # strength_main: data.frame with columns group, strength
  # strength_int : data.frame with columns pair, strength
  # direction_main (UNFILTERED): columns level, shap_active, n, onehot
  # direction_int  (UNFILTERED): columns feat_i, feat_j, group_i, group_j,
  #                              direction_active, n_active
  #
  # For example, if your old code already created them, keep it unchanged.
  #
  # strength_main <- ...
  # strength_int  <- ...
  # direction_main <- ...
  # direction_int  <- ...

  if (!exists("strength_main", inherits = FALSE) ||
      !exists("strength_int",  inherits = FALSE) ||
      !exists("direction_main", inherits = FALSE) ||
      !exists("direction_int",  inherits = FALSE)) {
    stop("summary.surveySHAP(): expected strength_main, strength_int, direction_main, direction_int to be created by the existing pipeline.")
  }

  # ---- NEW: normalize COMPLETE direction tables (NO FILTERING) ----
  direction_main_complete <- .normalize_direction_main(direction_main)
  direction_int_complete  <- .normalize_direction_int(direction_int)

  # ---- Filtering ONLY for printing views ----
  kept_main <- direction_main_complete[!is.na(direction_main_complete$n) & direction_main_complete$n >= min_n_main, , drop = FALSE]
  kept_int  <- direction_int_complete[!is.na(direction_int_complete$n_active) & direction_int_complete$n_active >= min_n_interaction, , drop = FALSE]

  # TOP/BOTTOM for printing
  top_main_pos <- kept_main[order(kept_main$shap_active, decreasing = TRUE), , drop = FALSE]
  top_main_pos <- head(top_main_pos[top_main_pos$shap_active > 0, , drop = FALSE], top_n)

  top_main_neg <- kept_main[order(kept_main$shap_active, decreasing = FALSE), , drop = FALSE]
  top_main_neg <- head(top_main_neg[top_main_neg$shap_active < 0, , drop = FALSE], top_n)

  top_int_pos <- kept_int[order(kept_int$direction_active, decreasing = TRUE), , drop = FALSE]
  top_int_pos <- head(top_int_pos[top_int_pos$direction_active > 0, , drop = FALSE], top_n)

  top_int_neg <- kept_int[order(kept_int$direction_active, decreasing = FALSE), , drop = FALSE]
  top_int_neg <- head(top_int_neg[top_int_neg$direction_active < 0, , drop = FALSE], top_n)

  # counts for the "Filters" section should refer to UNFILTERED totals
  filters <- list(
    min_n_main = min_n_main,
    min_n_interaction = min_n_interaction,
    kept_main = nrow(kept_main),
    total_main = nrow(direction_main_complete),
    kept_int = nrow(kept_int),
    total_int = nrow(direction_int_complete)
  )

  out <- list(
    strength_main = strength_main,
    strength_int  = strength_int,

    # COMPLETE (unfiltered) direction tables you can export to CSV
    direction_main = direction_main_complete,
    direction_int  = direction_int_complete,

    # printing views (filtered + truncated)
    direction_main_top_pos = top_main_pos,
    direction_main_top_neg = top_main_neg,
    direction_int_top_pos  = top_int_pos,
    direction_int_top_neg  = top_int_neg,

    filters = filters,
    top_n = top_n
  )

  class(out) <- "surveySHAP_summary"
  out
}

#' Print method for surveySHAP_summary
print.surveySHAP_summary <- function(x, ...) {
  cat("=== surveySHAP summary ===\n\n")

  cat("One-way strength (group):\n")
  print(x$strength_main)
  cat("\n")

  cat("Two-way strength (group pairs):\n")
  print(x$strength_int)
  cat("\n")

  cat("Filters:\n")
  cat(
    sprintf("  One-way direction: kept %d/%d (min_n_main = %d)\n",
            x$filters$kept_main, x$filters$total_main, x$filters$min_n_main)
  )
  cat(
    sprintf("  Two-way direction: kept %d/%d (min_n_interaction = %d)\n\n",
            x$filters$kept_int, x$filters$total_int, x$filters$min_n_interaction)
  )

  cat("One-way direction: TOP (positive)\n")
  print(x$direction_main_top_pos)
  cat("\n")

  cat("One-way direction: BOTTOM (negative)\n")
  print(x$direction_main_top_neg)
  cat("\n\n")

  cat("Two-way direction (active-active): TOP (positive)\n")
  print(x$direction_int_top_pos)
  cat("\n\n")

  cat("Two-way direction (active-active): BOTTOM (negative)\n")
  print(x$direction_int_top_neg)
  cat("\n")

  invisible(x)
}
