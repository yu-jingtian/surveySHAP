#' Internal utility helpers
#'
#' Small helpers used across design, SHAP, summaries, and bootstrap code.
#'
#' @name surveySHAP-internal
#' @keywords internal
NULL

.wmean <- function(x, w) {
  x <- as.numeric(x)
  w <- as.numeric(w)
  ok <- !is.na(x) & !is.na(w) & w > 0
  if (!any(ok)) return(NA_real_)
  sum(x[ok] * w[ok]) / sum(w[ok])
}

.n_eff <- function(w) {
  w <- as.numeric(w)
  ok <- !is.na(w) & w > 0
  if (!any(ok)) return(0)
  sw <- sum(w[ok])
  sw2 <- sum(w[ok]^2)
  if (sw2 <= 0) return(0)
  sw^2 / sw2
}

.normalize_weights <- function(w) {
  if (is.null(w)) return(NULL)
  w <- as.numeric(w)
  ok <- !is.na(w) & w > 0
  w <- w[ok]
  w / mean(w)
}

.default_groups <- function() {
  c("gun_own", "partisan", "race", "gender", "college", "metro")
}

prepare_survey_data <- function(data,
                                y_col = "gun_control",
                                weight_col = "weight",
                                group_vars = .default_groups(),
                                normalize_weights = TRUE) {
  stopifnot(is.data.frame(data))

  if (!"college" %in% names(data)) {
    if (!"educ" %in% names(data)) stop("Missing `educ` needed to derive `college`.")
    data$college <- factor(
      ifelse(data$educ %in% c(1, 2, 3), "Non-college",
             ifelse(data$educ %in% c(4, 5, 6), "College", NA)),
      levels = c("Non-college", "College")
    )
  }

  if (!"metro" %in% names(data)) {
    if (!"rucc" %in% names(data)) stop("Missing `rucc` needed to derive `metro`.")
    data$metro <- factor(
      ifelse(data$rucc == 1, "Big-metro", "Non-metro"),
      levels = c("Big-metro", "Non-metro")
    )
  }

  missing_cols <- setdiff(c(y_col, group_vars), names(data))
  if (length(missing_cols) > 0) {
    stop("Missing columns in `data`: ", paste(missing_cols, collapse = ", "))
  }

  needed <- unique(c(y_col, group_vars, weight_col))
  needed <- needed[needed %in% names(data)]
  df <- data[, needed, drop = FALSE]
  names(df)[names(df) == y_col] <- "y_count"
  df$y_count <- as.numeric(df$y_count)

  for (g in group_vars) {
    df[[g]] <- as.factor(df[[g]])
  }

  cc_cols <- c("y_count", group_vars)
  if (!is.null(weight_col) && weight_col %in% names(df)) {
    cc_cols <- c(cc_cols, weight_col)
  }
  df <- df[stats::complete.cases(df[, cc_cols, drop = FALSE]), , drop = FALSE]
  df <- droplevels(df)

  w <- NULL
  if (!is.null(weight_col) && weight_col %in% names(df)) {
    w <- as.numeric(df[[weight_col]])
    ok <- !is.na(w) & w > 0
    df <- df[ok, , drop = FALSE]
    w <- w[ok]
    if (normalize_weights) w <- w / mean(w)
  }

  list(df = df, w = w, group_vars = group_vars)
}

feature_pair_label <- function(feature1, feature2) {
  ifelse(feature1 <= feature2,
         paste(feature1, feature2, sep = "__"),
         paste(feature2, feature1, sep = "__"))
}

safe_level <- function(x) {
  make.names(as.character(x), unique = FALSE)
}

.make_sum_coded_main <- function(dummy_mat, g, levs) {
  L <- length(levs)
  if (L < 2) {
    stop("Group `", g, "` must have at least two levels for sum-to-zero coding.")
  }
  out <- dummy_mat[, seq_len(L - 1), drop = FALSE] - dummy_mat[, L, drop = FALSE]
  colnames(out) <- paste0(g, "__SC__", safe_level(levs[seq_len(L - 1)]))
  out
}

.reconstruct_main_sum_to_zero <- function(gamma, levs) {
  L <- length(levs)
  beta <- numeric(L)
  names(beta) <- levs
  if (L == 1) return(beta)
  beta[seq_len(L - 1)] <- gamma
  beta[L] <- -sum(gamma)
  beta
}

.reconstruct_interaction_sum_to_zero <- function(gamma_mat, levs1, levs2) {
  L1 <- length(levs1)
  L2 <- length(levs2)
  B <- matrix(0, nrow = L1, ncol = L2, dimnames = list(levs1, levs2))

  if (L1 == 1 || L2 == 1) return(B)

  B[seq_len(L1 - 1), seq_len(L2 - 1)] <- gamma_mat
  B[L1, seq_len(L2 - 1)] <- -colSums(gamma_mat)
  B[seq_len(L1 - 1), L2] <- -rowSums(gamma_mat)
  B[L1, L2] <- sum(gamma_mat)
  B
}