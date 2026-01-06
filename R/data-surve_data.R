#' Processed Survey Data for SHAP Demonstration
#'
#' A processed survey dataset used to demonstrate SHAP-based interpretation
#' methods for survey models.
#'
#' @format A data frame with 60,000 rows and 6 variables:
#' \describe{
#'   \item{gun_control}{Numeric policy preference score (higher = more restrictive).}
#'   \item{gun_own}{Gun ownership status of household.}
#'   \item{partisan}{Partisan identification (Dem., Rep., Ind.).}
#'   \item{race}{Self-reported race.}
#'   \item{gender}{Gender identity (Men, Woman, Other).}
#'   \item{college}{College education status.}
#' }
#'
#' @source
#' Processed from publicly available survey data.
"survey_data"
