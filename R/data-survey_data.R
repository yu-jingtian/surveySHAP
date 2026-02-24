#' Processed Survey Data for SHAP Demonstration
#'
#' A processed survey dataset used to demonstrate SHAP-based interpretation
#' methods for survey models.
#'
#' @format A data frame with 43,950 rows and 8 variables:
#' \describe{
#'   \item{gun_control}{Count of restrictive gun policy responses (0–6).
#'     Constructed as the sum of six binary survey items, where 1 indicates
#'     support for stricter regulation and 0 indicates looser regulation.
#'     Larger values indicate more restrictive policy preferences.}
#'   \item{gun_own}{Gun ownership status of household.}
#'   \item{partisan}{Partisan identification (Dem., Rep., Ind.).}
#'   \item{race}{Self-reported race.}
#'   \item{gender}{Gender identity (Men, Woman, Other).}
#'   \item{educ}{Highest level of education completed (single selection): \cr
#'     \code{[1]} Did not graduate from high school; \cr
#'     \code{[2]} High school graduate; \cr
#'     \code{[3]} Some college, but no degree (yet); \cr
#'     \code{[4]} 2-year college degree; \cr
#'     \code{[5]} 4-year college degree; \cr
#'     \code{[6]} Postgraduate degree (MA, MBA, MD, JD, PhD, etc.).}
#'   \item{rucc}{Rural–Urban Continuum Code (RUCC), a USDA-developed
#'     9-point county-level classification ranging from 1 (most urban)
#'     to 9 (most rural).}
#'   \item{weight}{Survey weight constructed from \code{commonweight}.
#'     The survey codebook recommends use of \code{commonweight} when
#'     characterizing opinions and behaviors of adult Americans.}
#' }
#'
#' @details
#' The response variable \code{gun_control} is derived from six survey items.
#' Responses are recoded so that 1 indicates support for stricter regulation
#' and 0 indicates looser regulation. The variable equals the row-wise sum,
#' resulting in an integer outcome between 0 and 6.
#'
#' Rows with missing values across included variables are removed,
#' resulting in 43,950 complete cases.
#'
#' @source
#' Processed from publicly available survey data.
"survey_data"