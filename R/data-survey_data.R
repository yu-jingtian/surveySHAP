#' Processed Survey Data for SHAP Demonstration
#'
#' A processed survey dataset used to demonstrate SHAP-based interpretation
#' methods for survey models.
#'
#' @format A data frame with 43,950 rows and variables including:
#' \describe{
#'   \item{gun_control}{Count of restrictive gun policy responses (0--6).}
#'   \item{gun_own}{Gun ownership status of household.}
#'   \item{partisan}{Partisan identification.}
#'   \item{race}{Self-reported race.}
#'   \item{gender}{Gender identity.}
#'   \item{educ}{Highest level of education completed.}
#'   \item{rucc}{Rural--Urban Continuum Code.}
#'   \item{weight}{Survey weight constructed from \code{commonweight}.}
#' }
#'
#' @details
#' The package derives two additional model groups from the raw variables:
#' \code{college} from \code{educ}, and \code{metro} from \code{rucc}.
#'
#' @source Processed from publicly available survey data.
"survey_data"
