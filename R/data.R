# This goes in R/data.R

#' @title Dataset of Municipalities of Bahia state in Brazil
#' @description Dataset of Municipalities of Bahia state in Brazil
#' @format A data frame with 489 rows (DMUs) and 3 variables (2 outputs and 1 inputs):
#' \describe{
#'   \item{\code{municipio}}{string variable with descriptions of the each local governments}
#'   \item{\code{cod}}{integer variable identifies each DMU for integer code}
#'   \item{\code{total_atend_amb_hosp_ab}}{float variable with public health services in local governments (output)}
#'   \item{\code{total_diversid}}{float variable with diversity of public services provide in local governments (output)}
#'   \item{\code{desp_saude}}{float variable with public service expeditures in local governments (input)}
#'}
#'@examples
#'  \donttest{
#'     #Load data exemple
#'     municipalities <- jackstrap::municipalities
#'  }
"municipalities"
