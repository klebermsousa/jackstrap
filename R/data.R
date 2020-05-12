# This goes in R/data.R

#' @title Dataset of Brazilian Municipalities
#' @description Dataset of Brazilian Municipalities used in Sousa & Stosic (2005)
#' @format A data frame with 489 rows (DMUs) and 3 variables (2 outputs and 1 inputs):
#' \describe{
#'   \item{\code{Description of DMUs}}{string variable with descriptions of the each DMUs}
#'   \item{\code{Code}}{integer variable identifies each DMU for integer code}
#'   \item{\code{y1}}{float variable with quantity of first produced output with resources}
#'   \item{\code{y2}}{float variable with quantity of second produced output with resources}
#'   \item{\code{x1}}{float variable with first input (resource) used to produce outpus (y)}
#'}
"municipalities"
