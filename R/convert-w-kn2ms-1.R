#' Converts Wind Speed from Knots to Meters per Second.
#'
#' @param wkn Vector with the wind speed values in knots.
#'
#' @usage convert_w_kn2ms(wkn)
#'
#' @import utils
#'
#' @export
#'
convert_w_kn2ms <- function(wkn) {
  cat("Converting wind speed from knots to meters per second...\n\n")
  wms <- wkn * 0.514
  return(wms)
}

