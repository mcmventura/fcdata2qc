#' Converts Wind Direction from 16-wind Compass Rose Points to Degrees.
#'
#' @param dd16wcr Vector with the wind direction values in 16-wind compass rose
#'   points.
#'
#' @usage convert_dd_16wcr2deg(dd16wcr)
#'
#' @import utils
#'
#' @export
#'
convert_dd_16wcr2deg <- function(dd16wcr) {
  cat("Converting wind direction from 16-wind compass rose points\n")
  cat("to degrees...\n\n")
  dddeg <- c()
  crose <- c(C = 0, NNE = 20, NE = 45, ENE = 70, E = 90, ESE = 110, SE = 135,
    SSE = 160, S = 180, SSW = 200, SW = 225, WSW = 250, W = 270, WNW = 290,
    NW = 315, NNW = 340, N = 360)
  for (i in 1:length(dd16wcr)) {
    if (dd16wcr[i] %in% names(crose)) {
      dddeg[i] <- crose[dd16wcr[i]]
    } else if (is.na(dd16wcr[i])) {
      dddeg[i] <- NA
    } else {
      # The value "999" will throw an error when testing wind speed (0, 360)
      # "999" to say that in the original units is wrong
      # A data frame in the C3S-QC format with the original values is saved
      # before doing this
      dddeg[i] <- 999
    }
  }
  return(dddeg)
}
