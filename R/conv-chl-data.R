#' Converts the Clilean Surface Records 1950-1958 to C3S-QC Format.
#'
#' @usage conv_chl_data()
#'
#' @export
#'
conv_chl_data <- function() {
  anual_digt <- read_chl_ffcul()
  st_name <- form_chl2qc(digt = anual_digt)
  # build_series(station = st_name)
}
