#' Reads the Digitisations of Clilean Surface Records 1950-1958 performed by
#' FCiências.ID.
#'
#' Reads one text file with the station metadata and twelve text files with the
#' digitisation tables for each month of a station-year. As all the data was
#' typed, performs the quality control of the station metadata and checks for
#' table format errors and typing errors.
#'
#' @details
#' \strong{Input:}
#' \itemize{
#' \item One text file with the metadata - 'StationName_Obs_YYYY.txt'. The
#' following constants are read from the metadata file:
#' \itemize{
#' \item WIGOS compatible station identifier
#' \item Observations year
#' \item Longitude
#' \item Latitude
#' \item Altitude
#' \item 'msl pressure' or 'surface pressure'
#' \item Real station name
#' }
#' \item Twelve text files with the digitisations, which correspond to a
#' digitisation table for each month of a station-year -
#' 'StationName_Mth_YYYY.txt'. The digitisation tables have 13 columns with the
#' following variables:
#' \describe{
#' \item{Column 1}{Day of the observation}
#' \item{Column 2}{Observation time in the format HHMM}
#' \item{Column 3}{Dew point temperature in Celsius degrees (ºC)}
#' \item{Column 4}{Cloud cover in oktas: 0,1,2,3,4,5,6,7,8,9}
#' \item{Column 5}{Wind direction in 16-wind compass rose points, plus C - Calm}
#' \item{Column 6}{Wind speed in knots (kn, kt)}
#' \item{Column 7}{Atmospheric pressure - mean sea level or surface - in
#' hectopascals (hPa)}
#' \item{Column 8}{Air temperature in Celsius degrees (ºC)}
#' \item{Column 9}{Acumulated precipitation in milimeters (mm) most commonly
#' measured at 12:00 UTC}
#' \item{Column 10}{Acumulated precipitation in milimeters (mm) most commonly
#' measured at 23:00 UTC}
#' \item{Column 11}{Minimum daily temperature in Celsius degrees (ºC) most
#' commonly measured at 12:00 UTC}
#' \item{Column 12}{Maximum daily temperature in Celsius degrees (ºC) most
#' commonly measured at 23:00 UTC}
#' \item{Column 13}{Relative humidity in percent}
#' }
#' }
#' \strong{Output:}
#' \itemize{
#' \item A .RData and a .txt file with the metadata bellonging to a
#' station-year: 'metadata_StationName_Year'.
#' \item A .RData and a .txt file with the anual digitisation of station-year:
#' 'digitisation_StationName_Year'.
#' \item Eventually, one or two .txt files with typing errors to be corrected
#' before proceeding with the quality control:
#' 'Hour-errors_StationName_YYYY.txt' and 'Typing-errors_StationName_YYYY.txt'.
#' }
#'
#' @param obstxt Character string giving the path of the metadata file.
#'
#' @return A data frame with seventeen columns: WIGOS compatible station ID, day
#'   of the year, year, month, day, hour, dew point temperature, cloud cover,
#'   wind direction, wind speed, air pressure, air temperature, accumulated
#'   precipitation at first hour, accumulated precipitation at second hour,
#'   daily minimum temperature, daily maximum temperature, relative humidity.
#'
#' @usage read_chl_ffcul(obstxt)
#'
#' @import utils
#'
#' @export
#'
read_chl_ffcul <- function(obstxt = file.choose()) {
  # Reads the file with the observations/comments/metadata
  cat("'StationName_Obs_YYYY.txt' file?\n")
  fpth <- dirname(obstxt)
  obs <- read.table(file = obstxt, header = FALSE, sep = "\t", dec = ".",
    quote = "", nrows = 8, stringsAsFactors = FALSE)
  print(obs)
  # WIGOS compatible station identifier
  sta <- as.character(obs[1, 1])
  # Year
  year <- as.integer(obs[2, 1])
  # Longitude in decimal degrees
  lon <- as.double(obs[4, 1])
  # Latitude in decimal degrees
  lat <- as.double(obs[5, 1])
  # Altitude in meters
  alt <- as.double(obs[6, 1])
  # Atmospheric pressure: msl or surface
  plv <- as.character(obs[7, 1])
  # Real station name
  clsta <- as.character(obs[8, 1])
  cat("\n")
  cat("Testing metadata...\n")
  # Tests year
  if (year < 1950 || year > 1958) {
    cat("The year is not valid for this record.\n")
    return(year)
  }
  # Tests coordinates
  if (lon < -180 || lon > 180) {
    cat("The longitude value is out of bounds.\n")
    return(lon)
  }
  if (lat < -90 || lat > 90) {
    cat("The latitude value is out of bounds.\n")
    return(lat)
  }
  # Tests variable name for pressure
  if (!(plv == "msl pressure" || plv == "surface pressure")) {
    cat("The variable name for pressure is wrong.\n")
    return(plv)
  }
  # Creates the data frame with the metadata
  meta <- data.frame(sta, clsta, lat, lon, alt, plv,
    stringsAsFactors = FALSE)
  # str(meta)
  # Saves the metadata dataframe as .RData and .txt
  meta_out <- paste("metadata", sta, as.character(year), sep = "_")
  if (!file.exists(sta)) {
    dir.create(sta)
  }
  write.table(meta, file = paste(sta, "/", meta_out, ".txt", sep = ""),
    row.names = FALSE, col.names = TRUE, sep = "\t", quote = FALSE )
  save(meta, file = paste(paste(sta, "/", meta_out, ".RData", sep = "")))
  # Tests if is leap year
  if (year%%4 == 0) {
    # Vector with the days of the year
    dayr <- 1:366
    # Vector with the days of the year repeated 4 times each
    dayr4 <- rep(dayr, each = 4)
    # Vector with the number of the month (1:12) repeated the number of days of
    # the month {31, 28/29, 31, 30, ..., 31}
    month <- c(rep(1, times = 31), rep(2, times = 29), rep(3, times = 31),
      rep(4, times = 30), rep(5, times = 31), rep(6, times = 30),
      rep(7, times = 31), rep(8, times = 31), rep(9, times = 30),
      rep(10, times = 31), rep(11, times = 30), rep(12, times = 31))
    # Vector with the number of the month repeated 4 times a day
    month4 <- rep(month, each = 4)
    # Creates a vector with the number of the day of the month (repeated 4
    # times) to compare with the digitised value
    aday <- c(rep(1:31, each = 4), rep(1:29, each = 4), rep(1:31, each = 4),
      rep(1:30, each = 4), rep(1:31, each = 4), rep(1:30, each = 4),
      rep(1:31, each = 4), rep(1:31, each = 4), rep(1:30, each = 4),
      rep(1:31, each = 4), rep(1:30, each = 4), rep(1:31, each = 4))
    # length(aday)
  } else {
    dayr <- 1:365
    dayr4 <- rep(dayr, each = 4)
    month <- c(rep(1, times = 31), rep(2, times = 28), rep(3, times = 31),
      rep(4, times = 30), rep(5, times = 31), rep(6, times = 30),
      rep(7, times = 31), rep(8, times = 31), rep(9, times = 30),
      rep(10, times = 31), rep(11, times = 30), rep(12, times = 31))
    month4 <- rep(month, each = 4)
    aday <- c(rep(1:31, each = 4), rep(1:28, each = 4), rep(1:31, each = 4),
      rep(1:30, each = 4), rep(1:31, each = 4), rep(1:30, each = 4),
      rep(1:31, each = 4), rep(1:31, each = 4), rep(1:30, each = 4),
      rep(1:31, each = 4), rep(1:30, each = 4), rep(1:31, each = 4))
    # length(aday)
  }
  # Constans: station name and year
  # New variables: day of the year and number of the month
  # Sets new variables type
  st <- rep(sta, each = length(dayr4))
  yr <- rep(year, each = length(dayr4))
  dayr4 <- as.integer(dayr4)
  month4 <- as.integer(month4)
  # Concatenates to obtain the name of the 12 input text files with the montly
  # digitisations
  txt_n <- paste(sta, month.abb, as.character(year), sep =  "_")
  txt_ne <- paste(txt_n, ".txt", sep = "")
  # Creates a data frame to receive the input data
  tb13 <- data.frame()
  cat("\n")
  cat("Reading montly digitisations...\n\n")
  for (i in 1:12) {
    # Reads the 12 input .txt files with the montly digitisations to 12
    # dataframes
    dfin <- paste(fpth, "/", txt_ne[i], sep = "")
    dfi <- read.table(dfin, header = FALSE, sep = "\t", quote = "", skip = 6,
      stringsAsFactors = FALSE,
      col.names = c("day", "hour", "tdw", "cloud", "wdir", "wsp", "ppa", "tta",
        "rrr1", "rrr2", "tmin", "tmax", "rhum"))
    # Creates a data frame with 13 columns and all the data per station-year
    tb13 <- rbind(tb13, dfi)
  }
  # Sets digitised variables type
  # The coercion brings problems (NA) when there is typing errors
  # Tested bellow
  day <- as.integer(tb13$day)
  # Tested bellow
  hour <- as.character(tb13$hour)
  # Each of the following has standard tests that will be applied in the QC
  tdw <- as.double(tb13$tdw)
  cloud <- as.double(tb13$cloud)
  wdir <- as.character(tb13$wdir)
  wsp <- as.double(tb13$wsp)
  ppa <- as.double(tb13$ppa)
  tta <- as.double(tb13$tta)
  rrr1 <- as.double(tb13$rrr1)
  rrr2 <- as.double(tb13$rrr2)
  tmin <- as.double(tb13$tmin)
  tmax <- as.double(tb13$tmax)
  rhum <- as.double(tb13$rhum)
  # Data frame with 17 columns containing the digitisations
  digt <- data.frame(station = st, dayr = dayr4, year = yr, month = month4,
    day, hour, tdw, cloud, wdir, wsp, ppa, tta, rrr1, rrr2, tmin, tmax, rhum,
    stringsAsFactors = FALSE)
  cat("Testing 'month', 'day' and 'hour' fields for errors\n")
  cat("(table format errors)...\n\n")
  # Solves typing errors in the day, i.e., missing values (NA) or invalid values
  if (!identical(digt$day, aday)) {
    digt$day <- aday
  }
  # Tests the hour "hhmm"
  # Hours from 1 to 24
  vhh <- c("01", "02", "03", "04", "05", "06", "07", "08", "09",
    as.character (10:24))
  # Minuts from 0 to 59
  vmm <- c("00", "01", "02", "03", "04", "05", "06", "07", "08", "09",
    as.character (10:59))
  hh <- c()
  mm <- c()
  digt_herr <- data.frame()
  for (j in 1:nrow(digt)) {
    hh[j] <- substr(digt$hour[j], start = 1, stop = 2)
    mm[j] <- substr(digt$hour[j], start = 3, stop = 4)
    if (!(hh[j] %in% vhh) || !(mm[j] %in% vmm)) {
      digt_herr <- rbind(digt_herr, digt[j, ])
    }
  }
  if (nrow(digt_herr != 0)) {
    cat("There are invalid characters for the hours.\n")
    cat("Check 'Hour-errors_StationName_YYYY.txt'.\n\n")
    herr <- paste("Hour-errors", sta, as.character(year), sep =  "_")
    write.table(digt_herr, file = paste(herr, ".txt", sep = ""),
      row.names = FALSE, col.names = TRUE, sep = "\t", quote = FALSE)
  }
  # Tests for NA which correspond to typing errors
  cat("Searching for NA which correspond to typing errors...\n\n")
  if (anyNA(digt)) {
    hour_na <- digt[is.na(digt$hour), ]
    tdw_na <- digt[is.na(digt$tdw), ]
    cloud_na <- digt[is.na(digt$cloud), ]
    wdir_na <- digt[is.na(digt$wdir), ]
    wsp_na <- digt[is.na(digt$wsp), ]
    ppa_na <- digt[is.na(digt$ppa), ]
    tta_na <- digt[is.na(digt$tta), ]
    rrr1_na <- digt[is.na(digt$rrr1), ]
    rrr2_na <- digt[is.na(digt$rrr2), ]
    tmin_na <- digt[is.na(digt$tmin), ]
    tmax_na <- digt[is.na(digt$tmax), ]
    rhum_na <- digt[is.na(digt$rhum), ]
    # Data frame with the NA: typing errors to fix
    digt_na <- rbind(hour_na, tdw_na, cloud_na, wdir_na, wsp_na, ppa_na, tta_na,
      rrr1_na, rrr2_na, tmin_na, tmax_na, rhum_na)
    # Replaces NA for missing values code -999
    digt[is.na(digt)] <- -999
    cat("NA values resulting from typing errors were originated.\n")
    cat("Those values were replaced by the missing value code '-999'.\n")
    cat("Check 'Typing-errors_StationName_YYYY.txt'.\n\n")
    terr <- paste("Typing-errors", sta, as.character(year), sep =  "_")
    write.table(digt_na, file = paste(terr, ".txt", sep = ""),
      row.names = FALSE, col.names = TRUE, sep = "\t", quote = FALSE)
  }
  # anyNA(digt)
  # str(digt)
  # Saves the digitisations data frame as .RData and .txt
  digt_out <- paste("digitisation", sta, as.character(year), sep = "_")
  write.table(digt, file = paste(sta, "/", digt_out, ".txt", sep = ""),
    row.names = FALSE, col.names = TRUE, sep = "\t", quote = FALSE)

  save(digt, file = paste(sta, "/", digt_out, ".RData", sep = ""))
  cat("Outputs of read_chl_ffcul() in the folder 'StationName':\n")
  cat("'metadata_StationName_YYYY' and 'digitisation_StationName_YYYY'\n\n")
  return(digt)
}
