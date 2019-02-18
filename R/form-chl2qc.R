#' Converts to WP3 C3S-QC Format the Digitisations of Chilean Surface Records
#' 1950-1958 performed by FCiências.ID.
#'
#' Splits the annual digitisations per station into several data frames by type
#' of meteorological variable. The output data frames and text files are in the
#' WP3 C3S-QC format: \strong{variable code | year (YYYY) | month (MM) | day
#' (DD) | time (HHMM) | observation}.
#'
#' @details
#' \strong{Input:}
#' \itemize{
#' \item The output data frame of \code{\link{read_chl_ffcul}} with seventeen
#' columns: WIGOS compatible station ID, day of the year, year, month, day,
#' hour, dew point temperature, cloud cover, wind direction, wind speed, air
#' pressure, air temperature, accumulated precipitation at first hour,
#' accumulated precipitation at second hour, daily minimum temperature, daily
#' maximum temperature, relative humidity.
#' }
#' \strong{Output:}
#' \itemize{
#' \item A .RData and a .txt file without header for each one of the ten
#' variables digitised (td, n, dd, w, p, ta, rr, Tn, Tx, rh) -
#' 'VariableCode_StationName_Year'. If misses the output for some variable it's
#' because doesn't exist any observations of that variable in the record.
#' \item A .txt file with the wind speed and another with the wind direction in
#' the original units - 'VariableCode_StationName_Year_16wcr' and
#' 'VariableCode_StationName_Year_kt'.
#' \item A .txt file for each one of the ten variables digitised which includes
#' the missing values in the anual series - 'VariableCode_StationName_Year_all'.
#' }
#'
#' @param digt A data frame with the following seventeen columns:
#'   \strong{station | dayr | year | month | day | hour | tdw | cloud | wdir
#'   | wsp | ppa | tta | rrr1 | rrr2 | tmin | tmax | rhum}.
#'
#' @usage form_chl2qc(digt)
#'
#' @import utils
#'
#' @export
#'
form_chl2qc <- function(digt) {
digt[digt == -999] <- NA
#station <- digt[1, 1]
station <- unique(digt[[1]])
year <- unique(digt[[3]])
digt$station <- NULL
cat("\n")
cat("Converting to C3S-QC format...\n\n")
# DEW POINT
# Checks if all the values are missing
if (sum(is.na(digt$tdw)) < nrow(digt)) {
  subda_td <- digt[c(2:5, 6)]
  # Or this way
  # subd_td <- digt[c("month", "day", "hour", "tdw")]
  # Creates the column with the variable code
  subda_td$vcod <- c(rep("td", nrow(subda_td)))
  # Defines the standard order of the columns in the data frame
  subda_td <- subda_td[c("vcod", "year", "month", "day", "hour", "tdw")]
  # Creates directory for td
  td_fol <- paste("td_", station, sep = "")
  if (!dir.exists(td_fol)) {
    dir.create(td_fol)
  }
  # Saves with the NA - good for plotting
  fna_td <- paste("td", station, year, "all", sep = "_")
  write.table(subda_td, file = paste(td_fol, "/", fna_td, ".txt", sep = ""),
    row.names = FALSE, col.names = FALSE, quote = FALSE, sep = "\t")
  # Saves without the NA
  subd_td <- subda_td[!is.na(subda_td$tdw), ]
  fn_td <- paste("td", station, year, sep = "_")
  write.table(subd_td, file = paste(td_fol, "/", fn_td, ".txt", sep = ""),
    row.names = FALSE, col.names = FALSE, quote = FALSE, sep = "\t")
  save(subd_td, file = paste(td_fol, "/", fn_td, ".RData", sep = ""))
  saveRDS(subd_td, file = paste(td_fol, "/", fn_td, ".rds", sep = ""))
}
# CLOUD COVER
if (sum(is.na(digt$cloud)) < nrow(digt)) {
  subda_n <- digt[c(2:5, 7)]
  subda_n$vcod <- c(rep("n", nrow(subda_n)))
  subda_n <- subda_n[c("vcod", "year", "month", "day", "hour", "cloud")]
  n_fol <- paste("n_", station, sep = "")
  if (!dir.exists(n_fol)) {
    dir.create(n_fol)
  }
  fna_n <- paste("n", station, year, "all", sep = "_")
  write.table(subda_n, file = paste(n_fol, "/", fna_n, ".txt", sep = ""),
    row.names = FALSE, col.names = FALSE, quote = FALSE, sep = "\t")
  subd_n <- subda_n[!is.na(subda_n$cloud), ]
  fn_n <- paste("n", station, year, sep = "_")
  write.table(subd_n, file = paste(n_fol, "/", fn_n, ".txt", sep = ""),
    row.names = FALSE, col.names = FALSE, quote = FALSE, sep = "\t")
  save(subd_n, file = paste(n_fol, "/", fn_n, ".RData", sep = ""))
  saveRDS(subd_n, file = paste(n_fol, "/", fn_n, ".rds", sep = ""))
}
# WIND DIRECTION
if (sum(is.na(digt$wdir)) < nrow(digt)) {
  subda_dd <- digt[c(2:5, 8)]
  subda_dd$vcod <- c(rep("dd", nrow(subda_dd)))
  subda_dd <- subda_dd[c("vcod", "year", "month", "day", "hour", "wdir")]
  dd_fol <- paste("dd_", station, sep = "")
  if (!dir.exists(dd_fol)) {
    dir.create(dd_fol)
  }
  fna_dd <- paste("dd", station, year, "all", sep = "_")
  write.table(subda_dd,
    file = paste(dd_fol, "/", fna_dd, "_16wcr", ".txt", sep = ""),
    row.names = FALSE, col.names = FALSE, quote = FALSE, sep = "\t")
  # Converts from 16-wind compass rose points to degrees
  dd16wcr <- subda_dd$wdir
  dddeg <- convert_dd_16wcr2deg(dd16wcr = dd16wcr)
  subda_dd_deg <- subda_dd
  subda_dd_deg$wdir <- dddeg
  fna_dd_deg <- paste("dd", station, year, "all", sep = "_")
  write.table(subda_dd_deg,
    file = paste(dd_fol, "/", fna_dd_deg, ".txt", sep = ""),
    row.names = FALSE, col.names = FALSE, quote = FALSE, sep = "\t")
  subd_dd_deg <- subda_dd_deg[!is.na(subda_dd_deg$wdir), ]
  fn_dd_deg <- paste("dd", station, year, sep = "_")
  write.table(subd_dd_deg,
    file = paste(dd_fol, "/", fn_dd_deg, ".txt", sep = ""),
    row.names = FALSE, col.names = FALSE, quote = FALSE, sep = "\t")
  save(subd_dd_deg, file = paste(dd_fol, "/", fn_dd_deg, ".RData", sep = ""))
  saveRDS(subd_dd_deg, file = paste(dd_fol, "/", fn_dd_deg, ".rds", sep = ""))
}
# WIND SPEED
if (sum(is.na(digt$wsp)) < nrow(digt)) {
  subda_w <- digt[c(2:5, 9)]
  subda_w$vcod <- c(rep("w", nrow(subda_w)))
  subda_w <- subda_w[c("vcod", "year", "month", "day", "hour", "wsp")]
  w_fol <- paste("w_", station, sep = "")
  if (!dir.exists(w_fol)) {
    dir.create(w_fol)
  }
  fna_w <- paste("w", station, year, "all", sep = "_")
  write.table(subda_w, file = paste(w_fol, "/", fna_w, "_kt", ".txt", sep = ""),
    row.names = FALSE, col.names = FALSE, quote = FALSE, sep = "\t")
  # Converts from knots to meters/second
  wkn <- subda_w$wsp
  wms <- convert_w_kn2ms(wkn = wkn)
  subda_w_ms <- subda_w
  subda_w_ms$wsp <- wms
  fna_w_ms <- paste("w", station, year, "all", sep = "_")
  write.table(subda_w_ms, file = paste(w_fol, "/", fna_w_ms, ".txt", sep = ""),
    row.names = FALSE, col.names = FALSE, quote = FALSE, sep = "\t")
  subd_w_ms <- subda_w_ms[!is.na(subda_w_ms$wsp), ]
  fn_w_ms <- paste("w", station, year, sep = "_")
  write.table(subd_w_ms, file = paste(w_fol, "/", fn_w_ms, ".txt", sep = ""),
    row.names = FALSE, col.names = FALSE, quote = FALSE, sep = "\t")
  save(subd_w_ms, file = paste(w_fol, "/", fn_w_ms, ".RData", sep = ""))
  saveRDS(subd_w_ms, file = paste(w_fol, "/", fn_w_ms, ".rds", sep = ""))
}
# AIR PRESSURE
if (sum(is.na(digt$ppa)) < nrow(digt)) {
  subda_p <- digt[c(2:5, 10)]
  subda_p$vcod <- c(rep("p", nrow(subda_p)))
  subda_p <- subda_p[c("vcod", "year", "month", "day", "hour", "ppa")]
  p_fol <- paste("p_", station, sep = "")
  if (!dir.exists(p_fol)) {
    dir.create(p_fol)
  }
  fna_p <- paste("p", station, year, "all", sep = "_")
  write.table(subda_p, file = paste(p_fol, "/", fna_p, ".txt", sep = ""),
    row.names = FALSE, col.names = FALSE, quote = FALSE, sep = "\t")
  subd_p <- subda_p[!is.na(subda_p$ppa), ]
  fn_p <- paste("p", station, year, sep = "_")
  write.table(subd_p, file = paste(p_fol, "/", fn_p, ".txt", sep = ""),
    row.names = FALSE, col.names = FALSE, quote = FALSE, sep = "\t")
  save(subd_p, file = paste(p_fol, "/", fn_p, ".RData", sep = ""))
  saveRDS(subd_p, file = paste(p_fol, "/", fn_p, ".rds", sep = ""))
}
# AIR TEMPERATURE
if (sum(is.na(digt$tta)) < nrow(digt)) {
  subda_ta <- digt[c(2:5, 11)]
  subda_ta$vcod <- c(rep("ta", nrow(subda_ta)))
  subda_ta <- subda_ta[c("vcod", "year", "month", "day", "hour", "tta")]
  ta_fol <- paste("ta_", station, sep = "")
  if (!dir.exists(ta_fol)) {
    dir.create(ta_fol)
  }
  fna_ta <- paste("ta", station, year, "all", sep = "_")
  write.table(subda_ta, file = paste(ta_fol, "/", fna_ta, ".txt", sep = ""),
    row.names = FALSE, col.names = FALSE, quote = FALSE, sep = "\t")
  subd_ta <- subda_ta[!is.na(subda_ta$tta), ]
  fn_ta <- paste("ta", station, year, sep = "_")
  write.table(subd_ta, file = paste(ta_fol, "/", fn_ta, ".txt", sep = ""),
    row.names = FALSE, col.names = FALSE, quote = FALSE, sep = "\t")
  save(subd_ta, file = paste(ta_fol, "/", fn_ta, ".RData", sep = ""))
  saveRDS(subd_ta, file = paste(ta_fol, "/", fn_ta, ".rds", sep = ""))
}
# ACCUMULATED PRECIPITATION
subda_rr <- data.frame()
subd_rr <- data.frame()
# Measured most commonly at 12:00 (not always)
if (sum(is.na(digt$rrr1)) < nrow(digt)) {
  subd_rr1 <- digt[c(2:5, 12)]
  subd_rr1$vcod <- c(rep("rr", nrow(subd_rr1)))
  subd_rr1 <- subd_rr1[c("vcod", "year", "month", "day", "hour", "rrr1")]
  rr_fol <- paste("rr_", station, sep = "")
  if (!dir.exists(rr_fol)) {
    dir.create(rr_fol)
  }
  # Output that keeps the NA
  # Subsets hour by hour, until de 3rd hour
  # Usually the value in on the 1st hour of the time resolution (4)
  subd_rr1_h1 <- subd_rr1[seq(1, nrow(subd_rr1), 4), ]
  rr1_h1_na <- sum(is.na(subd_rr1_h1$rrr1))
  subd_rr1_h2 <- subd_rr1[seq(2, nrow(subd_rr1), 4), ]
  rr1_h2_na <- sum(is.na(subd_rr1_h2$rrr1))
  subd_rr1_h3 <- subd_rr1[seq(3, nrow(subd_rr1), 4), ]
  rr1_h3_na <- sum(is.na(subd_rr1_h3$rrr1))
  # Subsets the hour for which the values aren't all NA
  if (rr1_h1_na < nrow(digt) / 4) {
    names(subd_rr1_h1)[6] <- "rrr"
    subda_rr <- rbind(subda_rr, subd_rr1_h1)
  } else if (rr1_h2_na < nrow(digt) / 4) {
    names(subd_rr1_h2)[6] <- "rrr"
    subda_rr <- rbind(subda_rr, subd_rr1_h2)
  } else if (rr1_h3_na < nrow(digt) / 4) {
    names(subd_rr1_h3)[6] <- "rrr"
    subda_rr <- rbind(subda_rr, subd_rr1_h3)
  }
  # Output that doesn't keep the NA
  # Selects the lines, from column 12, for which precipitation isn't NA
  subdv_rr1 <- subd_rr1[!(is.na(subd_rr1$rrr1)), ]
  names(subdv_rr1)[6] <- "rrr"
  subd_rr <- rbind(subd_rr, subdv_rr1)
}
# Measured most commonly at 23:00 (not always)
if (sum(is.na(digt$rrr2)) < nrow(digt)) {
  subd_rr2 <- digt[c(2:5, 13)]
  subd_rr2$vcod <- c(rep("rr", nrow(subd_rr2)))
  subd_rr2 <- subd_rr2[c("vcod", "year", "month", "day", "hour", "rrr2")]
  rr_fol <- paste("rr_", station, sep = "")
  if (!dir.exists(rr_fol)) {
    dir.create(rr_fol)
  }
  # Output that keeps the NA
  # Subsets hour by hour, starting on the 2nd hour
  # Usually the value in on the 4th hour of the time resolution (4)
  subd_rr2_h2 <- subd_rr2[seq(2, nrow(subd_rr2), 4), ]
  rr2_h2_na <- sum(is.na(subd_rr2_h2$rrr2))
  subd_rr2_h3 <- subd_rr2[seq(3, nrow(subd_rr2), 4), ]
  rr2_h3_na <- sum(is.na(subd_rr2_h3$rrr2))
  subd_rr2_h4 <- subd_rr2[seq(4, nrow(subd_rr2), 4), ]
  rr2_h4_na <- sum(is.na(subd_rr2_h4$rrr2))
  if (rr2_h2_na < nrow(digt) / 4) {
    names(subd_rr2_h2)[6] <- "rrr"
    subda_rr <- rbind(subda_rr, subd_rr2_h2)
  } else if (rr2_h3_na < nrow(digt) / 4) {
    names(subd_rr2_h3)[6] <- "rrr"
    subda_rr <- rbind(subda_rr, subd_rr2_h3)
  } else if (rr2_h4_na < nrow(digt) / 4) {
    names(subd_rr2_h4)[6] <- "rrr"
    subda_rr <- rbind(subda_rr, subd_rr2_h4)
  }
  # Output that doesn't keep the NA
  # Selects the lines, from column 13, for which precipitation isn't NA
  subdv_rr2 <- subd_rr2[!(is.na(subd_rr2$rrr2)), ]
  names(subdv_rr2)[6] <- "rrr"
  subd_rr <- rbind(subd_rr, subdv_rr2)
}
if (nrow(subda_rr) != 0) {
  # Orders by day
  subda_rr <- subda_rr[order(subda_rr[, 4]), ]
  # Then orders by month
  subda_rr <- subda_rr[order(subda_rr[, 3]), ]
  fna_rr <- paste("rr", station, year, "all", sep = "_")
  write.table(subda_rr, file = paste(rr_fol, "/", fna_rr, ".txt", sep = ""),
    row.names = FALSE, col.names = FALSE, quote = FALSE, sep = "\t")
}
if (nrow(subd_rr) != 0) {
  subd_rr <- subd_rr[order(subd_rr[, 4]), ]
  subd_rr <- subd_rr[order(subd_rr[, 3]), ]
  fn_rr <- paste("rr", station, year, sep = "_")
  write.table(subd_rr, file = paste(rr_fol, "/", fn_rr, ".txt", sep = ""),
    row.names = FALSE, col.names = FALSE, quote = FALSE, sep = "\t")
  save(subd_rr, file = paste(rr_fol, "/", fn_rr, ".RData", sep = ""))
  saveRDS(subd_rr, file = paste(rr_fol, "/", fn_rr, ".rds", sep = ""))
}
# DAILY MINIMUM AIR TEMPERATURE
# Measured most commonly at 12:00 (not always)
if (sum(is.na(digt$tmin)) < nrow(digt)) {
  subd_tn <- digt[c(2:5, 14)]
  subd_tn$vcod <- c(rep("Tn", nrow(subd_tn)))
  subd_tn <- subd_tn[c("vcod", "year", "month", "day", "hour", "tmin")]
  tn_fol <- paste("tn_", station, sep = "")
  if (!dir.exists(tn_fol)) {
    dir.create(tn_fol)
  }
  # Output that keeps the NA
  # Subsets hour by hour, until de 3rd hour
  # Usually the value in on the 1st hour of the time resolution (4)
  subda_tn <- data.frame()
  subd_tn_h1 <- subd_tn[seq(1, nrow(subd_tn), 4), ]
  tn_h1_na <- sum(is.na(subd_tn_h1$tmin))
  subd_tn_h2 <- subd_tn[seq(2, nrow(subd_tn), 4), ]
  tn_h2_na <- sum(is.na(subd_tn_h2$tmin))
  subd_tn_h3 <- subd_tn[seq(3, nrow(subd_tn), 4), ]
  tn_h3_na <- sum(is.na(subd_tn_h3$tmin))
  # Subsets the hour for which the values aren't all NA
  if (tn_h1_na < nrow(digt) / 4) {
  subda_tn <- rbind(subda_tn, subd_tn_h1)
  } else if (tn_h2_na < nrow(digt) / 4) {
  subda_tn <- rbind(subda_tn, subd_tn_h2)
  } else if (tn_h3_na < nrow(digt) / 4) {
  subda_tn <- rbind(subda_tn, subd_tn_h3)
  }
  fna_tn <- paste("tn", station, year, "all", sep = "_")
  write.table(subda_tn, file = paste(tn_fol, "/", fna_tn, ".txt", sep = ""),
  row.names = FALSE, col.names = FALSE, quote = FALSE, sep = "\t")
  # Output that doesn't keep the NA
  subd_tn <- subd_tn[!is.na(subd_tn$tmin), ]
  fn_tn <- paste("tn", station, year, sep = "_")
  write.table(subd_tn, file = paste(tn_fol, "/", fn_tn, ".txt", sep = ""),
  row.names = FALSE, col.names = FALSE, quote = FALSE, sep = "\t")
  save(subd_tn, file = paste(tn_fol, "/", fn_tn, ".RData", sep = ""))
  saveRDS(subd_tn, file = paste(tn_fol, "/", fn_tn, ".rds", sep = ""))

}
# DAILY MAXIMUM AIR TEMPERATURE
# Measured most commonly at 23:00 (not always)
if (sum(is.na(digt$tmax)) < nrow(digt)) {
  subd_tx <- digt[c(2:5, 15)]
  subd_tx$vcod <- c(rep("Tx", nrow(subd_tx)))
  subd_tx <- subd_tx[c("vcod", "year", "month", "day", "hour", "tmax")]
  tx_fol <- paste("tx_", station, sep = "")
  if (!dir.exists(tx_fol)) {
    dir.create(tx_fol)
  }
  ##################
  # If the value is always in the 4th hour (not sure...) the next will work:
  # subda_tx <- subd_tx[seq(4, nrow(subd_tx), 4), ]
  # fna_tx <- paste("tx", station, year, "all", sep = "_")
  # write.table(subda_tx, file = paste(fna_tx, "txt", sep = "."),
  #   row.names = FALSE, col.names = FALSE, quote = FALSE, sep = "\t")
  # subd_tx <- subd_tx[!is.na(subd_tx$tmax), ]
  # fn_tx <- paste("tx", station, year, sep = "_")
  # write.table(subd_tx, file = paste(fn_tx, "txt", sep = "."),
  #   row.names = FALSE, col.names = FALSE, quote = FALSE, sep = "\t")
  #################
  # Output that keeps the NA
  # Subsets hour by hour, starting on the 2nd hour
  # Usually the value in on the 4th hour of the time resolution (4)
  subda_tx <- data.frame()
  subd_tx_h2 <- subd_tx[seq(2, nrow(subd_tx), 4), ]
  tx_h2_na <- sum(is.na(subd_tx_h2$tmax))
  subd_tx_h3 <- subd_tx[seq(3, nrow(subd_tx), 4), ]
  tx_h3_na <- sum(is.na(subd_tx_h3$tmax))
  subd_tx_h4 <- subd_tx[seq(4, nrow(subd_tx), 4), ]
  tx_h4_na <- sum(is.na(subd_tx_h4$tmax))
  # Subsets the hour for which the values aren't all NA
  if (tx_h2_na < nrow(digt) / 4) {
    subda_tx <- rbind(subda_tx, subd_tx_h2)
  } else if (tx_h3_na < nrow(digt) / 4) {
    subda_tx <- rbind(subda_tx, subd_tx_h3)
  } else if (tx_h4_na < nrow(digt) / 4) {
    subda_tx <- rbind(subda_tx, subd_tx_h4)
  }
  fna_tx <- paste("tx", station, year, "all", sep = "_")
  write.table(subda_tx, file = paste(tx_fol, "/", fna_tx, ".txt", sep = ""),
    row.names = FALSE, col.names = FALSE, quote = FALSE, sep = "\t")
  # Output that doesn't keep the NA
  subd_tx <- subd_tx[!is.na(subd_tx$tmax), ]
  fn_tx <- paste("tx", station, year, sep = "_")
  write.table(subd_tx, file = paste(tx_fol, "/", fn_tx, ".txt", sep = ""),
    row.names = FALSE, col.names = FALSE, quote = FALSE, sep = "\t")
  save(subd_tx, file = paste(tx_fol, "/", fn_tx, ".RData", sep = ""))
  saveRDS(subd_tx, file = paste(tx_fol, "/", fn_tx, ".rds", sep = ""))
}
# RELATIVE HUMIDITY
if (sum(is.na(digt$rhum)) < nrow(digt)) {
  subda_rh <- digt[c(2:5, 16)]
  subda_rh$vcod <- c(rep("rh", nrow(subda_rh)))
  subda_rh <- subda_rh[c("vcod", "year", "month", "day", "hour", "rhum")]
  rh_fol <- paste("rh_", station, sep = "")
  if (!dir.exists(rh_fol)) {
    dir.create(rh_fol)
  }
  fna_rh <- paste("rh", station, year, "all", sep = "_")
  write.table(subda_rh, file = paste(rh_fol, "/", fna_rh, ".txt", sep = ""),
    row.names = FALSE, col.names = FALSE, quote = FALSE, sep = "\t")
  subd_rh <- subda_rh[!is.na(subda_rh$rhum), ]
  fn_rh <- paste("rh", station, year, sep = "_")
  write.table(subd_rh, file = paste(rh_fol, "/", fn_rh, ".txt", sep = ""),
    row.names = FALSE, col.names = FALSE, quote = FALSE, sep = "\t")
  save(subd_rh, file = paste(rh_fol, "/", fn_rh, ".RData", sep = ""))
  saveRDS(subd_rh, file = paste(rh_fol, "/", fn_rh, ".rds", sep = ""))
}
cat("Outputs of form_chl2c3sqc() in the folders 'varcode_StationName':\n")
cat("one file with the subdaily meteorological observations\n")
cat("for each one of the following variables -\n")
cat("td, n, dd, w, p, ta, rr, Tn, Tx, rh.\n")
cat("If any variable is missing it's because doesn't exist any\n")
cat("observations of that variable in the record.\n\n")
return(station)
}





