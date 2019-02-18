#' Builds Multi-year Series for the Station Variables.
#'
#' For each of the variables of a given station, rbinds the anual data frames in
#' the C3S-QC format to make a series of several years.
#'
#' @param station character string given the WIGOS compatible station
#'   identifier.
#'
#' @usage build_series(station)
#'
#' @import utils
#'
#' @export
#'
build_series <- function(station) {
  td <- paste("td", station, sep = "_")
  n <- paste("n", station, sep = "_")
  dd <- paste("dd", station, sep = "_")
  w <- paste("w", station, sep = "_")
  p <- paste("p", station, sep = "_")
  ta <- paste("ta", station, sep = "_")
  rr <- paste("rr", station, sep = "_")
  tn <- paste("tn", station, sep = "_")
  tx <- paste("tx", station, sep = "_")
  rh <- paste("rh", station, sep = "_")
  fol <- c(td, n, dd, w, p, ta, rr, tn, tx, rh)
  if (sum(dir.exists(fol)) == 0) {
    cat("Wrong WIGOS compatible StationName.\n\n")
    return()
  }
  for (i in 1:length(fol)) {
    if (dir.exists(fol[i])) {
      fvec <- dir(fol[i]) # or fvec <- list.files(fol[i])
      serie <- data.frame()
      rds <- grepl(".rds", fvec)
      trds <- fvec[rds]
      for (j in 1:length(trds)) {
          subd <- readRDS(paste(fol[i], "/", trds[j], sep = ""))
          serie <- rbind(serie, subd)
      }
      write.table(serie,
        file = paste(fol[i], "/", fol[i], "_50-58.txt", sep = ""),
        row.names = FALSE, col.names = FALSE, sep = "\t", quote = FALSE)
      save(serie, file = paste(fol[i], "/", fol[i], "_50-58.RData", sep = ""))
    }
  }
  cat("Outputs of build_series() in the folders 'varcode_StationName'\n")
  cat("containing the multi-year series: 'varcode_StationName_50-58.RData'\n\n")
}



