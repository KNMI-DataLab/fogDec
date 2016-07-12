#' Read MOR data files
#' @param filenames List of filenames
#' @return data.table
#' @import data.table
#' @export
ReadMORSensorData <- function(filenames) {
  FS261 <- TMM261 <- FS260 <- yyyymmdd <- hhmmss <- dateTime <- day <- NULL
  sensorData <- rbindlist(lapply(filenames, fread))
  sensorData[FS261  == -1, FS261  := NA]
  sensorData[TMM261 == -1, TMM261 := NA]
  sensorData[FS260  == -1, FS260  := NA]
  sensorData[, hhmmss := CorrectOurs(hhmmss)]
  sensorData[, yyyymmdd := as.POSIXct(paste(yyyymmdd, hhmmss), format="%Y%m%d %H%M%S", tz = "CET") - 10 * 60]
  setnames(sensorData, "yyyymmdd", "dateTime")
  sensorData[, hhmmss := NULL]
  sensorData[, year   := year(dateTime)]
  sensorData[, month  := month(dateTime)]
  sensorData[, day    := mday(dateTime)]
  sensorData[, hour   := hour(dateTime)]
  setcolorder(sensorData, c(1, 5, 6, 7, 8, 2, 3, 4))
  return(sensorData)
}


#' Read MOR data files Twente
#' @param filenames List of filenames
#' @return data.table
#' @export
ReadMORSensorDataTWE <- function(filenames) {
  #FS261 <- TMM261 <- FS260 <- yyyymmdd <- hhmmss <- dateTime <- day <- NULL
  sensorData <- read.csv(filenames,stringsAsFactors = FALSE)
  sensorData <- data.table(sensorData)
  sensorData[TOA.MOR_10  == -1, TOA.MOR_10  := NA]
  sensorData[TOA.VV_10 == -1, TOA.VV_10 := NA]
  sensorData[TOA.AH_10  == -1, TOA.AH_10  := NA]
  #sensorData[, hhmmss := CorrectOurs(hhmmss)]
  sensorData[, IT_DATETIME := as.POSIXct(sensorData[, IT_DATETIME], format = "%Y%m%d_%H%M%S", tz = "CET")]
  setnames(sensorData, "IT_DATETIME", "dateTime")
  #sensorData[, hhmmss := NULL]
  sensorData[, year   := year(dateTime)]
  sensorData[, month  := month(dateTime)]
  sensorData[, day    := mday(dateTime)]
  sensorData[, hour   := hour(dateTime)]
  setcolorder(sensorData, c(1, 2, 3, 5, 7, 9, 10, 11, 12, 4, 6, 8))
  return(sensorData)
}

CorrectOurs <- function(x) {
  y <- paste(x)
  y[x < 100000] <- paste0("0", x[x < 100000])
  y[x <  10000] <- paste0("00", x[x < 10000])
  return(y)
}
