#' Read wind data files
#' @param filenames List of filenames
#' @return data.table
#' @export
ReadWindData <- function(filenames) {
  dateTime <- day <- IT_DATETIME <- TOW.FF_10M <- NULL
  sensorData <- rbindlist(lapply(filenames, read.csv, stringsAsFactors = FALSE))
  sensorData <- data.table(sensorData)
  sensorData[TOW.FF_10M  == '', TOW.FF_10M  := NA]
  #sensorData[, hhmmss := CorrectOurs(hhmmss)]
  sensorData[, IT_DATETIME := as.POSIXct(sensorData[, IT_DATETIME], format = "%Y%m%d_%H%M%S", tz = "UTC")]
  setnames(sensorData, "IT_DATETIME", "dateTime")
  #sensorData[, hhmmss := NULL]
  sensorData[, year   := year(dateTime)]
  sensorData[, month  := month(dateTime)]
  sensorData[, day    := mday(dateTime)]
  sensorData[, hour   := hour(dateTime)]
  setcolorder(sensorData, c(2, 1, 5, 6, 7, 8, 3, 4))
  sensorData[, TOW.Q_FF_10M_10:= NULL]
  sensorData[, DS_CODE := NULL]
  sensorData[ ,`:=`(year = NULL, month = NULL, day = NULL, hour = NULL)]
  setnames(sensorData, "TOW.FF_10M_10","windSpeed")
  return(sensorData)
}

#' Read relative humidity data files
#' @param filenames List of filenames
#' @return data.table
#' @export
ReadHumidityData <- function(filename) {
  dateTime <- day <- IT_DATETIME <- TOW.FF_10M <- NULL
  sensorData <- fread(filename)
  setnames(sensorData, "TOT.U_10", "relHumidity")
  sensorData[, TOT.Q_U_10 := NULL]
  sensorData[relHumidity == '', relHumidity := NA]
  sensorData[, relHumidity := as.integer(relHumidity)]
  sensorData[, IT_DATETIME := as.POSIXct(sensorData[, IT_DATETIME], format = "%Y%m%d_%H%M%S", tz = "UTC")]
  setnames(sensorData, "IT_DATETIME", "dateTime")
  return(sensorData)
}


##THIS HAS TO BE TESTED##
SynchronizeSensorReadingsNoMORPicture <- function(sensorDataDT, imageInfoDT){
setkey(sensorDataDT, dateTime)
setkey(imageInfoDT, dateTime)
sensorReadingsNoMOR <- sensorDataDT[imageInfoDT, roll = "nearest", nomatch = 0]
sensorReadingsNoMOR
}