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
  dateTime <- day <- IT_DATETIME <- relHumidity <- NULL
  sensorData <- fread(filename)
  setnames(sensorData, "TOT.U_10", "relHumidity")
  sensorData[, TOT.Q_U_10 := NULL]
  sensorData[relHumidity == '', relHumidity := NA]
  sensorData[, relHumidity := as.integer(relHumidity)]
  sensorData[, IT_DATETIME := as.POSIXct(sensorData[, IT_DATETIME], format = "%Y%m%d_%H%M%S", tz = "UTC")]
  setnames(sensorData, "IT_DATETIME", "dateTime")
  return(sensorData)
}


#' Read temperature and dew point data files
#' @param filenames List of filenames
#' @return data.table
#' @export
ReadTempDewPointData <- function(filename) {
  dateTime <- day <- IT_DATETIME <- airTemperature <- dewPoint<- NULL
  sensorData <- fread(filename)
  setnames(sensorData, "TOT.T_DRYB_10", "airTemperature")
  setnames(sensorData, "TOT.T_DEWP_10", "dewPoint")
  sensorData[, TOT.Q_T_DRYB_10 := NULL]
  sensorData[, TOT.Q_T_DEWP_10 := NULL]
  sensorData[airTemperature == '', airTemperature := NA]
  sensorData[dewPoint == '', dewPoint := NA]
  sensorData[, airTemperature := as.numeric(airTemperature)]
  sensorData[, dewPoint := as.numeric(dewPoint)]
  sensorData[, IT_DATETIME := as.POSIXct(sensorData[, IT_DATETIME], format = "%Y%m%d_%H%M%S", tz = "UTC")]
  setnames(sensorData, "IT_DATETIME", "dateTime")
  return(sensorData)
}



#' Read precipitation data files
#' @param filenames List of filenames
#' @return data.table
#' @export
ReadPrecipitationData <- function(filename) {
  dateTime <- day <- IT_DATETIME <- precipitationIntElec <- precipitationIntPWS <-precipitationDurationElec <- precipitationDurationPWS  <- NULL
  sensorData <- fread(filename)
  setnames(sensorData, "TOR.RI_REGENM_10", "precipitationIntElec")
  setnames(sensorData, "TOR.RI_PWS_10", "precipitationIntPWS")
  setnames(sensorData, "TOR.DR_REGENM_10", "precipitationDurationElec")
  setnames(sensorData, "TOR.DR_PWS_10", "precipitationDurationPWS")
  sensorData[, TOR.Q_RI_REGENM_10 := NULL]
  sensorData[, TOR.Q_RI_PWS_10 := NULL]
  sensorData[, TOR.Q_DR_REGENM_10 := NULL]
  sensorData[, TOR.Q_DR_PWS_10 := NULL]
  sensorData[precipitationIntElec == '', precipitationIntElec := NA]
  sensorData[, precipitationIntElec := as.numeric(precipitationIntElec)]
  sensorData[precipitationIntPWS == '', precipitationIntPWS := NA]
  sensorData[, precipitationIntPWS := as.numeric(precipitationIntPWS)]
  sensorData[precipitationDurationElec == '', precipitationDurationElec := NA]
  sensorData[, precipitationDurationElec := as.numeric(precipitationDurationElec)]
  sensorData[precipitationDurationPWS == '', precipitationDurationPWS := NA]
  sensorData[, precipitationDurationPWS := as.numeric(precipitationDurationPWS)]
  sensorData[, IT_DATETIME := as.POSIXct(sensorData[, IT_DATETIME], format = "%Y%m%d_%H%M%S", tz = "UTC")]
  setnames(sensorData, "IT_DATETIME", "dateTime")
  return(sensorData)
}



##Works, maybe a formal test would be best##
SynchronizeSensorReadingsNoMORPicture <- function(sensorDataDT, imageInfoDT){
setkey(sensorDataDT, dateTime)
setkey(imageInfoDT, dateTime)
sensorReadingsNoMOR <- sensorDataDT[imageInfoDT, roll = "nearest", nomatch = 0]
sensorReadingsNoMOR
}