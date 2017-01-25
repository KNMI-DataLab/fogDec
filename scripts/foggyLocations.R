library(data.table)

files <- c("inst/extdata/Sensor/MOR-All/MOR20160101--20170123_Part1.csv",
           "inst/extdata/Sensor/MOR-All/MOR20160101--20170123_Part2.csv",
           "inst/extdata/Sensor/MOR-All/MOR20160101--20170123_Part3.csv")

sensorValues <- rbindlist(lapply(files, fread))

sensorValues <- sensorValues[TOA.MOR_10 != ""]

KisDateTimePosixct <- function(datetime) {
  as.POSIXct(substr(datetime, 1, 15), format = "%Y%m%d_%H%M%S", tz = "UTC")
}
sensorValues[, dateTime := KisDateTimePosixct(IT_DATETIME), by = IT_DATETIME]
sensorValues[, hour := hour(dateTime)]
sensorValues[, yday := yday(dateTime)]
sensorValues[, year := year(dateTime)]


sensorValues <- sensorValues[year == 2016]

sensorValues[, MOR := as.integer(TOA.MOR_10)]

sensorValues[, mean(MOR, na.rm=TRUE), by = DS_CODE]

sensorValues[MOR < 250 & hour %in% seq.int(4, 20), .N, by = DS_CODE]

aggValues <- sensorValues[MOR < 250& hour %in% seq.int(4, 20), .N, by = .(DS_CODE, yday)]
foggyDays <- aggValues[, .(foggyDays = .N), by = DS_CODE]

foggyLocations <- foggyDays[foggyDays >= 40]
setkey(foggyLocations, foggyDays)

foggyLocations[, naam := c("(Woensdrecht, locatie A)",
                          "(Woensdrecht, locatie obs07t) ",
                          "(Deelen, locatie 20t)",
                          "(Deelen, waarneemterrein)",
                          "(Stavoren)",
                          "(Hoogeveen)",
                          "(Schiphol, locatie Muiden)",
                          "(Deelen, locatie obs20t)",
                          "(Eelde, locatie 23t)",
                          "(Voorschoten, barometer en PW-sensor)",
                          "(Twenthe, locatie A)"
                          )]


