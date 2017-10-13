library(DBI)
library(jsonlite)
library(data.table)

dbConfig <- fromJSON("config.json")

con <- dbConnect(RPostgreSQL::PostgreSQL(),
                dbname = "FOGDB",
                host = dbConfig[["host"]], port = 9418,
                user = dbConfig[["user"]], password = dbConfig[["pw"]])



insertionDayPhase <- dbGetQuery(con, "INSERT INTO day_phases ( day_phase_id, day_phase_description)
                                                  VALUES (0, 'night'), (1, 'day'), (10, 'civil dawn'),
                                                  (11, 'civil dusk'), (20, 'nautical dawn'), (21, 'nautical dusk'),
                                                  (30, 'astronomical dawn'), (31, 'astronomical dusk');")


dbDisconnect(con)

###
###
###
library(data.table)

#summary <- readRDS("../processedImages/Meetterrein/201510_summary.rds")
summary  <- readRDS("../processedImages/Cabauw/2016_10_summary.rds")

## Locations
dbListFields(con, "locations")
dbReadTable(con, "locations")
# tmp <- summary[, .(latitude = unique(lat), longitude = unique(lon), stationID = unique(stationID), locationName = unique(locationName)), by = locationID]
# tmp <- tmp[, .(location_description = paste0(locationName, " (", stationID, ")"), longitude, latitude)]

tmp <- summary[, .(latitude = unique(lat), longitude = unique(lon), locationName = unique(locationName)), by = .(locationID, stationID)]
tmp <- tmp[, .(location_description = paste0(locationName, " (", stationID, ")"), longitude, latitude)]

dbWriteTable(con, "locations", tmp, append = TRUE, row.names = FALSE, match.cols = TRUE)

## Camera
dbListFields(con, "cameras")
dbReadTable(con, "cameras")

#tmp <- data.frame(location_id = 1, camera_description = "Meetterrein normal", camera_name = "Camera 1")
tmp <- data.frame(location_id = c(2,2), camera_description = c("Cabauw left (NL3)", "Cabauw right (NL4)"), camera_name = c("Camera 1", "Camera 2"))
dbWriteTable(con, "cameras", tmp, append = TRUE, row.names = FALSE, match.cols = TRUE)


## Images
dbListFields(con, "images")
Sys.setenv(TZ = "UTC")
dbReadTable(con, "images")
# tmp <- summary[, .(camera_id=1, timestamp = dateTime, filepath = filePath, day_phase = 1)]
tmp <- summary[locationID == "NL4", .(camera_id=3, timestamp = dateTime, filepath = filePath, day_phase = 1)]
dbWriteTable(con, "images", tmp, append = TRUE, row.names = FALSE, match.cols = TRUE)

Sys.setenv(TZ = "UTC")
ref <- as.data.table(dbReadTable(con, "images"))

## Image features
dbListFields(con, "image_features")
dbReadTable(con, "image_features")

images <- as.data.table(dbReadTable(con, "images"))
fullSummary <- merge(summary, images, by.x = "filePath", by.y="filepath")

tmp <- fullSummary[, .(image_id, camera_id, timestamp, mean_edge = meanEdge,
                       change_point = changePoint, smoothness,
                       fractal_dim = fractalDim, mean_hue = meanHue, 
                       mean_saturation = meanSaturation,
                       mean_brightness = meanBrightness)]

dbWriteTable(con, "image_features", tmp, append = TRUE, row.names = FALSE, match.cols = TRUE)

allfeatures <- as.data.table(dbReadTable(con, "image_features"))

tmp <- as.data.table(dbGetQuery(con, "SELECT * FROM image_features WHERE extract(year from timestamp) = 2016 AND extract(month from timestamp) = 10 AND extract(day from timestamp) = 10;"))

## Meteo features
dbListFields(con, "meteo_features_stations")
tmp <- as.data.table(dbReadTable(con, "meteo_features_stations"))

###
### Set time zone!
###
Sys.setenv(TZ = "UTC")
cabauw <- fread("~/mschijf_roth/tmp/roth_zm_348.txt")
cabauw[, timestamp := as.POSIXct(paste(V2, V3 %/% 100, V3 %% 100, sep = "-"), format = "%Y%m%d-%H-%M", tz = "UTC")]
# 
cabauw <- cabauw[, .(timestamp, MOR = V4)]
# 
tmp <- as.data.table(dbGetQuery(con, "SELECT * FROM image_features WHERE camera_id = 2"))
tmp2 <- merge(tmp, cabauw, by = "timestamp")
# 
# tmp2 <- tmp2[, .(location_id = 3, timestamp, mor_visibility = MOR)]
# tmp2 

# deBilt <- visDec:::ReadMORSensorData("~/mschijf_roth/tmp/table.csv")
# deBilt <- deBilt[, .(timestamp = dateTime, mor_visibility = TOA.MOR_10)]
# 
# tmp <- as.data.table(dbGetQuery(con, "SELECT * FROM images WHERE camera_id = 1"))
# 
# tmp2 <- merge(tmp, deBilt, by = "timestamp")
# tmp2 <- tmp2[, .(location_id = 1, timestamp, mor_visibility)]

dbWriteTable(con, "meteo_features_stations", tmp2, append = TRUE, row.names = FALSE, match.cols = TRUE)

##
## update


tmpOld <- as.data.table(dbReadTable(con, "meteo_features_stations"))
tmp2 <- tmp2[, .(location_id = 3, timestamp, mor_visibility = MOR)]

tmpMix <- merge(tmp2, tmpOld, by = c("location_id", "timestamp"))

paste("UPDATE meteo_features_stations SET mor_visibility=", 
      tmpMix$mor_visibility.x[i], 
      " where id=", 
      tmpMix$meteo_feature_id[i])

tmpNa <- tmpMix[is.na(mor_visibility.x), ]

for (i in 1 : length(tmpMix[, meteo_feature_id])) {
  sqlQuery <- paste0("UPDATE meteo_features_stations SET mor_visibility=", 
        tmpMix$mor_visibility.x[i], 
        " where meteo_feature_id=",
        tmpMix$meteo_feature_id[i])
  # if (i ==1) print(sqlQuery)
  dbGetQuery(con, sqlQuery)
}

for (i in 1 : length(tmpNa[, meteo_feature_id])) {
  sqlQuery <- paste0("DELETE FROM meteo_features_stations ",
                     " where meteo_feature_id=",
                     tmpNa$meteo_feature_id[i])
  # if (i ==1) print(sqlQuery)
  dbGetQuery(con, sqlQuery)
}



##########################De Bilt images######################################################################
filesDeBilt <- list.files("/net/pc150395/nobackup/users/roth/processedImages/Meetterrein",full.names = T)
listDFSummary <- lapply(filesDeBilt, readRDS)
deBiltDataSummary <- rbindlist(listDFSummary)
Sys.setenv(TZ = "UTC")
tmp <- deBiltDataSummary[,.(camera_id=1, timestamp = dateTime, filepath = filePath, day_phase = 1)]

dbListFields(con, "images")
Sys.setenv(TZ = "UTC")
dbReadTable(con, "images")

dbWriteTable(con, "images", tmp, append = TRUE, row.names = FALSE, match.cols = TRUE)
##############################################################################################################


###################Cabauw images##############################################################################
## Images
dbListFields(con, "images")
Sys.setenv(TZ = "UTC")
dbReadTable(con, "images")

filesCabauw <- list.files("/net/pc150395/nobackup/users/roth/processedImages/Cabauw", full.names = T)
listDFSummary <- lapply(filesCabauw, readRDS)
cabauwDataSummary <- rbindlist(listDFSummary)

tmp <- cabauwDataSummary[locationID == "NL4", .(camera_id=3, timestamp = dateTime, filepath = filePath, day_phase = 1)]
dbWriteTable(con, "images", tmp, append = TRUE, row.names = FALSE, match.cols = TRUE)

tmp2 <- cabauwDataSummary[locationID == "NL3", .(camera_id=2, timestamp = dateTime, filepath = filePath, day_phase = 1)]
dbWriteTable(con, "images", tmp2, append = TRUE, row.names = FALSE, match.cols = TRUE)
##############################################################################################################



###Image features De Bilt#####################################################################################
filesDeBilt <- list.files("/net/pc150395/nobackup/users/roth/processedImages/Meetterrein", full.names = T)
listDFSummary <- lapply(filesDeBilt, readRDS)
deBiltDataSummary <- rbindlist(listDFSummary)


dbListFields(con, "image_features")
dbReadTable(con, "image_features")

images <- as.data.table(dbReadTable(con, "images"))
fullSummary <- merge(deBiltDataSummary, images, by.x = "filePath", by.y="filepath")

tmp <- fullSummary[, .(image_id, camera_id, timestamp, mean_edge = meanEdge,
                       change_point = changePoint, smoothness,
                       fractal_dim = fractalDim, mean_hue = meanHue, 
                       mean_saturation = meanSaturation,
                       mean_brightness = meanBrightness)]

dbWriteTable(con, "image_features", tmp, append = TRUE, row.names = FALSE, match.cols = TRUE)
##############################################################################################################


###Image features Cabauw######################################################################################
filesCabauw<-list.files("/net/pc150395/nobackup/users/roth/processedImages/Cabauw", full.names = T)
listDFSummary<-lapply(filesCabauw, readRDS)
cabauwDataSummary<-rbindlist(listDFSummary)


dbListFields(con, "image_features")
dbReadTable(con, "image_features")

images <- as.data.table(dbReadTable(con, "images"))
fullSummary <- merge(cabauwDataSummary, images, by.x = "filePath", by.y="filepath")

tmp <- fullSummary[, .(image_id, camera_id, timestamp, mean_edge = meanEdge,
                       change_point = changePoint, smoothness,
                       fractal_dim = fractalDim, mean_hue = meanHue, 
                       mean_saturation = meanSaturation,
                       mean_brightness = meanBrightness)]

dbWriteTable(con, "image_features", tmp, append = TRUE, row.names = FALSE, match.cols = TRUE)
##############################################################################################################
imageFeatures <- as.data.table(dbReadTable(con, "image_features"))







##Insertion airport location info#########################################################################################################
airportInfo <- fromJSON("camerasConf.json")
tmp <- airportInfo$cameras$airports
tmp <- within(tmp, rm(ipAddr, "_note"))
tmp <- unique(tmp[,c('location','longitude', 'latitude')])
tmp$location_description <-paste(tmp$location, "airport")
tmp <- within(tmp, rm(location))
dbWriteTable(con, "locations", tmp, append = TRUE, row.names = FALSE, match.cols = TRUE)
##############################################################################################################
locations <- as.data.table(dbReadTable(con, "locations"))


##Insertion airport cameras info#########################################################################################################
airportInfo <- fromJSON("camerasConf.json")

locations <- as.data.table(dbReadTable(con, "locations"))

tmp <- airportInfo$cameras$airports
tmp <- within(tmp, rm(ipAddr, "_note"))
tmp <- unique(tmp[,c('location','cameraID')])
tmp$camera_description <- paste0(tmp$location,"-",tmp$cameraID)
tmp$camera_name <- tmp$cameraID
tmp <- within(tmp, rm(cameraID))
tmp$tempKey <- paste(tmp$location, "airport")

tmp <- data.table(tmp)

setkey(tmp,tempKey)
setkey(locations, location_description)

tmp<-locations[tmp,]

tmp[, c('longitude','latitude', 'location_description', 'location') := NULL ]

dbWriteTable(con, "cameras", tmp, append = TRUE, row.names = FALSE, match.cols = TRUE)
##############################################################################################################
cameras <- as.data.table(dbReadTable(con, "cameras"))







##Insertion Scotland location info#########################################################################################################
scotlandinfo <- fromJSON("camerasConf.json")
tmp <- scotlandinfo$cameras$SCOTLAND
tmp <- within(tmp, rm(ipAddr, pwd, user, dir, server, cameraID))
uniqueLines <- tmp[!duplicated(tmp[,c("longitude","latitude")]),]
test<-tmp[duplicated(tmp[,c("longitude", "latitude")]),]
test$newLocation<-paste0(lapply(strsplit(test$location,"-(?=[^-]+$)", perl = T), function(x) x[1]),"-shared-location")
test<-data.table(test)
tmp<-data.table(tmp)
setkey(test, longitude,latitude)
setkey(tmp, longitude,latitude)
test2<-test[tmp,]
test2$newLocation[is.na(test2$newLocation)]<-test2$i.location[is.na(test2$newLocation)]

test2 <- within(test2, rm(i.location, location))
#######
test2$location_description <-paste(test2$newLocation, "scotland motorway")
test2 <- within(test2, rm(newLocation))
test2<-unique(test2)
dbWriteTable(con, "locations", test2, append = TRUE, row.names = FALSE, match.cols = TRUE)
##############################################################################################################
locations <- as.data.table(dbReadTable(con, "locations"))


##Insertion scotland cameras info#########################################################################################################
scotlandInfo <- fromJSON("camerasConf.json")

locations <- as.data.table(dbReadTable(con, "locations"))

tmp <- scotlandInfo$cameras$SCOTLAND
tmp <- within(tmp, rm(ipAddr, pwd, user, dir, server))
test<-tmp[duplicated(tmp[,c("longitude", "latitude")]),]
test$newLocation<-paste0(lapply(strsplit(test$location,"-(?=[^-]+$)", perl = T), function(x) x[1]),"-shared-location")
test<-data.table(test)
tmp<-data.table(tmp)
setkey(test, longitude,latitude)
setkey(tmp, longitude,latitude)
test2<-test[tmp,]
test2$newLocation[is.na(test2$newLocation)]<-test2$i.location[is.na(test2$newLocation)]
test2<-within(test2, rm(location,cameraID, i.location))




test2$camera_description <- paste0(test2$newLocation,"-",test2$i.cameraID)
test2$camera_name <- test2$i.cameraID
test2 <- within(test2, rm(i.cameraID))
test2$tempKey <- paste(test2$newLocation, "scotland motorway")

setkey(test2,tempKey)
setkey(locations, location_description)

tmp<-locations[test2,]

tmp[, c('longitude','latitude', 'location_description', 'newLocation', 'i.longitude', 'i.latitude') := NULL ]

dbWriteTable(con, "cameras", tmp, append = TRUE, row.names = FALSE, match.cols = TRUE)
##############################################################################################################
cameras <- as.data.table(dbReadTable(con, "cameras"))





##Insertion RWS location info#########################################################################################################
RWSinfo <- fromJSON("camerasConf.json")
tmp <- RWSinfo$cameras$RWS
tmp <- within(tmp, rm(ipAddr, '_note'))
uniqueLines <- tmp[!duplicated(tmp[,c("longitude","latitude")]),]
test<-tmp[duplicated(tmp[,c("longitude", "latitude")]),]
#test$newLocation<-paste0(lapply(strsplit(test$location,"-(?=[^-]+$)", perl = T), function(x) x[1]),"-shared-location")
#test<-data.table(test)
tmp<-data.table(tmp)
tmp[,location_description:=paste0(location,"-",cameraID)]
tmp <- within(tmp,rm(cameraID,location))

dbWriteTable(con, "locations", tmp, append = TRUE, row.names = FALSE, match.cols = TRUE)
##############################################################################################################
locations <- as.data.table(dbReadTable(con, "locations"))

##Insertion RWS cameras info#########################################################################################################
RWSInfo <- fromJSON("camerasConf.json")

locations <- as.data.table(dbReadTable(con, "locations"))

tmp <- RWSInfo$cameras$RWS
tmp <- within(tmp, rm(ipAddr, "_note"))
tmp <- unique(tmp[,c('location','cameraID')])
tmp$camera_description <- paste0(tmp$location,"-",tmp$cameraID)
tmp$camera_name <- tmp$cameraID
tmp$tempKey <- paste0(tmp$location,"-",tmp$cameraID)
tmp <- within(tmp, rm(cameraID))

tmp <- data.table(tmp)

setkey(tmp,tempKey)
setkey(locations, location_description)

tmp<-locations[tmp,]

tmp[, c('longitude','latitude', 'location_description', 'location') := NULL ]

dbWriteTable(con, "cameras", tmp, append = TRUE, row.names = FALSE, match.cols = TRUE)
##############################################################################################################
cameras <- as.data.table(dbReadTable(con, "cameras"))



############METEO STATIONS LOCATIONS############################################
locationKNMIStations<-read.csv("inst/extdata/testStationsGood2.csv", sep = ";",header = TRUE)
locationKNMIStations<-data.table(locationKNMIStations)
#fill NA with a 0 coordinate NB: Bonaire station 990 is not filled
locationKNMIStations[is.na(lon), lon:=0]
locationKNMIStations<-locationKNMIStations[lat!=9999 & as.Date(as.character(locationKNMIStations$endDate), "%Y%m%d")==as.Date("99991231", "%Y%m%d")]
locationKNMIStations<-locationKNMIStations[typeStation=="Luchtdruk- en weerwaarnemingen"]

uniqueKNMIStations<-unique(locationKNMIStations,by=c("lat", "lon"))
uniqueKNMIStations[,KISID:=gsub("_A_","_WAARNEEMID_", KISID)]
setnames(uniqueKNMIStations, c("lon","lat", "stationName", "KISID", "locationCode"), c("longitude", "latitude", "meteo_station_name", "knmi_kis_id", "meteo_station_location_code"))

tmp<-uniqueKNMIStations
tmp<-within(tmp, rm(typeStation,altitude,startDate,endDate))
tmp<-tmp[,location_description:= paste("knmi",meteo_station_name)]
tmp<-within(tmp, rm("meteo_station_name", "meteo_station_location_code", "knmi_kis_id"))

#stationsForDB<-uniqueKNMIStations[, !c("lat", "lon", "startDate","endDate")]
dbWriteTable(con, "locations", tmp, append = TRUE, row.names = FALSE, match.cols = TRUE)
###############################################################################




############METEO STATIONS TABLE############################################
locationKNMIStations<-read.csv("inst/extdata/testStationsGood2.csv", sep = ";",header = TRUE)
locationKNMIStations<-data.table(locationKNMIStations)
#fill NA with a 0 coordinate NB: Bonaire station 990 is not filled
locationKNMIStations[is.na(lon), lon:=0]
locationKNMIStations<-locationKNMIStations[lat!=9999 & as.Date(as.character(locationKNMIStations$endDate), "%Y%m%d")==as.Date("99991231", "%Y%m%d")]
locationKNMIStations<-locationKNMIStations[typeStation=="Luchtdruk- en weerwaarnemingen"]

uniqueKNMIStations<-unique(locationKNMIStations,by=c("lat", "lon"))
uniqueKNMIStations[,KISID:=gsub("_A_","_SENSORID_", KISID)]
setnames(uniqueKNMIStations, c("lon","lat", "stationName", "KISID", "locationCode","typeStation"), c("longitude", "latitude", "meteo_station_name", "knmi_kis_id", "meteo_station_location_code","meteo_station_type"))

tmp<-uniqueKNMIStations
#tmp<-within(tmp, rm(altitude,startDate,endDate,latitude,longitude))

setkeyv(tmp,c("longitude","latitude"))

locations <- as.data.table(dbReadTable(con, "locations"))
setkeyv(locations, c("longitude","latitude"))

tmp<-tmp[locations,nomatch=0]

#tmp<-tmp[,location_description:= paste("knmi",meteo_station_name)]
tmp<-tmp[,c("meteo_station_name", "meteo_station_location_code", "knmi_kis_id","location_id","meteo_station_type")]

dbWriteTable(con, "meteo_stations", tmp, append = TRUE, row.names = FALSE, match.cols = TRUE)
###############################################################################




dbDisconnect(con)



