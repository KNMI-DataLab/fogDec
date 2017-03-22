library(DBI)
library(jsonlite)

dbConfig <- fromJSON("config.json")

con <- dbConnect(RPostgreSQL::PostgreSQL(),
                dbname = "FOGDB",
                host = dbConfig[["host"]], port = 22022,
                user = dbConfig[["user"]], password = dbConfig[["pw"]])



insertionDayPhase <- dbGetQuery(con, "INSERT INTO day_phases ( day_phase_id, day_phase_description)
                                                  VALUES (0, 'night'), (1, 'day'), (10, 'civil dawn'),
                                                  (11, 'civil dusk'), (20, 'nautical dawn'), (21, 'nautical dusk'),
                                                  (30, 'astronomical dawn'), (31, 'astronomical dusk');")


dfScotlandLocations <- read.csv("inst/extScripts/scotlandLatLon.csv", stringsAsFactors = F)
dfScotlandLocationsToWrite <- dfScotlandLocations[,3:5]
colnames(dfScotlandLocationsToWrite) <- c("location_description","longitude","latitude")
dfScotlandLocationsToWrite$location_description <- paste("UK", dfScotlandLocationsToWrite$location_description)
dbWriteTable(con, "locations", dfScotlandLocationsToWrite, append = T)

                          

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


dbDisconnect(con)







