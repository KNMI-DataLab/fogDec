#' Couple cameras and KNMI nearby stations
#' @param maxDistance Numeric distance radius from the KNMI station
#' @import jsonlite DBI postGIStools geosphere data.table stats
#' @export
coupleCamerasAndKNMInearStations<-function(maxDistance = 5000,dbConfigDir){

distanceUsableSensor<-maxDistance

Sys.setenv(TZ = "UTC")

dbConfig <- fromJSON(paste0(dbConfigDir,"config.json"))

con <- dbConnect(RPostgreSQL::PostgreSQL(),
                 dbname = "FOGDB",
                 host = dbConfig[["host"]], port = 9418,
                 user = dbConfig[["user"]], password = dbConfig[["pw"]])

knmiStations <- as.data.table(dbReadTable(con, "meteo_stations"))
#camerasTable <- as.data.table(dbReadTable(con, "cameras"))
locationsTable <- as.data.table(dbReadTable(con, "locations"))
dbDisconnect(con)


locationsCameras<-locationsTable
#locationsPortHW<-data.table(locationsPortHW)

setkey(knmiStations,location_id)
setkey(locationsTable,location_id)
combinedMeteoStationsLocations<-locationsTable[knmiStations,]

matCameras<-cbind(locationsTable$longitude,locationsTable$latitude)
matCameras<-matrix(matCameras,ncol = 2)



matSensors<-cbind(combinedMeteoStationsLocations$longitude,combinedMeteoStationsLocations$latitude)
matSensors<-matrix(matSensors,ncol=2)
distm(matSensors,matCameras)
distances<-distm(matSensors,matCameras)
rownames(distances)<-combinedMeteoStationsLocations$knmi_kis_id
colnames(distances)<-locationsTable$location_id
distances[distances>distanceUsableSensor]<-NA
distancesDT<-data.table(distances)

distancesDT


#get the rownames
KISstations<-rownames(x = distances)[apply(distances, 2, function(x){ if (all(is.na(x))) {NA}  else {which.min(x)}})]
locationIDsHW<-colnames(distances)
locationAndMeteoStation<-cbind(KISstations,locationIDsHW)
locationAndMeteoStation<-locationAndMeteoStation[complete.cases(locationAndMeteoStation),]
locationAndMeteoStation<-data.table(locationAndMeteoStation)

locationAndMeteoStation

}



