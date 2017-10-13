library(DBI)
library(jsonlite)
library(data.table)
#library(knmiR)
library(postGIStools)
library(geosphere)


coupleCamerasAndKNMInearStations<-function(maxDistance = 5000){
locationKNMIStations<-read.csv("inst/extdata/testStationsGood2.csv", sep = ";",header = TRUE)
locationKNMIStations<-data.table(locationKNMIStations)
#fill NA with a 0 coordinate NB: Bonaire station 990 is not filled
locationKNMIStations[is.na(lon), lon:=0]
locationKNMIStations<-locationKNMIStations[lat!=9999 & as.Date(as.character(locationKNMIStations$endDate), "%Y%m%d")==as.Date("99991231", "%Y%m%d")]
locationKNMIStations<-locationKNMIStations[typeStation=="Luchtdruk- en weerwaarnemingen"]

uniqueKNMIStations<-unique(locationKNMIStations,by=c("lat", "lon"))
uniqueKNMIStations[,KISID:=gsub("_A_","_WAARNEEMID_", KISID)]
stationsForDB<-uniqueKNMIStations[, !c("lat", "lon", "startDate","endDate")]


distanceUsableSensor<-maxDistance

Sys.setenv(TZ = "UTC")

dbConfig <- fromJSON("config.json")

con <- dbConnect(RPostgreSQL::PostgreSQL(),
                 dbname = "FOGDB",
                 host = dbConfig[["host"]], port = 9418,
                 user = dbConfig[["user"]], password = dbConfig[["pw"]])


camerasTable <- as.data.table(dbReadTable(con, "cameras"))
locationsTable <- as.data.table(dbReadTable(con, "locations"))
dbDisconnect(con)


locationsCameras<-locationsTable
#locationsPortHW<-data.table(locationsPortHW)

matCameras<-cbind(locationsCameras$longitude,locationsCameras$latitude)
matCameras<-matrix(matCameras,ncol = 2)
matSensors<-cbind(locationKNMIStations$lon,locationKNMIStations$lat)
matSensors<-matrix(matSensors,ncol=2)
distm(matSensors,matCameras)
distances<-distm(matSensors,matCameras)
rownames(distances)<-locationKNMIStations$KISID
colnames(distances)<-locationsCameras$location_id
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

# test<-locationsPortHW[rep(seq_len(nrow(locationsPortHW)), 7), ]
# test2<-locationPortSight[rep(seq_len(nrow(locationPortSight)), 27), ]
# fullData<-cbind(test,test2)
# 
# distance<-distm(as.matrix(cbind(fullData$longitude, fullData$latitude),ncol=2),as.matrix(cbind(fullData$lon,fullData$lat),ncol=2), fun = distHaversine)


