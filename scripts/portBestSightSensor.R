library(DBI)
library(jsonlite)
library(data.table)
#library(knmiR)
library(postGIStools)
library(geosphere)

locationPortSight<-read.csv("inst/extdata/sightLocationRotterdam.csv", sep = ";")
locationPortSight<-data.table(locationPortSight)
distanceUsableSensor<-5000

Sys.setenv(TZ = "UTC")

dbConfig <- fromJSON("config.json")

con <- dbConnect(RPostgreSQL::PostgreSQL(),
                 dbname = "FOGDB",
                 host = dbConfig[["host"]], port = 9418,
                 user = dbConfig[["user"]], password = dbConfig[["pw"]])


camerasTable <- as.data.table(dbReadTable(con, "cameras"))
locationsTable <- as.data.table(dbReadTable(con, "locations"))
dbDisconnect(con)


locationsPortHW<-locationsTable[location_id>=371 & location_id<=397]
locationsPortHW<-data.table(locationsPortHW)

matHW<-cbind(locationsPortHW$longitude,locationsPortHW$latitude)
matHW<-matrix(matHW,ncol = 2)
matSensors<-cbind(locationPortSight$lon,locationPortSight$lat)
matSensors<-matrix(matSensors,ncol=2)
distm(matSensors,matHW)
distances<-distm(matSensors,matHW)
rownames(distances)<-locationPortSight$KIS_ID
colnames(distances)<-locationsPortHW$location_id
distances[distances>distanceUsableSensor]<-NA
distancesDT<-data.table(distances)

distancesDT


#get the rownames
KISstations<-rownames(x = distances)[apply(distances, 2, function(x){ if (all(is.na(x))) {NA}  else {which.min(x)}})]
locationIDsHW<-colnames(distances)
locationAndMeteoStation<-cbind(KISstations,locationIDsHW)
locationAndMeteoStation<-locationAndMeteoStation[complete.cases(locationAndMeteoStation),]
locationAndMeteoStation<-data.table(locationAndMeteoStation)

  # test<-locationsPortHW[rep(seq_len(nrow(locationsPortHW)), 7), ]
# test2<-locationPortSight[rep(seq_len(nrow(locationPortSight)), 27), ]
# fullData<-cbind(test,test2)
# 
# distance<-distm(as.matrix(cbind(fullData$longitude, fullData$latitude),ncol=2),as.matrix(cbind(fullData$lon,fullData$lat),ncol=2), fun = distHaversine)


