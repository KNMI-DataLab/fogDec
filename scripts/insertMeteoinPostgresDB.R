library(DBI)
library(jsonlite)
library(data.table)
library(knmiR)








getTemp<-function(x, location){
  if(location==1){
  value<-KIS(var = "MOR_10", '260_A_a', as.character(x)) #DeBilt
  }
  if(location==3){
  value<-KIS(var = "MOR_10", '348_A_a', as.character(x))#Cabauw
  }
  if(location==7){
    value<-KIS(var = "MOR_10", '280_A_23t', as.character(x))#Eelde
    
  }
  #if(location==3){
   # value<-KIS(var = "MOR_10", '348_A_a', as.character(x))
    
  #}
  value
}



prepareMeteoTable<-function(location){


dbConfig <- fromJSON("config.json")

con <- dbConnect(RPostgreSQL::PostgreSQL(),
                 dbname = "FOGDB",
                 host = dbConfig[["host"]], port = 9418,
                 user = dbConfig[["user"]], password = dbConfig[["pw"]])



imagesTable <- as.data.table(dbReadTable(con, "images"))

camerasTable <- as.data.table(dbReadTable(con, "cameras"))

locationsTable <- as.data.table(dbReadTable(con, "locations"))

meteoFeaturesTable <- as.data.table(dbReadTable(con, "meteo_features_stations"))


dbDisconnect(con)


setkey(camerasTable,location_id)

setkey(locationsTable,location_id)


setkey(imagesTable,camera_id)


tempTB<-camerasTable[locationsTable]

setkey(tempTB,camera_id)

tempTB<-imagesTable[tempTB]

setkey(tempTB,timestamp,location_id)

setkey(meteoFeaturesTable, timestamp,location_id)

temp2<-tempTB[meteoFeaturesTable]

dataNoMeteoAll<-tempTB[!(which(tempTB$image_id %in% temp2$image_id))]

dataNoMeteoDebilt<-dataNoMeteoAll[location_id == location, location_id,timestamp]

datesRequired<-unique(as.Date(dataNoMeteoDebilt[,timestamp]))



today<-as.Date(Sys.time())

datesToFetch<-datesRequired[datesRequired != today]

values<-lapply(datesToFetch, getTemp, location)
values<-rbindlist(values)



values[TOA.MOR_10  == -1, TOA.MOR_10  := NA]
#sensorData[, hhmmss := CorrectOurs(hhmmss)]
values[, IT_DATETIME := as.POSIXct(values[, IT_DATETIME], format = "%Y%m%d_%H%M%S", tz = "UTC")]
setnames(values, "IT_DATETIME", "timestamp")
tmp <- within(values, rm(DS_CODE, "TOA.Q_MOR_10"))
setnames(tmp,"TOA.MOR_10" ,"mor_visibility")
tmp[,location_id:=location]
}


dbWriteTable(con, "meteo_features_stations", tmp, append = TRUE, row.names = FALSE, match.cols = TRUE)


