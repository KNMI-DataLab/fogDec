library(DBI)
library(jsonlite)
library(data.table)
library(knmiR)








getTemp<-function(x){
  value<-KIS(var = "MOR_10", '260_A_a', as.character(x))
  value
}






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

dataNoMeteoDebilt<-dataNoMeteoAll[location_id == 1, location_id,timestamp]

datesRequired<-unique(as.Date(dataNoMeteoDebilt[,timestamp]))



today<-as.Date(Sys.time())

datesToFetch<-datesRequired[datesRequired != today]

values<-lapply(datesToFetch, getTemp)
values<-rbindlist(values)







# insertionDayPhase <- dbGetQuery(con, "INSERT INTO day_phases ( day_phase_id, day_phase_description)
#                                                   VALUES (0, 'night'), (1, 'day'), (10, 'civil dawn'),
#                                                   (11, 'civil dusk'), (20, 'nautical dawn'), (21, 'nautical dusk'),
#                                                   (30, 'astronomical dawn'), (31, 'astronomical dusk');")




###
###
###
