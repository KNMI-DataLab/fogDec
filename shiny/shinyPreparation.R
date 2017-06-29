library(DBI)
library(jsonlite)
library(data.table)
library(caret)






buildModelForShinyApp<-function() {
dbConfig <- fromJSON("../config.json")

con <- dbConnect(RPostgreSQL::PostgreSQL(),
                 dbname = "FOGDB",
                 host = dbConfig[["host"]], port = 9418,
                 user = dbConfig[["user"]], password = dbConfig[["pw"]])



#Get meteo conditions for De Bilt
tableMeteo <- dbGetQuery(con, "SELECT * from meteo_features_stations
                                  WHERE location_id =1;")

#Get features for De Bilt images

tableFeatures <- dbGetQuery(con, "SELECT * from image_features
                                  WHERE camera_id =1;")


tableMeteo <- data.table(tableMeteo)
tableMeteo[, foggy := mor_visibility < 250]


tableFeatures <- data.table(tableFeatures)

setkey(tableFeatures,timestamp)

setkey(tableMeteo,timestamp)




fullTable <- tableFeatures[tableMeteo,]
train <- fullTable[year(timestamp)>=2016,]
test <- fullTable[year(timestamp)<2016,]

train<-train[is.na(foggy)==FALSE]
train[,foggy:= as.factor(foggy)]

test[,foggy:=as.factor(foggy)]

rf <- train(foggy~mean_edge+change_point+smoothness+fractal_dim+mean_hue+mean_saturation+mean_brightness, data=train, method="rf",na.action=na.exclude)

prova<-predict(rf, test)
confusionMatrix(prova, test$foggy,mode = "prec_recall", positive = "TRUE")

return(rf)
}


