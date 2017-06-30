library(DBI)
library(jsonlite)
library(data.table)
library(caret)


#setwd("shiny/")
makeModelRDS<-function(){
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

rf<-randomForest(foggy~mean_edge+change_point+smoothness+fractal_dim+mean_hue+mean_saturation+mean_brightness, data=train, method="rf",na.action=na.exclude)

#rf <- train(foggy~mean_edge+change_point+smoothness+fractal_dim+mean_hue+mean_saturation+mean_brightness, data=train, method="rf",na.action=na.exclude)

prova<-predict(rf, test)
confusionMatrix(prova, test$foggy,mode = "prec_recall", positive = "TRUE")



saveRDS(rf,"rfModel.RDS")
}




gatherFeaturesForImages<-function(){
  files<-list.files("picturesForApp/", pattern = "jpg")
  
  dbConfig <- fromJSON("../config.json")
  
  
  
  con <- dbConnect(RPostgreSQL::PostgreSQL(),
                   dbname = "FOGDB",
                   host = dbConfig[["host"]], port = 9418,
                   user = dbConfig[["user"]], password = dbConfig[["pw"]])
  
  
  results<-rbindlist(lapply(files, queryMetrics, con))
  
  dbDisconnect(con)
  
  results<-data.table(results)
  results <- within(results,rm(timestamp.y,camera_id.y))
  setnames(results,"timestamp.x","timestamp")
  setnames(results,"camera_id.x","camera_id")
  
  
  getFileName<-function(string){
    splits<-strsplit(string, "/")
    out<-tail(splits,1)
    out
  }
  
  paths<-results[filepath]
  
  results[,filename:=getFileName(filepath)]
  
  #lapply(targetPicturesFeatures[,filepath],getFileName)
  
  
  saveRDS(results, "targetPicturesFeatures.RDS")  
}



#quick-and-dirty method to get the features for the filenames passed
queryMetrics<-function(fileName,con){
  fileName<-paste0("%", fileName)
  
  
  queryImagesTable <- paste0("SELECT * from images where filepath LIKE '",fileName,"';")
  targetPicturesImagesTb <- dbGetQuery(con, queryImagesTable)
  
  id<-targetPicturesImagesTb$image_id
  
  queryFeaturesTable<- paste0("SELECT * from image_features
WHERE image_id =",id, ";")
  targetPicturesFeatures <- dbGetQuery(con, queryFeaturesTable)
  
  total <- merge(targetPicturesImagesTb, targetPicturesFeatures ,by="image_id")
}









