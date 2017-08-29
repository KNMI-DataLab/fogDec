library(foreach)
library(doParallel)
library(imager)
library(nnet)
library(data.table)
library(DBI)
library(jsonlite)
library(caret)



dbConfig <- fromJSON("config.json")

con <- dbConnect(RPostgreSQL::PostgreSQL(),
                 dbname = "FOGDB",
                 host = dbConfig[["host"]], port = 9418,
                 user = dbConfig[["user"]], password = dbConfig[["pw"]])



imagesRWSDayLight <- dbGetQuery(con, "SELECT images.image_id, images.filepath, images.timestamp, images.day_phase
                                FROM images
                                WHERE camera_id =370 AND day_phase=1 AND timestamp<'2017-08-29 00:00:00';")



test<-sapply(imagesRWSDayLight$filepath, strsplit, "HM59/")
test<-sapply(test, "[[", 2)


setwd("~/images/RWS/")

files<-test


cl <- makeCluster(16)
registerDoParallel(cl)

clusterEvalQ(cl, library("imager"))

matRWS<-foreach(i=1:length(files), .combine = rbind) %dopar%{
  message(files[[i]])
  image<-load.image(files[[i]])
  image<-resize(image,28,28)
  image<-blur_anisotropic(image, amplitude = 15000)
  df<-as.data.frame(image)
  v<-df$value
  #mat<-rbind(mat,v)
  v
}

stopCluster(cl)





# predictedRWS<-predict(net,matRWS)
# 
# predictedRWS<-data.table(predictedRWS)
# 
# #predictedRWS[,predictedLabels:=colnames(predictedRWS)[max.col(predictedRWS, ties.method = "first")]]
# predictedRWS[,fog:=V1>0.4]
# predictedRWS[,file:=files]














