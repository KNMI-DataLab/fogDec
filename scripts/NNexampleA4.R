library(foreach)
library(doParallel)
library(imager)
library(nnet)
library(data.table)
library(DBI)
library(jsonlite)
library(caret)



setwd("~/development/fogDec/")
dbConfig <- fromJSON("config.json")

con <- dbConnect(RPostgreSQL::PostgreSQL(),
                 dbname = "FOGDB",
                 host = dbConfig[["host"]], port = 9418,
                 user = dbConfig[["user"]], password = dbConfig[["pw"]])



imagesRWSDayLight <- dbGetQuery(con, "SELECT images.image_id, images.filepath, images.timestamp, images.day_phase
                                FROM images
                                WHERE camera_id IN (370, 378, 377, 376, 369, 358, 359, 360, 361) AND day_phase=1 AND timestamp<'2017-08-29 00:00:00';")


conditionsFogSchiphol <- dbGetQuery(con, "SELECT location_id, timestamp, mor_visibility 
                                FROM meteo_features_stations 
                                WHERE location_id =9 AND timestamp<'2017-08-29 00:00:00';")

dbDisconnect(con)

imagesRWSDayLight<-data.table(imagesRWSDayLight)
imagesRWSDayLight[,timeSyncToMeteo:=strptime("1970-01-01", "%Y-%m-%d", tz="UTC") + round(as.numeric(timestamp)/600)*600]

conditionsFogSchiphol<-data.table(conditionsFogSchiphol)

setkey(imagesRWSDayLight, timeSyncToMeteo)
setkey(conditionsFogSchiphol, timestamp)

imagesAndMOR<-conditionsFogSchiphol[imagesRWSDayLight]

imagesAndMOR[,foggy:=mor_visibility<=250]





training<-imagesAndMOR[foggy==TRUE]


set.seed(11)

training<-rbind(training,imagesAndMOR[sample(nrow(imagesAndMOR[foggy==FALSE]),400)])



files<-sapply(training$filepath, strsplit, "/")
files<-sapply(files, "[[", 10)


setwd("~/images/RWS/A4Schiphol/")

#files<-test




cl <- makeCluster(16)
registerDoParallel(cl)

clusterEvalQ(cl, library("imager"))

matRWS<-foreach(i=1:length(files), .combine = rbind) %dopar%{
  message(files[[i]])
  image<-tryCatch(
    load.image(files[[i]]),
    
    error=function(error_message) {
      #message("Yet another error message.")
      #message("Here is the actual R error message:")
      #next
      return(NA)
    }
  )
  if(is.na(image[[1]])){
    v<-NA*1:(32*32)
    message("Image not available error in acquisition")
    v
    }else{
  image<-resize(image,32,32)
  image<-blur_anisotropic(image, amplitude = 10000)
  df<-as.data.frame(image)
  v<-df$value
  #mat<-rbind(mat,v)
  v
  }
}

stopCluster(cl)


dtMat<-data.table(matRWS)
#dtMat[,vis_class:=res2$vis_class]
dtMat[,foggy:=training$foggy]





feats <- names(dtMat)
feats <- feats[-length(feats)]
#feats<-feats[1:30]

# Concatenate strings
f <- paste(feats,collapse=' + ')
f <- paste('foggy ~',f)

f <- as.formula(f)


net<-nnet(f,dtMat,size=3, MaxNWts=55000, maxit=300)


predictedRWS<-predict(net,matRWS)
# 
predictedRWS<-data.table(predictedRWS)
# 
# #predictedRWS[,predictedLabels:=colnames(predictedRWS)[max.col(predictedRWS, ties.method = "first")]]
predictedRWS[,fog:=V1>0.4]
predictedRWS[,file:=files]



confusion<-data.table(predicted=predictedRWS$fog,fogSensor=dtMat$foggy)

table(confusion$predicted,confusion$fog)

confusionMatrix(confusion$predicted,confusion$fog, mode = "prec_recall", positive = "TRUE")




#######################TEST-SET######################################

testSet<-imagesAndMOR[sample(nrow(imagesAndMOR),1000)]


filesTest<-sapply(testSet$filepath, strsplit, "/")
filesTest<-sapply(filesTest, "[[", 10)


setwd("~/images/RWS/A4Schiphol/")

#files<-test




cl <- makeCluster(16)
registerDoParallel(cl)

clusterEvalQ(cl, library("imager"))

matRWSTest<-foreach(i=1:length(filesTest), .combine = rbind) %dopar%{
  message(filesTest[[i]])
  image<-tryCatch(
    load.image(filesTest[[i]]),
    
    error=function(error_message) {
      #message("Yet another error message.")
      #message("Here is the actual R error message:")
      #next
      return(NA)
    }
  )
  if(is.na(image[[1]])){
    v<-NA*1:(32*32)
    message("Image not available error in acquisition")
    v
  }else{
    image<-resize(image,32,32)
    image<-blur_anisotropic(image, amplitude = 10000)
    df<-as.data.frame(image)
    v<-df$value
    #mat<-rbind(mat,v)
    v
  }
}

stopCluster(cl)



predictedRWSTest<-predict(net,matRWSTest)
# 
predictedRWSTest<-data.table(predictedRWSTest)
# 
# #predictedRWS[,predictedLabels:=colnames(predictedRWS)[max.col(predictedRWS, ties.method = "first")]]
predictedRWSTest[,fog:=V1>0.1]
predictedRWSTest[,file:=filesTest]



confusionTest<-data.table(predicted=predictedRWSTest$fog,fogSensor=testSet$foggy)

table(confusionTest$predicted,confusionTest$fog)

confusionMatrix(confusionTest$predicted,confusionTest$fog, mode = "prec_recall", positive = "TRUE")



