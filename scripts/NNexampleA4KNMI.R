library(foreach)
library(doParallel)
library(imager)
library(nnet)
library(data.table)
library(DBI)
library(jsonlite)
library(caret)
library(darch)
library(stringr)


imagesAndMeteo<-function(dfImages, dfMeteo){
  imagesDayLight<-data.table(dfImages)
  imagesDayLight[,timeSyncToMeteo:=strptime("1970-01-01", "%Y-%m-%d", tz="UTC") + round(as.numeric(timestamp)/600)*600]
  
  tableMeteo<-data.table(dfMeteo)
  
  setkey(imagesDayLight, timeSyncToMeteo)
  setkey(tableMeteo, timestamp)
  
  imagesAndMOR<-tableMeteo[imagesDayLight]
  
  imagesAndMOR[,foggy:=mor_visibility<=250]
  
  imagesAndMOR
}














setwd("~/development/fogDec/")
dbConfig <- fromJSON("config.json")

resolutionImg<-28

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


imagesDeBilt<-dbGetQuery(con, "SELECT images.image_id, images.filepath, images.timestamp, images.day_phase
                                FROM images
                                WHERE camera_id IN (1,99) AND day_phase=1 AND timestamp<'2017-08-29 00:00:00';")

conditionsDeBilt <- dbGetQuery(con, "SELECT location_id, timestamp, mor_visibility 
                                FROM meteo_features_stations 
                                WHERE location_id =1 AND timestamp<'2017-08-29 00:00:00';")

imagesCabauw<-dbGetQuery(con, "SELECT images.image_id, images.filepath, images.timestamp, images.day_phase
                                FROM images
                                WHERE camera_id IN (2,3) AND day_phase=1 AND timestamp<'2017-08-29 00:00:00';")

conditionsCabauw <- dbGetQuery(con, "SELECT location_id, timestamp, mor_visibility 
                                FROM meteo_features_stations 
                                WHERE location_id =3 AND timestamp<'2017-08-29 00:00:00';")

imagesEelde<-dbGetQuery(con, "SELECT images.image_id, images.filepath, images.timestamp, images.day_phase
                                FROM images
                                WHERE camera_id IN (11,12) AND day_phase=1 AND timestamp<'2017-08-29 00:00:00';")

conditionsEelde <- dbGetQuery(con, "SELECT location_id, timestamp, mor_visibility 
                                FROM meteo_features_stations 
                                WHERE location_id =7 AND timestamp<'2017-08-29 00:00:00';")

imagesSchiphol<-dbGetQuery(con, "SELECT images.image_id, images.filepath, images.timestamp, images.day_phase
                                FROM images
                                WHERE camera_id=15 AND day_phase=1 AND timestamp<'2017-08-29 00:00:00';")

imagesRotterdam<-dbGetQuery(con, "SELECT images.image_id, images.filepath, images.timestamp, images.day_phase
                                FROM images
                                WHERE camera_id IN (13,14) AND day_phase=1 AND timestamp<'2017-08-29 00:00:00';")

conditionsRotterdam <- dbGetQuery(con, "SELECT location_id, timestamp, mor_visibility 
                                FROM meteo_features_stations 
                                WHERE location_id =8 AND timestamp<'2017-08-29 00:00:00';")






dbDisconnect(con)


mergedA4Schiphol<-imagesAndMeteo(imagesRWSDayLight, conditionsFogSchiphol)
mergedDeBilt<-imagesAndMeteo(imagesDeBilt, conditionsDeBilt)
mergedCabauw<-imagesAndMeteo(imagesCabauw, conditionsCabauw)
mergedEelde<-imagesAndMeteo(imagesEelde, conditionsEelde)
mergedRotterdam<-imagesAndMeteo(imagesRotterdam, conditionsRotterdam)
mergedSchipholAirport<-imagesAndMeteo(imagesSchiphol, conditionsFogSchiphol)

total<-rbind(mergedA4Schiphol,mergedDeBilt, mergedCabauw, mergedEelde)


# training<-total[foggy==TRUE]
# 
# 
# set.seed(11)
# 
# training<-rbind(training,total[sample(nrow(total[foggy==FALSE]),1000)])

inTrain<-createDataPartition(total$foggy, p=0.7, list = FALSE)

training<-total[inTrain,]


files<-sapply(training$filepath, function(x) gsub(".*/AXIS214/", "oldArchiveDEBILT/",x))
files<-sapply(files, function(x) gsub(".*/CAMERA/", "",x))
files<-sapply(files, function(x) gsub(".*/cabauw/", "oldArchiveCABAUW/cabauw/",x))






# 
# files<-sapply(training$filepath, strsplit, "/CAMERA/",simplify = T)
# #files<-sapply(files, "[[", 10)
# 
# 
# files<-sapply(files, str_replace_all, "/AXIS214/",  "/AXIS214/oldArchiveDEBILT/",simplify = T )
# files<-sapply(files, strsplit, "/AXIS214/",simplify = T)
# files<-unlist(files)
# 
# files<-files[c(FALSE, TRUE)]



setwd("~/share/")

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
    v<-NA*1:(resolutionImg*resolutionImg)
    message("Image not available error in acquisition")
    v
    }else{
  image<-resize(image,resolutionImg,resolutionImg)
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





#feats <- names(dtMat)
#feats <- feats[-length(feats)]
##feats<-feats[1:30]

## Concatenate strings
#f <- paste(feats,collapse=' + ')
#f <- paste('foggy ~',f)

#f <- as.formula(f)


#net<-nnet(f,dtMat,size=3, MaxNWts=55000, maxit=300)


complete<-dtMat[complete.cases(dtMat)]

lastFeature<-resolutionImg*resolutionImg*3

trainData<-complete[,1:lastFeature]
groundTruth<-lastFeature+1
trainTargets<-complete[,groundTruth:groundTruth]

darch  <- darch(trainData, trainTargets, rbm.numEpochs = 0, rbm.batchSize = 100, rbm.trainOutputLayer = F, layers = c(500,200,100,10), darch.batchSize = 100, darch.learnRate = 2, darch.retainData = F, darch.numEpochs = 1500 )





predictedRWS<-predict(darch,matRWS, type = "bin")
  #predict(net,matRWS)
# 
predictedRWS<-data.table(predictedRWS)
# 
# #predictedRWS[,predictedLabels:=colnames(predictedRWS)[max.col(predictedRWS, ties.method = "first")]]
predictedRWS[,fog:=V2>0]
predictedRWS[,file:=files]



confusion<-data.table(predicted=predictedRWS$fog,fogSensor=dtMat$foggy)

table(confusion$predicted,confusion$fog)

confusionMatrix(confusion$predicted,confusion$fog, mode = "prec_recall", positive = "TRUE")




#######################TEST-SET######################################
set.seed(11)
#testSet<-total[sample(nrow(total),10000)]

testSet<-total[-inTrain,]


filesTest<-sapply(testSet$filepath, function(x) gsub(".*/AXIS214/", "oldArchiveDEBILT/",x))
filesTest<-sapply(filesTest, function(x) gsub(".*/CAMERA/", "",x))
filesTest<-sapply(filesTest, function(x) gsub(".*/cabauw/", "oldArchiveCABAUW/cabauw/",x))




# filesTest<-sapply(testSet$filepath, strsplit, "/CAMERA/",simplify = T)
# #files<-sapply(files, "[[", 10)
# 
# 
# filesTest<-sapply(filesTest, str_replace_all, "/AXIS214/",  "/AXIS214/oldArchiveDEBILT/",simplify = T )
# filesTest<-sapply(filesTest, strsplit, "/AXIS214/",simplify = T)
# filesTest<-unlist(filesTest)
# 
# filesTest<-filesTest[c(FALSE, TRUE)]



setwd("~/share/")





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
    v<-NA*1:(resolutionImg*resolutionImg)
    message("Image not available error in acquisition")
    v
  }else{
    image<-resize(image,resolutionImg,resolutionImg)
    image<-blur_anisotropic(image, amplitude = 10000)
    df<-as.data.frame(image)
    v<-df$value
    #mat<-rbind(mat,v)
    v
  }
}

stopCluster(cl)



predictedRWSTest<-predict(darch,matRWSTest, type = "bin")#predict(net,matRWSTest)
# 
predictedRWSTest<-data.table(predictedRWSTest)
# 
# #predictedRWS[,predictedLabels:=colnames(predictedRWS)[max.col(predictedRWS, ties.method = "first")]]
predictedRWSTest[,fog:=V2>0]
predictedRWSTest[,file:=filesTest]



confusionTest<-data.table(predicted=predictedRWSTest$fog,fogSensor=testSet$foggy)

table(confusionTest$predicted,confusionTest$fog)

confusionMatrix(confusionTest$predicted,confusionTest$fog, mode = "prec_recall", positive = "TRUE")



