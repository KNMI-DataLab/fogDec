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



#the coupling contains also the locations of the station itself of course
coupling<-coupleCamerasAndKNMInearStations(maxDistance = 7500)




Sys.setenv(TZ = "UTC")


imagesAndMeteoGeneral<-function(dtImages, dtMeteo){
  #imagesDayLight<-data.table(dfImages)
  dtImages[,timeSyncToMeteo:=strptime("1970-01-01", "%Y-%m-%d", tz="UTC") + round(as.numeric(timestamp)/600)*600]
  
  #tableMeteo<-data.table(dfMeteo)
  
  setkey(dtImages, location_id_closest_KNMI_meteo,timeSyncToMeteo)
  setkey(dtMeteo, location_id,timestamp)
  
  imagesAndMOR<-dtMeteo[dtImages]
  
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

dateStr<-"\'2017-11-01 00:00:00\'"



cameras<-dbReadTable(con, "cameras")
meteoStations<-dbReadTable(con,"meteo_stations")


camerasRWSCoupledMeteo <- dbGetQuery(con, paste0("SELECT * FROM cameras
                                            WHERE location_id IN (", paste(coupling$locationIDsHW, collapse=", "), ");"))




imagesRWSDayLight <- dbGetQuery(con, paste0("SELECT images.image_id, images.filepath, images.timestamp, images.day_phase, images.camera_id
                                FROM images
                                WHERE camera_id IN (", paste(camerasRWSCoupledMeteo$camera_id, collapse=", "), ")AND day_phase=1 AND timestamp<", dateStr,";"))

camerasRWSCoupledMeteo<-data.table(camerasRWSCoupledMeteo)
imagesRWSDayLight<-data.table(imagesRWSDayLight)
setkey(camerasRWSCoupledMeteo,camera_id)
setkey(imagesRWSDayLight,camera_id)

full<-imagesRWSDayLight[camerasRWSCoupledMeteo]

coupling$locationIDsHW<-as.numeric(coupling$locationIDsHW)
setkey(coupling, locationIDsHW)
setkey(full, location_id)
full<-full[coupling, nomatch=0]


meteoStations <- dbGetQuery(con, paste0("SELECT * FROM meteo_stations
                                            WHERE knmi_kis_id IN (", paste(paste0("'",coupling$KISstations,"'"), collapse = ", "), ");"))


meteoStations<-data.table(meteoStations)
setkey(meteoStations,knmi_kis_id)
setkey(full,KISstations)

full<-full[meteoStations,nomatch=0]

setnames(full, old=c("i.location_id"), new=c("location_id_closest_KNMI_meteo"))

#meteoConditions <- dbGetQuery(con, paste0("SELECT * FROM meteo_stations
                               #             WHERE knmi_kis_id IN (", paste(paste0("'",coupling$KISstations,"'"), collapse = ", "), ");"))
meteoConditions <- dbGetQuery(con, paste0("SELECT location_id, timestamp, mor_visibility 
                                FROM meteo_features_stations 
                                WHERE location_id IN (", paste(meteoStations$location_id, collapse=", "), ") AND timestamp<", dateStr,";"))

meteoConditions<-data.table(meteoConditions)
dbDisconnect(con)


mergedRWSandKNMIstations<-imagesAndMeteoGeneral(full, meteoConditions)


##################checked TILL HERE SOLVING THE METEO TABLE FIRST#######################################











total<-rbind(mergedA4Schiphol,mergedDeBilt, mergedCabauw, mergedEelde)

set.seed(11)

foggyData<-total[foggy==TRUE]
inTraining<-sample(nrow(foggyData),0.7*nrow(foggyData))
training<-foggyData[inTraining]



nonFoggyData<-total[foggy==FALSE]
training<-rbind(training,nonFoggyData[sample(nrow(nonFoggyData),nrow(training))])



testing<-foggyData[-inTraining]
testing<-rbind(testing,nonFoggyData[sample(nrow(nonFoggyData),4000)])

#inTrain<-createDataPartition(total$foggy, p=0.7, list = FALSE)

#training<-total[inTrain,]


saveRDS(training,"~/development/fogNNmodels/trainingDataLabels.RDS")

saveRDS(testing,"~/development/fogNNmodels/testingDataLabels.RDS")



files<-sapply(training$filepath, function(x) gsub(".*/AXIS214/", "oldArchiveDEBILT/",x))
files<-sapply(files, function(x) gsub(".*/CAMERA/", "",x))
files<-sapply(files, function(x) gsub(".*/cabauw/", "oldArchiveCABAUW/cabauw/",x))


saveRDS(files,"~/development/fogNNmodels/trainingFileNames.RDS")




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

#darch  <- darch(trainData, trainTargets, rbm.numEpochs = 0, rbm.batchSize = 50, rbm.trainOutputLayer = F, layers = c(2352,500,100,10), darch.batchSize = 50, darch.learnRate = 2, darch.retainData = F, darch.numEpochs = 500 )

#darch1<- darch(trainData, trainTargets, rbm.numEpochs = 0, rbm.batchSize = 50, rbm.trainOutputLayer = F, layers = c(2352,800, 500,100,10), darch.batchSize = 50, darch.learnRate = 2, darch.retainData = F, darch.numEpochs = 800 )
#darch2  <- darch(trainData, trainTargets, rbm.numEpochs = 0, rbm.batchSize = 50, rbm.trainOutputLayer = F, layers = c(2352,1000,800,500,100,10), darch.batchSize = 50, darch.learnRate = 2, darch.retainData = F, darch.numEpochs = 1000 )
#darch3  <- darch(trainData, trainTargets, rbm.numEpochs = 0, rbm.batchSize = 50, rbm.trainOutputLayer = F, layers = c(2352,100,10), darch.batchSize = 50, darch.learnRate = 2, darch.retainData = F, darch.numEpochs = 500 )




saveRDS(matRWS,"~/development/fogNNmodels/trainingDataMat.RDS")


predictedRWS<-predict(darch1,matRWS, type = "bin")
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

#testSet<-total[-inTrain,]


filesTest<-sapply(testing$filepath, function(x) gsub(".*/AXIS214/", "oldArchiveDEBILT/",x))
filesTest<-sapply(filesTest, function(x) gsub(".*/CAMERA/", "",x))
filesTest<-sapply(filesTest, function(x) gsub(".*/cabauw/", "oldArchiveCABAUW/cabauw/",x))

saveRDS(filesTest,"~/development/fogNNmodels/filenamesTest.RDS")



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



saveRDS(matRWSTest,"~/development/fogNNmodels/testingDataMat.RDS")



predictedRWSTest<-predict(darch1,matRWSTest, type = "bin")#predict(net,matRWSTest)
# 
predictedRWSTest<-data.table(predictedRWSTest)
# 
# #predictedRWS[,predictedLabels:=colnames(predictedRWS)[max.col(predictedRWS, ties.method = "first")]]
predictedRWSTest[,fog:=V2>0]
predictedRWSTest[,file:=filesTest]



confusionTest<-data.table(predicted=predictedRWSTest$fog,fogSensor=testing$foggy)

table(confusionTest$predicted,confusionTest$fogSensor)

confusionMatrix(confusionTest$predicted,confusionTest$fog, mode = "prec_recall", positive = "TRUE")




###############################TEST UNKNOWN UNLABELD DATASET##########################


#id non labled data cameraid=276#

con <- dbConnect(RPostgreSQL::PostgreSQL(),
                 dbname = "FOGDB",
                 host = dbConfig[["host"]], port = 9418,
                 user = dbConfig[["user"]], password = dbConfig[["pw"]])



imagesRWSDayLightNoLabel <- dbGetQuery(con, paste0("SELECT images.image_id, images.filepath, images.timestamp, images.day_phase
                                FROM images
                                WHERE camera_id=276 AND day_phase=1 AND timestamp<", dateStr,";"))
dbDisconnect(con)

filesNew<-sapply(imagesRWSDayLightNoLabel$filepath, function(x) gsub(".*/AXIS214/", "oldArchiveDEBILT/",x))
filesNew<-sapply(filesNew, function(x) gsub(".*/CAMERA/", "",x))
filesNew<-sapply(filesNew, function(x) gsub(".*/cabauw/", "oldArchiveCABAUW/cabauw/",x))




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

matRWSNew<-foreach(i=1:length(filesNew), .combine = rbind) %dopar%{
  message(filesNew[[i]])
  image<-tryCatch(
    load.image(filesNew[[i]]),
    
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






predictedRWSNew<-predict(darch1,matRWSNew, type = "bin")#predict(net,matRWSTest)
# 
predictedRWSNew<-data.table(predictedRWSNew)
# 
# #predictedRWS[,predictedLabels:=colnames(predictedRWS)[max.col(predictedRWS, ties.method = "first")]]
predictedRWSNew[,fog:=V2>0]
predictedRWSNew[,file:=filesNew]



#confusionTest<-data.table(predicted=predictedRWSTest$fog,fogSensor=testing$foggy)

#table(confusionTest$predicted,confusionTest$fog)

#confusionMatrix(confusionTest$predicted,confusionTest$fog, mode = "prec_recall", positive = "TRUE")






















