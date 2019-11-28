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
library(fogDec)
library(dplyr)


setwd("~/development/fogDec/")

#the coupling contains also the locations of the station itself of course
coupling<-coupleCamerasAndKNMInearStations(maxDistance = 7500)


draw_confusion_matrix_binary <- function(cm) {
   
     layout(matrix(c(1,1,2)))
     par(mar=c(2,2,2,2))
     plot(c(100, 345), c(300, 450), type = "n", xlab="", ylab="", xaxt='n', yaxt='n')
     title('CONFUSION MATRIX', cex.main=2)
   
     # create the matrix
     rect(150, 430, 240, 370, col='#3F97D0')
     text(195, 435, 'FALSE', cex=1.2)
     rect(250, 430, 340, 370, col='#F7AD50')
     text(295, 435, 'TRUE', cex=1.2)
     text(125, 370, 'Predicted', cex=1.3, srt=90, font=2)
     text(245, 450, 'Actual', cex=1.3, font=2)
     rect(150, 305, 240, 365, col='#F7AD50')
     rect(250, 305, 340, 365, col='#3F97D0')
     text(140, 400, 'FALSE', cex=1.2, srt=90)
     text(140, 335, 'TRUE', cex=1.2, srt=90)
   
     # add in the cm results
     res <- as.numeric(cm$table)
     text(195, 400, res[1], cex=1.6, font=2, col='white')
     text(195, 335, res[2], cex=1.6, font=2, col='white')
     text(295, 400, res[3], cex=1.6, font=2, col='white')
     text(295, 335, res[4], cex=1.6, font=2, col='white')
   
     # add in the specifics
     plot(c(100, 0), c(100, 0), type = "n", xlab="", ylab="", main = "DETAILS", xaxt='n', yaxt='n')
     text(10, 85, names(cm$byClass[1]), cex=1.2, font=2)
     text(10, 70, round(as.numeric(cm$byClass[1]), 3), cex=1.2)
     text(30, 85, names(cm$byClass[2]), cex=1.2, font=2)
     text(30, 70, round(as.numeric(cm$byClass[2]), 3), cex=1.2)
     text(50, 85, names(cm$byClass[5]), cex=1.2, font=2)
     text(50, 70, round(as.numeric(cm$byClass[5]), 3), cex=1.2)
     text(70, 85, names(cm$byClass[6]), cex=1.2, font=2)
     text(70, 70, round(as.numeric(cm$byClass[6]), 3), cex=1.2)
     text(90, 85, names(cm$byClass[7]), cex=1.2, font=2)
     text(90, 70, round(as.numeric(cm$byClass[7]), 3), cex=1.2)
   
     # add in the accuracy information
     text(50, 35, names(cm$overall[1]), cex=1.5, font=2)
     text(50, 20, round(as.numeric(cm$overall[1]), 3), cex=1.4)
     #text(70, 35, names(cm$overall[2]), cex=1.5, font=2)
     #text(70, 20, round(as.numeric(cm$overall[2]), 3), cex=1.4)
   }


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



dateStr<-"\'2018-02-28 00:00:00\'"
resolutionImg<-28


coupleImagesAndMeteoToDate<-function(dateStr){
setwd("~/development/fogDec/")
dbConfig <- fromJSON("config.json")


con <- dbConnect(RPostgreSQL::PostgreSQL(),
                 dbname = "FOGDB",
                 host = dbConfig[["host"]], port = 9418,
                 user = dbConfig[["user"]], password = dbConfig[["pw"]])




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
#######TEST TABLE
meteoConditions <- dbGetQuery(con, paste0("SELECT location_id, timestamp, mor_visibility 
                                FROM meteo_features_copy  
                                WHERE location_id IN (", paste(meteoStations$location_id, collapse=", "), ") AND timestamp<", dateStr,";"))

meteoConditions<-data.table(meteoConditions)
dbDisconnect(con)


mergedRWSandKNMIstations<-imagesAndMeteoGeneral(full, meteoConditions)



total<-mergedRWSandKNMIstations
total
}



evaluateModel<-function(model,dataSet,resolutionImg=28)
{
  set.seed(11)
  #testSet<-total[sample(nrow(total),10000)]
  
  #testSet<-total[-inTrain,]
  
  
  fileList<-sapply(dataSet$filepath, function(x) gsub(".*/AXIS214/", "oldArchiveDEBILT/",x))
  fileList<-sapply(fileList, function(x) gsub(".*/CAMERA/", "",x))
  fileList<-sapply(fileList, function(x) gsub(".*/cabauw/", "oldArchiveCABAUW/cabauw/",x))
  
  #saveRDS(fileList,"~/development/fogNNmodels/filenamesTestEGU.RDS")
  
  
  setwd("~/share/")
  
  cores<-40
  cl <- makeCluster(40)
  registerDoParallel(cl)
  
  clusterEvalQ(cl, library("imager"))
  
  matRWSTest<-foreach(i=1:length(fileList)) %dopar%{
    message(fileList[[i]])
    image<-tryCatch(
      load.image(fileList[[i]]),
      
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
  
  matRWSTest<-do.call(rbind,matRWSTest)
  
  
  #not inside the function
  #saveRDS(matRWSTest,"~/development/fogNNmodels/testingDataMatEGU.RDS")
  
  
  
  predictedRWSTest<-predict(model,matRWSTest, type = "bin")
  
  predictedRWSTest<-data.table(predictedRWSTest)
  
  predictedRWSTest[,fog:=V2>0]
  predictedRWSTest[,file:=fileList]
  
  
  
  confusionTest<-data.table(predicted=predictedRWSTest$fog,fogSensor=dataSet$foggy)
  
  table(confusionTest$predicted,confusionTest$fogSensor)
  
  confMatrixTest<-confusionMatrix(confusionTest$predicted,confusionTest$fog, mode = "prec_recall", positive = "TRUE")
  
  
  draw_confusion_matrix_binary(confMatrixTest)
  confMatrixTest
}


total<-coupleImagesAndMeteoToDate(dateStr)

set.seed(11)


nFog<-dim(total[foggy==TRUE])[[1]]
nNonFog<-dim(total[foggy==FALSE])[[1]]

fogNonFogRatio<-nFog/nNonFog


#FOGGY CASES
#####TRAINING

foggyData<-total[foggy==TRUE]
inTraining<-sample(nrow(foggyData),0.6*nrow(foggyData)/2)
trainingSmall<-foggyData[inTraining]
### SMALL FOG DTATASET FOR EXPERIMENT 
#inTrainingMore<-sample(nrow(trainingSmall),100000, replace = T)
training<-trainingSmall#[inTrainingMore]




#####CROSS VALIDATION

remaining<-foggyData[-inTraining]
inCrossVal<-sample(nrow(remaining),0.2*nrow(foggyData)/2)

crossValidating<-remaining[inCrossVal]

#####TEST SET
testing<-remaining[-inCrossVal]


#check that are disjoint datasets
sum(duplicated(rbind(crossValidating,testing)))
sum(duplicated(rbind(unique(training),testing)))
sum(duplicated(rbind(unique(training),crossValidating)))

#####NON-FOGGY CASES
#####TRAINING
nonFoggyData<-total[foggy==FALSE]
inTrainNoFog<-sample(nrow(nonFoggyData), nrow(training)/fogNonFogRatio)
nonFoggyTraining<-nonFoggyData[inTrainNoFog]

#####CROSS VALIDATION
remaining<-nonFoggyData[-inTrainNoFog]
inCrossVal<-sample(nrow(remaining),0.2*nrow(foggyData))

nonFoggyCrossValidating<-remaining[inCrossVal]

######TEST SET
foggyInTest<-dim(testing[foggy==TRUE])[[1]]
nonFoggyForRealisticRatio<-foggyInTest*1/fogNonFogRatio
inTestNoFog<-sample(nrow(remaining[-inCrossVal]),nonFoggyForRealisticRatio)

nonFoggyTesting<-remaining[-inCrossVal][inTestNoFog]


sum(duplicated(rbind(nonFoggyCrossValidating,nonFoggyTesting)))
sum(duplicated(rbind(nonFoggyTraining,nonFoggyTesting)))
sum(duplicated(rbind(nonFoggyTraining,nonFoggyCrossValidating)))



####Binding the fog and non-fog sets with the corresponding
training<-rbind(training,nonFoggyTraining)
crossValidating<-rbind(crossValidating,nonFoggyCrossValidating)
testing<-rbind(testing,nonFoggyTesting)

timeNow<-Sys.time()
timeNowString<-strftime(timeNow , "%Y-%m-%dT%H:%M:%S%Z")


saveRDS(training,paste0("~/development/fogNNmodels/trainingDataLabels",timeNowString,".RDS"))
saveRDS(crossValidating,paste0("~/development/fogNNmodels/crossValidatingDataLabels",timeNowString,".RDS"))
saveRDS(testing,paste0("~/development/fogNNmodels/testingDataLabels",timeNowString,".RDS"))



files<-sapply(training$filepath, function(x) gsub(".*/AXIS214/", "oldArchiveDEBILT/",x))
files<-sapply(files, function(x) gsub(".*/CAMERA/", "",x))
files<-sapply(files, function(x) gsub(".*/cabauw/", "oldArchiveCABAUW/cabauw/",x))


saveRDS(files,paste0("~/development/fogNNmodels/trainingFileNames",timeNowString,".RDS"))


setwd("~/share/")

#files<-test



cores<-60
cl <- makeCluster(cores)
registerDoParallel(cl)

clusterEvalQ(cl, library("imager"))

matRWS<-foreach(i=1:length(files)) %dopar%{
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

matRWS<-do.call(rbind,matRWS)



dtMat<-data.table(matRWS)
#dtMat[,vis_class:=res2$vis_class]
dtMat[,foggy:=training$foggy]



saveRDS(dtMat,paste0("~/development/fogNNmodels/dtMat",timeNowString,".RDS"))



complete<-dtMat[complete.cases(dtMat)]

lastFeature<-resolutionImg*resolutionImg*3

trainData<-complete[,1:lastFeature]
groundTruth<-lastFeature+1
trainTargets<-complete[,groundTruth:groundTruth]




#############GENERATE MODELS WITH SEVERAL PARAMS TO CROSS-VALIDATE################################

m1Int<-darch(trainData, trainTargets, rbm.numEpochs = 0, 
             rbm.batchSize = 1500, rbm.lastLayer = 0, 
             layers = c(2352,500,100,10), darch.batchSize = 500, 
             darch.numEpochs = 250, bp.learnRate = 3,darch.dither = TRUE)





m1<-future({m1Int<-darch(trainData, trainTargets, rbm.numEpochs = 0, 
                     rbm.batchSize = 500, rbm.lastLayer = 0, 
                     layers = c(2352,500,100,10), darch.batchSize = 5000, 
                     darch.numEpochs = 1000, bp.learnRate = 3,darch.dither = TRUE )}) %plan% multiprocess

m2<-future({m2Int<-darch(trainData, trainTargets, rbm.numEpochs = 0, 
                          rbm.batchSize = 500, rbm.lastLayer = 0, 
                          layers = c(2352,500,100,10), darch.batchSize = 750, 
                          darch.numEpochs = 850, bp.learnRate = 7.5,darch.dither = TRUE)}) %plan% multiprocess

m3<-future({darch(trainData, trainTargets, rbm.numEpochs = 0, 
                                         rbm.batchSize = 500, rbm.lastLayer = 0, 
                                         layers = c(2352,500,100,50,10), darch.batchSize = 500, 
                                         darch.numEpochs = 650, bp.learnRate = 3)}) %plan% multiprocess
m4<-future({darch(trainData, trainTargets, rbm.numEpochs = 0, 
                  rbm.batchSize = 500, rbm.lastLayer = 0, 
                  layers = c(2352,500,100,50,10), darch.batchSize = 500, 
                  darch.numEpochs = 650, bp.learnRate =3,darch.dither = TRUE )}) %plan% multiprocess

# m5 <-future({darch(trainData, trainTargets, rbm.numEpochs = 0, 
#                                              rbm.batchSize = 500, rbm.lastLayer = 0, 
#                                              layers = c(2352,1200,500,100,50,10), darch.batchSize = 500, 
#                                              darch.numEpochs = 500, bp.learnRate = 0.5 )}) %plan% multiprocess
# 
# m6 <-future({darch(trainData, trainTargets, rbm.numEpochs = 0, 
#                    rbm.batchSize = 500, rbm.lastLayer = 0, 
#                    layers = c(2352,1200,500,100,50,10), darch.batchSize = 500, 
#                    darch.numEpochs = 500, bp.learnRate = 0.5,darch.dither = TRUE )}) %plan% multiprocess



c1<-evaluateModel(value(m1),crossValidating)
c2<-evaluateModel(value(m2),crossValidating)
c3<-evaluateModel(value(m3),crossValidating)
c4<-evaluateModel(value(m4),crossValidating)
c5<-evaluateModel(value(m5),crossValidating)
c6<-evaluateModel(value(m6),crossValidating)

####TO PUT LATER
###saveRDS(darch1,"~/development/fogNNmodels/NNmodelTrainedWithStationCouplingEGU.RDS")


# saveRDS(matRWS,paste0("~/development/fogNNmodels/trainingDataMat", timeNowString,".RDS"))
# 
# 
# predictedRWS<-predict(darch1,matRWS, type = "bin")
#   #predict(net,matRWS)
# # 
# predictedRWS<-data.table(predictedRWS)
# # 
# # #predictedRWS[,predictedLabels:=colnames(predictedRWS)[max.col(predictedRWS, ties.method = "first")]]
# predictedRWS[,fog:=V2>0]
# predictedRWS[,file:=files]
# 
# 
# 
# confusion<-data.table(predicted=predictedRWS$fog,fogSensor=dtMat$foggy)
# 
# table(confusion$predicted,confusion$fog)
# 
# confMatrixTraining<-confusionMatrix(confusion$predicted,confusion$fog, mode = "prec_recall", positive = "TRUE")
# 
# draw_confusion_matrix_binary(confMatrixTraining)
# 
# 
# #######################TEST-SET######################################

