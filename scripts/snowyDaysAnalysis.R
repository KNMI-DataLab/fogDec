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



draw_confusion_matrix <- function(cm) {
  
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














setwd("~/development/fogDec/")
dbConfig <- fromJSON("config.json")

resolutionImg<-28

con <- dbConnect(RPostgreSQL::PostgreSQL(),
                 dbname = "FOGDB",
                 host = dbConfig[["host"]], port = 9418,
                 user = dbConfig[["user"]], password = dbConfig[["pw"]])

dateBeginning<-"\'2017-12-08 00:00:00\'"
dateEnd<-"\'2017-12-12 00:00:00\'"



cameras<-dbReadTable(con, "cameras")
meteoStations<-dbReadTable(con,"meteo_stations")


camerasRWSCoupledMeteo <- dbGetQuery(con, paste0("SELECT * FROM cameras
                                                 WHERE location_id IN (", paste(coupling$locationIDsHW, collapse=", "), ");"))




imagesRWSDayLight <- dbGetQuery(con, paste0("SELECT images.image_id, images.filepath, images.timestamp, images.day_phase, images.camera_id
                                            FROM images
                                            WHERE camera_id IN (", paste(camerasRWSCoupledMeteo$camera_id, collapse=", "), ")AND day_phase=1 AND timestamp<", dateEnd,"AND timestamp>", dateBeginning, ";"))

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
                                          WHERE location_id IN (", paste(meteoStations$location_id, collapse=", "), ") AND timestamp<", dateEnd,"AND timestamp>", dateBeginning,";"))

meteoConditions<-data.table(meteoConditions)
dbDisconnect(con)


mergedRWSandKNMIstations<-imagesAndMeteoGeneral(full, meteoConditions)





total<-mergedRWSandKNMIstations




#total<-rbind(mergedA4Schiphol,mergedDeBilt, mergedCabauw, mergedEelde)


set.seed(11)

foggyData<-total[foggy==TRUE]



nonFoggyData<-total[foggy==FALSE]





files<-sapply(total$filepath, function(x) gsub(".*/AXIS214/", "oldArchiveDEBILT/",x))
files<-sapply(files, function(x) gsub(".*/CAMERA/", "",x))
files<-sapply(files, function(x) gsub(".*/cabauw/", "oldArchiveCABAUW/cabauw/",x))





setwd("~/share/")

#files<-test




cl <- makeCluster(16)
registerDoParallel(cl)

clusterEvalQ(cl, library("imager"))

matSnowyDaysRWS<-foreach(i=1:length(files), .combine = rbind) %dopar%{
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


dtMatSnowy<-data.table(matSnowyDaysRWS)
#dtMat[,vis_class:=res2$vis_class]
dtMatSnowy[,foggy:=total$foggy]




complete<-dtMatSnowy[complete.cases(dtMatSnowy)]

lastFeature<-resolutionImg*resolutionImg*3

trainData<-complete[,1:lastFeature]
groundTruth<-lastFeature+1
trainTargets<-complete[,groundTruth:groundTruth]



modelBeforeSnow<-readRDS("~/development/fogNNmodels/NNmodelTrainedWithStationCoupling21122017Tillsnow.RDS")



predictedRWSBeforeSnow<-predict(modelBeforeSnow,matSnowyDaysRWS, type = "bin")
#predict(net,matRWS)
# 
predictedRWSBeforeSnow<-data.table(predictedRWSBeforeSnow)
# 
# #predictedRWS[,predictedLabels:=colnames(predictedRWS)[max.col(predictedRWS, ties.method = "first")]]
predictedRWSBeforeSnow[,fog:=V2>0]
predictedRWSBeforeSnow[,file:=files]



confusion<-data.table(predicted=predictedRWSBeforeSnow$fog,fogSensor=dtMatSnowy$foggy)

table(confusion$predicted,confusion$fog)

cmBeforeSnow<-confusionMatrix(confusion$predicted,confusion$fog, mode = "prec_recall", positive = "TRUE")



modelAfterSnow<-readRDS("~/development/fogNNmodels/NNmodelTrainedWithStationCoupling22122017Tillsnow.RDS")

predictedRWSAfterSnow<-predict(modelAfterSnow,matSnowyDaysRWS, type = "bin")
#predict(net,matRWS)
# 
predictedRWSAfterSnow<-data.table(predictedRWSAfterSnow)
# 
# #predictedRWS[,predictedLabels:=colnames(predictedRWS)[max.col(predictedRWS, ties.method = "first")]]
predictedRWSAfterSnow[,fog:=V2>0]
predictedRWSAfterSnow[,file:=files]



confusion<-data.table(predicted=predictedRWSAfterSnow$fog,fogSensor=dtMatSnowy$foggy)

table(confusion$predicted,confusion$fog)

cmAfterSnow<-confusionMatrix(confusion$predicted,confusion$fog, mode = "prec_recall", positive = "TRUE")

draw_confusion_matrix(cmAfterSnow)


###### let's train and test with this special dataset in the training (a part of it) and test set (a part of it)#####


set.seed(11)

foggyData<-total[foggy==TRUE]
inTraining<-sample(nrow(foggyData),0.7*nrow(foggyData))
training<-foggyData[inTraining]



nonFoggyData<-total[foggy==FALSE]
inTrainingNonFog<-sample(nrow(nonFoggyData),0.5*nrow(nonFoggyData))
training<-rbind(training,nonFoggyData[inTrainingNonFog])



testing<-foggyData[-inTraining]
testing<-rbind(testing,nonFoggyData[-inTrainingNonFog]) #all the non foggy pics from the snowy days are either in the training or test set



dateLastDayMeteo<-"\'2017-12-21 00:00:00\'"


#getting the other images have to enrich the training set with


con <- dbConnect(RPostgreSQL::PostgreSQL(),
                 dbname = "FOGDB",
                 host = dbConfig[["host"]], port = 9418,
                 user = dbConfig[["user"]], password = dbConfig[["pw"]])




imagesRWSDayLight <- dbGetQuery(con, paste0("SELECT images.image_id, images.filepath, images.timestamp, images.day_phase, images.camera_id
                                            FROM images
                                            WHERE camera_id IN (", paste(camerasRWSCoupledMeteo$camera_id, collapse=", "), ")AND day_phase=1 AND timestamp<", dateLastDayMeteo, ";"))

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
                                          WHERE location_id IN (", paste(meteoStations$location_id, collapse=", "), ") AND timestamp<", dateLastDayMeteo,";"))

meteoConditions<-data.table(meteoConditions)
dbDisconnect(con)


mergedRWSandKNMIstations<-imagesAndMeteoGeneral(full, meteoConditions)


total<-mergedRWSandKNMIstations




#total<-rbind(mergedA4Schiphol,mergedDeBilt, mergedCabauw, mergedEelde)


#have to adjust the addition of foggy images to the training set to match the balanced training set given the images non foggy of the snowy days
set.seed(11)

foggyData<-total[foggy==TRUE]
inTraining<-sample(nrow(foggyData),0.7*nrow(foggyData))
training<-foggyData[inTraining]



nonFoggyData<-total[foggy==FALSE]
training<-rbind(training,nonFoggyData[sample(nrow(nonFoggyData),nrow(training))])



testing<-foggyData[-inTraining]
testing<-rbind(testing,nonFoggyData[sample(nrow(nonFoggyData),4000)])







