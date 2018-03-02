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


imagesAndMeteoGeneral<-function(dtImages, dtMeteo, multiCat){
  #imagesDayLight<-data.table(dfImages)
  dtImages[,timeSyncToMeteo:=strptime("1970-01-01", "%Y-%m-%d", tz="UTC") + round(as.numeric(timestamp)/600)*600]
  
  #tableMeteo<-data.table(dfMeteo)
  
  setkey(dtImages, location_id_closest_KNMI_meteo,timeSyncToMeteo)
  setkey(dtMeteo, location_id,timestamp)
  
  imagesAndMOR<-dtMeteo[dtImages]
  
  if (multiCat==TRUE){
    #categories defined by the meeting with meteorologists of the weather room
    imagesAndMOR$visClass<-cut(imagesAndMOR$mor_visibility, c(0,50,200,1000,Inf), right=FALSE, labels=c("A","B","C","D"))
  }else{
    imagesAndMOR[,foggy:=mor_visibility<=250]
  }
  
  # mydata$Agecat4<-cut(mydata$Age, c(0,5,10,15,20,25,30), right=FALSE, labels=c(1:6))
  
  
  
  
  imagesAndMOR
}







resolutionImg<-28

dateStr<-"\'2018-02-28 00:00:00\'"

coupleImagesAndMeteo<-function(lastDateTime){

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
                                WHERE camera_id IN (", paste(camerasRWSCoupledMeteo$camera_id, collapse=", "), ")AND day_phase=1 AND timestamp<", lastDateTime,";"))
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
                                WHERE location_id IN (", paste(meteoStations$location_id, collapse=", "), ") AND timestamp<", lastDateTime,";"))

meteoConditions<-data.table(meteoConditions)
dbDisconnect(con)
multiCat<-TRUE
mergedRWSandKNMIstations<-imagesAndMeteoGeneral(full, meteoConditions,multiCat)
total<-mergedRWSandKNMIstations
total
}




####TRAIN AND TEST SET CREATION####

set.seed(11)


dataCoupled<-coupleImagesAndMeteo(dateStr)

ggplot(dataCoupled, aes(visClass))+geom_bar()+scale_y_log10(breaks=c(100,1000,10000,100000,1000000))


###############
nasVis<-dataCoupled[is.na(visClass)==TRUE,]
uniqueNasVis<-nasVis[!duplicated(nasVis[,.(meteo_station_name,timestamp)]),]
ggplot(uniqueNasVis, aes(meteo_station_name))+geom_bar()+scale_y_log10(breaks=c(100,1000,10000,15000))+ggtitle("NA reading MOR")

##############

total<-dataCoupled

classDataA<-total[visClass=="A"]
classDataB<-total[visClass=="B"]
classDataC<-total[visClass=="C"]
classDataD<-total[visClass=="D"]

##TRAINING

inTrainingA<-sample(nrow(classDataA),100)
training<-classDataA[inTrainingA]
inTrainingB<-sample(nrow(classDataB),0.7*nrow(classDataB))
training<-rbind(training,classDataB[inTrainingB])
inTrainingC<-sample(nrow(classDataC),5000)
training<-rbind(training,classDataC[inTrainingC])
inTrainingD<-sample(nrow(classDataD),5000)
training<-rbind(training,classDataD[inTrainingD])

##TEST

testing<-classDataA[-inTrainingA]
testing<-rbind(testing,classDataB[-inTrainingB])
testing<-rbind(testing,classDataC[sample(nrow(classDataC[-inTrainingC]),7500)])
testing<-rbind(testing,classDataD[sample(nrow(classDataD[-inTrainingD]),12500)])




saveRDS(training,"~/development/fogNNmodels/training4Classes.RDS")

saveRDS(testing,"~/development/fogNNmodels/testing4Classes.RDS")



files<-sapply(training$filepath, function(x) gsub(".*/AXIS214/", "oldArchiveDEBILT/",x))
files<-sapply(files, function(x) gsub(".*/CAMERA/", "",x))
files<-sapply(files, function(x) gsub(".*/cabauw/", "oldArchiveCABAUW/cabauw/",x))


#saveRDS(files,"~/development/fogNNmodels/trainingFileNames.RDS")




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
dtMat[,visClass:=training$visClass]





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

darch1<- darch(trainData, trainTargets, rbm.numEpochs = 0, rbm.batchSize = 50, rbm.trainOutputLayer = F, layers = c(2352,800, 500,100,10), darch.batchSize = 50, darch.learnRate = 2, darch.retainData = F, darch.numEpochs = 800 )
#darch2  <- darch(trainData, trainTargets, rbm.numEpochs = 0, rbm.batchSize = 50, rbm.trainOutputLayer = F, layers = c(2352,1000,800,500,100,10), darch.batchSize = 50, darch.learnRate = 2, darch.retainData = F, darch.numEpochs = 1000 )
#darch3  <- darch(trainData, trainTargets, rbm.numEpochs = 0, rbm.batchSize = 50, rbm.trainOutputLayer = F, layers = c(2352,100,10), darch.batchSize = 50, darch.learnRate = 2, darch.retainData = F, darch.numEpochs = 500 )



saveRDS(darch1,"~/development/fogNNmodels/NNmodelTraine4Classes.RDS")


saveRDS(matRWS,"~/development/fogNNmodels/training4Classes.RDS")


predictedRWS<-predict(darch1,matRWS, type = "class")
  #predict(net,matRWS)
# 
predictedRWS<-data.table(predictedRWS)
# 
# #predictedRWS[,predictedLabels:=colnames(predictedRWS)[max.col(predictedRWS, ties.method = "first")]]
#predictedRWS[,fog:=V2>0]
predictedRWS[,file:=files]

setnames(predictedRWS, "predictedRWS", "predictedClass")

confusion<-data.table(predicted=predictedRWS$predictedClass,fogSensorClass=dtMat$visClass)

table(confusion$predicted,confusion$fogSensorClass)

confusionInSample<-confusionMatrix(confusion$predicted,confusion$fog, mode = "prec_recall", positive = "TRUE")


draw_confusion_matrix(confusionInSample)


#######################TEST-SET######################################
set.seed(11)
#testSet<-total[sample(nrow(total),10000)]

#testSet<-total[-inTrain,]


filesTest<-sapply(testing$filepath, function(x) gsub(".*/AXIS214/", "oldArchiveDEBILT/",x))
filesTest<-sapply(filesTest, function(x) gsub(".*/CAMERA/", "",x))
filesTest<-sapply(filesTest, function(x) gsub(".*/cabauw/", "oldArchiveCABAUW/cabauw/",x))

#saveRDS(filesTest,"~/development/fogNNmodels/filenamesTest.RDS")



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



saveRDS(matRWSTest,"~/development/fogNNmodels/testingDataMat4Classes.RDS")



predictedRWSTest<-predict(darch1,matRWSTest, type = "class")#predict(net,matRWSTest)
# 
predictedRWSTest<-data.table(predictedRWSTest)

setnames(predictedRWSTest, "predictedRWSTest", "predictedClass")

# 
# #predictedRWS[,predictedLabels:=colnames(predictedRWS)[max.col(predictedRWS, ties.method = "first")]]
#predictedRWSTest[,fog:=V2>0]
predictedRWSTest[,file:=filesTest]



confusionTest<-data.table(predicted=predictedRWSTest$predictedClass,fogSensor=testing$visClass)

table(confusionTest$predicted,confusionTest$fogSensor)

confMatTestSet<-confusionMatrix(confusionTest$predicted,confusionTest$fog, mode = "prec_recall", positive = "TRUE")

draw_confusion_matrix(confMatTestSet)


draw_confusion_matrix <- function(cm) {
     layout(matrix(c(1,1,2)))
     par(mar=c(2,2,2,2))
     plot(c(20, 350), c(200, 650), type = "n", xlab="", ylab="", xaxt='n', yaxt='n')
     title('CONFUSION MATRIX', cex.main=2)
   
     # create the matrix
     
     text(100, 575, 'CLASS A', cex=1.2)
     text(175, 575, 'CLASS B', cex=1.2)
     text(250, 575, 'CLASS C', cex=1.2)
     text(325, 575, 'CLASS D', cex=1.2)
     rect(75, 500, 125, 550, col='#024411')
     rect(75, 430, 125, 480, col='#990100')
     rect(75, 360, 125, 410, col='#990100')
     rect(75, 290, 125, 340, col='#990100')
     
     rect(150, 500, 200, 550, col='#990100')
     rect(150, 430, 200, 480, col='#024411')
     rect(150, 360, 200, 410, col='#990100')
     rect(150, 290, 200, 340, col='#990100')
     
     rect(225, 500, 275, 550, col='#990100')
     rect(225, 430, 275, 480, col='#990100')
     rect(225, 360, 275, 410, col='#024411')
     rect(225, 290, 275, 340, col='#990100')
     
     rect(300, 500, 350, 550, col='#990100')
     rect(300, 430, 350, 480, col='#990100')
     rect(300, 360, 350, 410, col='#990100')
     rect(300, 290, 350, 340, col='#024411')
     
     text(30, 425, 'Predicted', cex=1.3, srt=90, font=2)
     text(212, 625, 'Actual', cex=1.3, font=2)
     #rect(150, 305, 240, 365, col='#F7AD50')
     #rect(250, 305, 340, 365, col='#3F97D0')
     text(60, 525, 'CLASS A', cex=1.2)#, srt=90)
     text(60, 455, 'CLASS B', cex=1.2)
     text(60, 385, 'CLASS C', cex=1.2)
     text(60, 315, 'CLASS D', cex=1.2)
   
     # add in the cm results
     res <- as.numeric(cm$table)
     text(100, 525, res[1], cex=1.6, font=2, col='white')
     text(100, 455, res[2], cex=1.6, font=2, col='white')
     text(100, 385, res[3], cex=1.6, font=2, col='white')
     text(100, 315, res[4], cex=1.6, font=2, col='white')
     
     text(175, 525, res[5], cex=1.6, font=2, col='white')
     text(175, 455, res[6], cex=1.6, font=2, col='white')
     text(175, 385, res[7], cex=1.6, font=2, col='white')
     text(175, 315, res[8], cex=1.6, font=2, col='white')
     
     text(250, 525, res[9], cex=1.6, font=2, col='white')
     text(250, 455, res[10], cex=1.6, font=2, col='white')
     text(250, 385, res[11], cex=1.6, font=2, col='white')
     text(250, 315, res[12], cex=1.6, font=2, col='white')
     
     text(325, 525, res[12], cex=1.6, font=2, col='white')
     text(325, 455, res[14], cex=1.6, font=2, col='white')
     text(325, 385, res[15], cex=1.6, font=2, col='white')
     text(325, 315, res[16], cex=1.6, font=2, col='white')
   
     # add in the specifics
     plot(c(160, -40), c(200, 0), type = "n", xlab="", ylab="", main = "DETAILS", xaxt='n', yaxt='n')
     text(30, 180, dimnames(cm$byClass)[[1]][1], cex=1.2, font=2)
     text(70, 180, dimnames(cm$byClass)[[1]][2], cex=1.2, font=2)
     text(110, 180, dimnames(cm$byClass)[[1]][3], cex=1.2, font=2)
     text(150, 180, dimnames(cm$byClass)[[1]][4], cex=1.2, font=2)
     
     text(5, 150, dimnames(cm$byClass)[[2]][5], cex=1.2, font=2)
     text(5, 100, dimnames(cm$byClass)[[2]][6], cex=1.2, font=2)
     text(5, 50, dimnames(cm$byClass)[[2]][7], cex=1.2, font=2)
     
     #Precision
     text(30, 150, round(as.numeric(cm$byClass[17]), 3), cex=1.2)
     text(70, 150, round(as.numeric(cm$byClass[18]), 3), cex=1.2)
     text(110, 150, round(as.numeric(cm$byClass[19]), 3), cex=1.2)
     text(150, 150, round(as.numeric(cm$byClass[20]), 3), cex=1.2)
     
     #Recall
     text(30, 100, round(as.numeric(cm$byClass[21]), 3), cex=1.2)
     text(70, 100, round(as.numeric(cm$byClass[22]), 3), cex=1.2)
     text(110, 100, round(as.numeric(cm$byClass[23]), 3), cex=1.2)
     text(150, 100, round(as.numeric(cm$byClass[24]), 3), cex=1.2)     
    
     #F1
     text(30, 50, round(as.numeric(cm$byClass[25]), 3), cex=1.2)
     text(70, 50, round(as.numeric(cm$byClass[26]), 3), cex=1.2)
     text(110, 50, round(as.numeric(cm$byClass[27]), 3), cex=1.2)
     text(150, 50, round(as.numeric(cm$byClass[28]), 3), cex=1.2)  
   
     # add in the accuracy information
     text(-30, 150, paste("Overall\n",names(cm$overall[1])), cex=1.5, font=2)
     text(-30, 75, round(as.numeric(cm$overall[1]), 3), cex=1.4)
     #text(70, 35, names(cm$overall[2]), cex=1.5, font=2)
     #text(70, 20, round(as.numeric(cm$overall[2]), 3), cex=1.4)
   }

























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






















