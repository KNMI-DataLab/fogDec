library(h2o)
library(doParallel)
library(imager)
library(data.table)
library(fogDec)
library(dplyr)


Sys.setenv(TZ = "UTC")

set.seed(11)

trainValTestSetList7500<-createTrainValidTestSetsBinary("~/share/", dateMax= "\'2018-05-14 00:00:00\'", dbConfigDir = "~/development/fogDec/",maxDist=7500)
trainValTestSetList2500<-createTrainValidTestSetsBinary("~/share/", dateMax= "\'2018-05-14 00:00:00\'", dbConfigDir = "~/development/fogDec/",maxDist=2500)

samplesTrain7500<-trainValTestSetList7500[[1]]
samplesTrain2500<-trainValTestSetList2500[[1]]

samplesTest2500<-trainValTestSetList2500[[3]]
samplesTest7500<-trainValTestSetList7500[[3]]



testFull<-rbind(samplesTest2500,samplesTest7500)
trainFull<-rbind(samplesTrain2500,samplesTrain7500)
testFull<-data.table(testFull)
trainFull<-data.table(trainFull)
setkey(testFull,image_id)
setkey(trainFull,image_id)
diffDT<-setdiff(testFull, trainFull)

diffDT<-data.table(diffDT)

testSetDiff<-diffDT

files<-sapply(testSetDiff$filepath, function(x) gsub(".*/AXIS214/", "oldArchiveDEBILT/",x))
files<-sapply(files, function(x) gsub(".*/CAMERA/", "",x))
files<-sapply(files, function(x) gsub(".*/cabauw/", "oldArchiveCABAUW/cabauw/",x))



setwd("~/share/")

#files<-test


resolutionImg<-28

cl <- makeCluster(45)
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
dtMat[,foggy:=testSetDiff$foggy]
dtMat[,filepath:=testSetDiff$filepath]
complete<-dtMat[complete.cases(dtMat)]
#lastFeature<-resolutionImg*resolutionImg*3
#trainData<-complete[,1:lastFeature]
#groundTruth<-lastFeature+1
#trainTargets<-complete[,groundTruth:groundTruth]

#shuffle rows just to avoid learning on inserted data (initial fog after no fog)
#completeTraining<-complete[sample(nrow(complete),size = 200000),]
completeTest<-complete


write.csv(completeTest,"~/nndataH2O/TECO/testSetPixelValues.csv")


h2o.init(nthreads=-1, max_mem_size="120G")
h2o.removeAll() ## clean slate - just in case the cluster was already running


h2oTestFrame<-h2o.importFile("/home/pagani/nndataH2O/TECO/testSetPixelValues.csv")


model2500<-h2o.loadModel("/home/pagani/nndataH2O/TECO/dl_grid_model_35_2500m")
model7500<-h2o.loadModel("/home/pagani/nndataH2O/TECO/dl_grid_model_2_7500m")
model2500m<-h2o.loadModel("/home/pagani/nndataH2O/TECO/dl_grid_model_7_2500m")



predictionsModel2500 <- h2o.predict(model2500, h2oTestFrame)
predictionsModel2500m <- h2o.predict(model2500m, h2oTestFrame)

predictionsModel7500 <- h2o.predict(model7500, h2oTestFrame)


prediction2500DT<-as.data.table(predictionsModel2500)
prediction2500DTm<-as.data.table(predictionsModel2500m)

prediction7500DT<-as.data.table(predictionsModel7500)

resultsTest2500<-as.data.table(h2oTestFrame$filepath)
resultsTest2500$foggy<-as.data.table(h2oTestFrame$foggy)
resultsTest2500$prediction<-prediction2500DT$predict

resultsTest2500m<-as.data.table(h2oTestFrame$filepath)
resultsTest2500m$foggy<-as.data.table(h2oTestFrame$foggy)
resultsTest2500m$prediction<-prediction2500DTm$predict

resultsTest7500<-as.data.table(h2oTestFrame$filepath)
resultsTest7500$foggy<-as.data.table(h2oTestFrame$foggy)
resultsTest7500$prediction<-prediction7500DT$predict

library(caret)

#cm2500<-confusionMatrix(resultsTest2500$prediction,reference = resultsTest2500$foggy,positive = "TRUE",mode = "prec_recall")
cm7500<-confusionMatrix(resultsTest7500$prediction,reference = resultsTest7500$foggy,positive = "TRUE",mode = "prec_recall")
cm2500m<-confusionMatrix(resultsTest2500m$prediction,reference = resultsTest2500m$foggy,positive = "TRUE",mode = "prec_recall")


draw_confusion_matrix_binaryCaretMatrix <- function(cm) {
  
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
  TN<-cm$table[1,1]
  FN<-cm$table[1,2]
  FP<-cm$table[2,1]
  TP<-cm$table[2,2]
  precision<-TP/(TP+FP)
  recall<-TP/(TP+FN)
  
  text(195, 400, TN, cex=1.6, font=2, col='white')
  text(195, 335, FP, cex=1.6, font=2, col='white')
  text(295, 400, FN, cex=1.6, font=2, col='white')
  text(295, 335, TP, cex=1.6, font=2, col='white')
  
  # add in the specifics
  plot(c(100, 0), c(100, 0), type = "n", xlab="", ylab="", main = "DETAILS", xaxt='n', yaxt='n')
  #text(10, 85, names(cm$byClass[1]), cex=1.2, font=2)
  #text(10, 70, round(as.numeric(cm$byClass[1]), 3), cex=1.2)
  #text(30, 85, names(cm$byClass[2]), cex=1.2, font=2)
  #text(30, 70, round(as.numeric(cm$byClass[2]), 3), cex=1.2)
  text(10, 85, "Precision", cex=1.4, font=2)
  text(10, 65, round(precision, 3), cex=1.4)
  text(30, 85, "Recall", cex=1.4, font=2)
  text(30, 65, round(recall, 3), cex=1.4)
  text(50, 85, "F1 Score", cex=1.4, font=2)
  text(50, 65, round(2*precision*recall/(precision+recall), 3), cex=1.4)
  
  # add in the accuracy information
  text(80, 85, "Accuracy", cex=1.4, font=2)
  text(80, 65, round((TP+TN)/(TP+TN+FP+FN), 3), cex=1.4)
  #text(70, 35, names(cm$overall[2]), cex=1.5, font=2)
  #text(70, 20, round(as.numeric(cm$overall[2]), 3), cex=1.4)
}

draw_confusion_matrix_binaryCaretMatrix(cm2500)
draw_confusion_matrix_binaryCaretMatrix(cm2500m)

draw_confusion_matrix_binaryCaretMatrix(cm7500)




