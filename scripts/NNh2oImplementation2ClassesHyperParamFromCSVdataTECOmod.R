library(h2o)
library(doParallel)
library(imager)
library(data.table)
library(fogDec)
library(caret)




draw_confusion_matrix_binaryH20 <- function(cm) {
  
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
  TN<-cm[1,1]
  FN<-cm[2,1]
  FP<-cm[1,2]
  TP<-cm[2,2]
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


resultsOnThreshold<-function(predictions,h2oFrame,threshold){
  
  predictionDT<-as.data.table(predictions)
  
  
  results<-as.data.table(h2oFrame$filepath)
  results$groundTruth<-as.data.table(h2oFrame$foggy)
  results$prediction<-as.factor(predictionDT$TRUE.>=threshold)
  
  results
}

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



h2o.init(nthreads=-1, max_mem_size="250G")
h2o.removeAll() ## clean slate - just in cas
h2oTrainingFrame<-h2o.importFile("/data_enc/trainingh2o.csv")

loadedModel<-h2o.loadModel("/workspace/andrea/exports/models/2500_m_train/24_07/dl_grid_model_2")


# 
# cmTrain<-h2o.confusionMatrix(loadedModel,h2oTrainingFrame)
# perfTrain<-h2o.performance(loadedModel,h2oTrainingFrame)

#draw_confusion_matrix_binaryH20(cmTrain)


#h2o.saveModel(loadedModel,"/workspace/andrea/exports/models/500_m_train/latest",force = T)


h2oValidating<-h2o.importFile("/data_enc/validatingh2oWithFilename.csv")



perfTraining<-h2o.performance(loadedModel,h2oTrainingFrame)
cmTrain<-h2o.confusionMatrix(loadedModel,h2oTrainingFrame)

draw_confusion_matrix_binaryH20(cmTrain)


predOnValid<-h2o.predict(loadedModel, h2oValidating)

threshold<-perfTraining@metrics$max_criteria_and_metric_scores$threshold[[1]]




results<-resultsOnThreshold(predOnValid,h2oValidating,threshold)


cmValid<-confusionMatrix(results$prediction,reference = results$groundTruth,positive = "TRUE",mode = "prec_recall")



draw_confusion_matrix_binaryCaretMatrix(cmValid)



h2oTestTECO<-h2o.importFile("/data_enc/testSetPixelValues.csv")

predOnTest<-h2o.predict(loadedModel, h2oTestTECO)

threshold<-perfTraining@metrics$max_criteria_and_metric_scores$threshold[[1]]

resultsTest<-resultsOnThreshold(predOnTest,h2oTestTECO,threshold)

cmTest<-confusionMatrix(resultsTest$prediction,reference = resultsTest$groundTruth,positive = "TRUE",mode = "prec_recall")


draw_confusion_matrix_binaryCaretMatrix(cmTest)
















# predictionsTrain <- h2o.predict(best_model, h2oTrainingFrame)
# 
# predTrainDT<-as.data.table(predictionsTrain)
# 
# trainDT<-fread("/data_enc/trainingh2o.csv")
# 
# totalTrainDT<-trainDT[,c("foggy","filepath")]
# totalTrainDT<-cbind(totalTrainDT,predTrainDT)
# 
# 
# ##CROSS VALID SET - doing it on a propert fog to non-fog ratio##
# 
# validating<-validSet
# 
# 
# files<-sapply(validating$filepath, function(x) gsub(".*/AXIS214/", "oldArchiveDEBILT/",x))
# files<-sapply(files, function(x) gsub(".*/CAMERA/", "",x))
# files<-sapply(files, function(x) gsub(".*/cabauw/", "oldArchiveCABAUW/cabauw/",x))
# 
# 
# 
# setwd("~/share/")
# 
# #files<-test
# 
# 
# resolutionImg<-28
# 
# cl <- makeCluster(40)
# registerDoParallel(cl)
# 
# clusterEvalQ(cl, library("imager"))
# 
# matRWSvalid<-foreach(i=1:length(files)) %dopar%{
#   message(files[[i]])
#   image<-tryCatch(
#     load.image(files[[i]]),
#     
#     error=function(error_message) {
#       #message("Yet another error message.")
#       #message("Here is the actual R error message:")
#       #next
#       return(NA)
#     }
#   )
#   if(is.na(image[[1]])){
#     v<-NA*1:(resolutionImg*resolutionImg)
#     message("Image not available error in acquisition")
#     v
#   }else{
#     image<-resize(image,resolutionImg,resolutionImg)
#     image<-blur_anisotropic(image, amplitude = 10000)
#     df<-as.data.frame(image)
#     v<-df$value
#     #mat<-rbind(mat,v)
#     v
#   }
# }
# 
# stopCluster(cl)
# 
# 
# matRWSvalid<-do.call(rbind,matRWSvalid)
# 
# 
# dtMatValid<-data.table(matRWSvalid)
# dtMatValid[,foggy:=validating$foggy]
# dtMatValid[,filepath:=validating$filepath]
# completeValid<-dtMatValid[complete.cases(dtMatValid)]
# lastFeature<-resolutionImg*resolutionImg*3
# trainData<-completeValid[,1:lastFeature]
# groundTruth<-lastFeature+1
# testTargets<-completeValid[,groundTruth:groundTruth]
# 
# saveRDS(completeValid,"~/nndataH2O/crossValH2O_64px.RDS")
# 
# 
# 
# h2o.init(nthreads=-1, max_mem_size="120G")
# h2o.removeAll() ## clean slate - just in case the cluster was already running
# 
# 
# h2oValidating<-as.h2o(completeValid)
# 
# 
# h2o.exportFile(h2oValidating,"/home/pagani/nndataH2O/h2oFrames/validatingh2o_64px.csv", force = T)
# 
# 
# 
# 
# h2o.performance(testh2oDL,h2oValidating)

#################



















