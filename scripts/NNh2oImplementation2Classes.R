library(h2o)
library(doParallel)
library(imager)
library(data.table)

h2o.init(nthreads=-1, max_mem_size="2G")
h2o.removeAll() ## clean slate - just in case the cluster was already running


photoDir<-"/usr/people/pagani/share"
setwd(photoDir) ##For RStudio



trainValTestSetList<-createTrainValidTestSetsBinary("~/share/", dbConfigDir = "/usr/people/pagani/development/fogVisibility/fogDec/")

trainSet<-trainValTestSetList[[1]]
validSet<-trainValTestSetList[[2]]
testSet<-trainValTestSetList[[3]]

training<-trainSet[sample(nrow(trainSet),20000),]


files<-sapply(training$filepath, function(x) gsub(".*/AXIS214/", "oldArchiveDEBILT/",x))
files<-sapply(files, function(x) gsub(".*/CAMERA/", "",x))
files<-sapply(files, function(x) gsub(".*/cabauw/", "oldArchiveCABAUW/cabauw/",x))



setwd("~/share/")

#files<-test


resolutionImg<-28

cl <- makeCluster(3)
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
dtMat[,foggy:=training$foggy]
complete<-dtMat[complete.cases(dtMat)]
lastFeature<-resolutionImg*resolutionImg*3
trainData<-complete[,1:lastFeature]
groundTruth<-lastFeature+1
trainTargets<-complete[,groundTruth:groundTruth]

#shuffle rows just to avoid learning on inserted data (initial fog after no fog)
complete2<-complete[sample(nrow(complete)),]

h2oTrainingFrame<-as.h2o(complete)
h2oTrainingFrame2<-as.h2o(complete2)


#interesting to know and not clear from the documentation: https://groups.google.com/forum/#!topic/h2ostream/Szov_rHgduU
# https://groups.google.com/forum/#!topic/h2ostream/yKPBdb38hdU

testh2oDL<-h2o.deeplearning(1:2352,"foggy",training_frame = h2oTrainingFrame, score_training_samples = 0)

testh2oDL2<-h2o.deeplearning(1:2352,"foggy",training_frame = h2oTrainingFrame2, score_training_samples = 0)

confMat<-h2o.confusionMatrix(testh2oDL)

predictions <- h2o.predict(testh2oDL, h2oTrainingFrame)



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
  FN<-cm[1,2]
  FP<-cm[2,1]
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

