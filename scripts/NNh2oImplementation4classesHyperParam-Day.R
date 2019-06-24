library(h2o)
library(doParallel)
library(imager)
library(data.table)
library(fogDec)
library(caret)



applyNoise<-function(rowMat,fractionToChange){
  totalPixels<-length(rowMat)
  pixelToChange<-fractionToChange*totalPixels
  randomPositions<-sample(totalPixels,pixelToChange)
  pixelValues<-runif(pixelToChange)
  rowMat[randomPositions]<-pixelValues
  rowMat
}


bindAndFilter<-function(mat,labels){
  dataTab<-data.table(mat)
  tempDT<-cbind(dataTab,labels)
  completeTempDT<-tempDT[complete.cases(tempDT)]
  completeTempDT
}




draw_confusion_matrix_multiclass <- function(cm) {
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
  
  text(325, 525, res[13], cex=1.6, font=2, col='white')
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
  #res <- as.numeric(cm$table)
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










photoDir<-"~/share"
setwd(photoDir) ##For RStudio

set.seed(33)

trainValTestSetList<-createTrainValidTestSetsSplitMulticlassRandom("~/share/", dateMax= "\'2019-01-01 00:00:00\'", dbConfigDir = "~/development/fogDec/",maxDist=7500,visibilityThreshold = c(200,1000,5000),dayPhaseFlag=1)

trainSet<-trainValTestSetList[[1]]
#foggyTrain<-trainSet[foggy==TRUE]
#foggyAugment<-as.data.table(lapply(foggyTrain,rep,8))
#trainSet<-trainSet[sample(nrow(trainSet)),]

validSet<-trainValTestSetList[[2]]
#validSet<-validSet[sample(nrow(validSet)),]


testSet<-trainValTestSetList[[3]]
#testSet<-testSet[sample(nrow(testSet)),]








######DATA AUGMENTATION FOR all classes#####################################################################

training<-trainSet


files<-sapply(training$filepath, function(x) gsub(".*/AXIS214/", "oldArchiveDEBILT/",x))
files<-sapply(files, function(x) gsub(".*/CAMERA/", "",x))
files<-sapply(files, function(x) gsub(".*/cabauw/", "oldArchiveCABAUW/cabauw/",x))



setwd("~/share/")

#files<-test


resolutionImg<-28

cl <- makeCluster(24)
registerDoParallel(cl)

clusterEvalQ(cl, library("imager"))

filesToElaborate<-length(files)

##normal#
matRWSnormal<-foreach(i=1:filesToElaborate) %dopar%{
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
    #image<-blur_anisotropic(image, amplitude = 10000)
    df<-as.data.frame(image)
    v<-df$value
    #mat<-rbind(mat,v)
    v
  }
}



stopCluster(cl)


matRWSnormal<-do.call(rbind,matRWSnormal)


#dtMat<-data.table(matRWS)

#dtMat[,foggy:=training$foggy]
#dtMat[,filepath:=training$filepath]
#dtMat<-cbind(dtMat,training)
#complete<-dtMat[complete.cases(dtMat)]

#lastFeature<-resolutionImg*resolutionImg*3
#trainData<-complete[,1:lastFeature]
#groundTruth<-lastFeature+1
#trainTargets<-complete[,groundTruth:groundTruth]

#shuffle rows just to avoid learning on inserted data (initial fog after no fog)
#completeTraining<-complete[sample(nrow(complete),size = 200000),]
#completeTraining<-complete

#fwrite(completeTraining,"~/nndataH2O/dawnCivil7500m_28px_train03SplitNoBlur.csv")

#rm(matRWS)




####dataAugmentation

#data augmentation:noise added to original
noise05pct<-t(apply(matRWSnormal,1,applyNoise,0.05)) #apply returns a transpose, so transpose to get to the original dimensions
noise10pct<-t(apply(matRWSnormal,1,applyNoise,0.1))
noise20pct<-t(apply(matRWSnormal,1,applyNoise,0.2))


#
#noise10pct,noise20pct
#trainingAugmented<-lapply(X = c(matRWSnormal),FUN = bindAndFilter,training[1:10])


d1<-bindAndFilter(matRWSnormal,training)
d2<-bindAndFilter(noise05pct,training)
d3<-bindAndFilter(noise10pct,training)
d4<-bindAndFilter(noise20pct,training)

augmentedTrain<-rbindlist(list(d1,d2,d3,d4))
#rm(d1,d2,d3,d4)



cl <- makeCluster(24)
registerDoParallel(cl)

clusterEvalQ(cl, library("imager"))

matRWSmirror<-foreach(i=1:filesToElaborate) %dopar%{
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
    image<-mirror(image,"x")
    image<-blur_anisotropic(image, amplitude = 10000)
    df<-as.data.frame(image)
    v<-df$value
    #mat<-rbind(mat,v)
    v
  }
}

stopCluster(cl)


matRWSmirror<-do.call(rbind,matRWSmirror)

#data augmentation:noise added to original
noise05pctmirror<-t(apply(matRWSmirror,1,applyNoise,0.05)) #apply returns a transpose, so transpose to get to the original dimensions
noise10pctmirror<-t(apply(matRWSmirror,1,applyNoise,0.1))
noise20pctmirror<-t(apply(matRWSmirror,1,applyNoise,0.2))


d1<-bindAndFilter(matRWSmirror,training)
d2<-bindAndFilter(noise05pctmirror,training)
d3<-bindAndFilter(noise10pctmirror,training)
d4<-bindAndFilter(noise20pctmirror,training)

augmentedTrainMirror<-rbindlist(list(d1,d2,d3,d4))
#rm(d1,d2,d3,d4)

trainFullAugmented<-rbindlist(list(augmentedTrain,augmentedTrainMirror))
#rm(augmentedTrain,augmentedTrainMirror)


####################TRAINING NON FOGGY ELABORATION##########################################################




trainFullAugmented<-trainFullAugmented[sample(nrow(trainFullAugmented)),]




#############





fwrite(trainFullAugmented,"~/nndataH2O/multiclassDay7500m_28px_train025SplitBlurAugmented.csv")


#dtMat[,foggy:=training$foggy]
#dtMat[,filepath:=training$filepath]
#dtMat<-cbind(dtMat,validSet)
#complete<-dtMat[complete.cases(dtMat)]








#validSetIndex<-sample(nrow(validSet),size = 5000,replace = F)
#validSet<-validSet[validSetIndex]

#################validation###############


files<-sapply(validSet$filepath, function(x) gsub(".*/AXIS214/", "oldArchiveDEBILT/",x))
files<-sapply(files, function(x) gsub(".*/CAMERA/", "",x))
files<-sapply(files, function(x) gsub(".*/cabauw/", "oldArchiveCABAUW/cabauw/",x))



setwd("~/share/")

#files<-test


resolutionImg<-28

cl <- makeCluster(24)
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
    #image<-blur_anisotropic(image, amplitude = 10000)
    df<-as.data.frame(image)
    v<-df$value
    #mat<-rbind(mat,v)
    v
  }
}

stopCluster(cl)


matRWS<-do.call(rbind,matRWS)


#dtMat[,foggy:=training$foggy]
#dtMat[,filepath:=training$filepath]
matRWS<-cbind(matRWS,validSet)
complete<-matRWS[complete.cases(matRWS)]

#lastFeature<-resolutionImg*resolutionImg*3
#trainData<-complete[,1:lastFeature]
#groundTruth<-lastFeature+1
#trainTargets<-complete[,groundTruth:groundTruth]

#shuffle rows just to avoid learning on inserted data (initial fog after no fog)
#completeTraining<-complete[sample(nrow(complete),size = 200000),]
completeValid<-complete

fwrite(completeValid,"~/nndataH2O//multiclassDay7500m_28px_validRealSplitBlurAugmented.csv")



#########################################







#saveRDS(completeTraining,"~/nndataH2O/trainingH2O_realRatio_50px.RDS")

h2o.init(nthreads=20, max_mem_size="100G")
h2o.removeAll() ## clean slate - just in case the cluster was already running


h2oTrainingFrame <- h2o.importFile(path = "/home/pagani/nndataH2O/multiclassDay7500m_28px_train025SplitBlurAugmented.csv", destination_frame = "trainMulti.hex")
h2oValidationFrame <- h2o.importFile(path = "/home/pagani/nndataH2O/multiclassDay7500m_28px_validRealSplitBlurAugmented.csv", destination_frame = "validMulti.hex")


h2oTrainingFrame[,"visClass"]<-as.factor(h2oTrainingFrame[,"visClass"])

trainingFrame<-fread("/home/pagani/nndataH2O/multiclassDay7500m_28px_train025SplitBlurAugmented.csv")
validationFrame<-fread("/home/pagani/nndataH2O/multiclassDay7500m_28px_validRealSplitBlurAugmented.csv")






#interesting to know and not clear from the documentation: https://groups.google.com/forum/#!topic/h2ostream/Szov_rHgduU
# https://groups.google.com/forum/#!topic/h2ostream/yKPBdb38hdU



hyper_params <- list(
  activation = c("Rectifier", "Maxout", "Tanh", "RectifierWithDropout", "MaxoutWithDropout", "TanhWithDropout"),
  hidden = list(c(1000, 1000, 1000, 500, 200, 100)),
  epochs = c(250, 100),
  l1 = c(0, 0.00001, 0.0001),
  l2 = c(0, 0.00001, 0.0001),
  rate = c(0, 0.05, 0.01),
  rate_annealing = c(1e-8, 1e-7, 1e-6),
  rho = c(0.9, 0.95, 0.99, 0.999),
  epsilon = c(1e-10, 1e-8, 1e-6, 1e-4),
  momentum_start = c(0, 0.5),
  momentum_stable = c(0.99, 0.5, 0),
  input_dropout_ratio = c(0, 0.1, 0.2, 0.25),
  max_w2 = c(10, 100, 1000, 3.4028235e+38)#,
  #balance_classes = TRUE
)


search_criteria <- list(strategy = "RandomDiscrete",
                        max_models = 500,
                        max_runtime_secs = 900,
                        stopping_tolerance = 0.01,
                        stopping_rounds = 500,
                        seed = 42
)



dl_grid <- h2o.grid(algorithm = "deeplearning",
                    x = 1:2352,
                    y = "visClass",
                    #weights_column = weights,
                    grid_id = "dl_grid",
                    training_frame = h2oTrainingFrame,
                    #validation_frame = valid,
                    #nfolds = 25,                       
                    #fold_assignment = "Stratified",
                    hyper_params = hyper_params,
                    search_criteria = search_criteria,
                    seed = 42
                    )




grid<-h2o.getGrid("dl_grid")
#sort_options_1 <- c("mean_per_class_error", "mse", "err")
grid <- h2o.getGrid("dl_grid", sort_by = "logloss", decreasing = FALSE)
grid
modelID<-grid@model_ids
best_model <- h2o.getModel(modelID[[1]])
best_model
h2o.confusionMatrix(best_model)
h2o.performance(best_model,h2oTrainingFrame)

h2oFrameTrainPred<-h2o.predict(best_model,h2oTrainingFrame)
h2oFrameTrainPredDT<-as.data.table(h2oFrameTrainPred)
trainDTres<-cbind(h2oFrameTrainPredDT,groundTruth=trainingFrame$visClass,filepath=trainingFrame$filepath)


cfTrain<-confusionMatrix(trainDTres$predict,as.factor(trainDTres$groundTruth))
draw_confusion_matrix_multiclass(cfTrain)


#trainDTres<-data.table(trainDTres)
# trainDTres[,predict:=TRUE.>=0.75]
# fpVal<-dim(trainDTres[predict==TRUE & groundTruth==FALSE])[1]
# tpVal<-dim(trainDTres[predict==TRUE & groundTruth==TRUE])[1]
# tnVal<-dim(trainDTres[predict==FALSE & groundTruth==FALSE])[1]
# fnVal<-dim(trainDTres[predict==FALSE & groundTruth==TRUE])[1]
# confMatPred<-matrix(c(tnVal,fnVal,fpVal,tpVal),nrow = 2,ncol = 2)
# draw_confusion_matrix_binaryH20(confMatPred)


h2oFramevalidPred<-h2o.predict(best_model,h2oValidationFrame)
h2oFramevalidPredDT<-as.data.table(h2oFramevalidPred)
validDTres<-cbind(h2oFramevalidPredDT,groundTruth=validationFrame$visClass,filepath=validationFrame$filepath)
validDTres<-data.table(validDTres)
cfValid<-confusionMatrix(validDTres$predict,as.factor(validDTres$groundTruth))
draw_confusion_matrix_multiclass(cfValid)
#validDTres[,predict:=TRUE.>=0.75]

fpVal<-dim(validDTres[predict==TRUE & groundTruth==FALSE])[1]
tpVal<-dim(validDTres[predict==TRUE & groundTruth==TRUE])[1]
tnVal<-dim(validDTres[predict==FALSE & groundTruth==FALSE])[1]
fnVal<-dim(validDTres[predict==FALSE & groundTruth==TRUE])[1]
confMatVal<-matrix(c(tnVal,fnVal,fpVal,tpVal),nrow = 2,ncol = 2)
draw_confusion_matrix_binaryH20(confMatVal)




precision<-tpVal/(tpVal+fpVal)
recall<-tpVal/(tpVal+fnVal)
f1score<-2*precision*recall/(precision+recall)
print(precision)
print(recall)
print(f1score)


#######################################