library(h2o)
library(doParallel)
library(imager)
library(data.table)
library(fogDec)




# photoDir<-"~/share"
# setwd(photoDir) ##For RStudio
# 
# set.seed(11)
# 
# trainValTestSetList<-createTrainValidTestSetsBinary("~/share/", dateMax= "\'2018-05-14 00:00:00\'", dbConfigDir = "~/development/fogDec/",maxDist=2500)
# 
# trainSet<-trainValTestSetList[[1]]
# trainSet<-trainSet[sample(nrow(trainSet)),]
# 
# validSet<-trainValTestSetList[[2]]
# validSet<-validSet[sample(nrow(validSet)),]
# 
# 
# testSet<-trainValTestSetList[[3]]
# testSet<-testSet[sample(nrow(testSet)),]
# 
# 
# #training<-trainSet[sample(nrow(trainSet),20000),]
# training<-trainSet
# 
# 
# files<-sapply(training$filepath, function(x) gsub(".*/AXIS214/", "oldArchiveDEBILT/",x))
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
# cl <- makeCluster(35)
# registerDoParallel(cl)
# 
# clusterEvalQ(cl, library("imager"))
# 
# matRWS<-foreach(i=1:length(files)) %dopar%{
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
# matRWS<-do.call(rbind,matRWS)
# 
# 
# dtMat<-data.table(matRWS)
# dtMat[,foggy:=training$foggy]
# dtMat[,filepath:=training$filepath]
# complete<-dtMat[complete.cases(dtMat)]
# lastFeature<-resolutionImg*resolutionImg*3
# #trainData<-complete[,1:lastFeature]
# #groundTruth<-lastFeature+1
# #trainTargets<-complete[,groundTruth:groundTruth]
# 
# #shuffle rows just to avoid learning on inserted data (initial fog after no fog)
# #completeTraining<-complete[sample(nrow(complete),size = 200000),]
# completeTraining<-complete
# saveRDS(completeTraining,"~/nndataH2O/trainingH2O_realRatio_50px.RDS")
# 
# h2o.init(nthreads=-1, max_mem_size="100G")
# h2o.removeAll() ## clean slate - just in case the cluster was already running
# 
# 
# h2oTrainingFrame<-as.h2o(completeTraining)





#interesting to know and not clear from the documentation: https://groups.google.com/forum/#!topic/h2ostream/Szov_rHgduU
# https://groups.google.com/forum/#!topic/h2ostream/yKPBdb38hdU


h2o.init(nthreads=-1, max_mem_size="250G")
h2o.removeAll() ## clean slate - just in cas
h2oTrainingFrame<-h2o.importFile("/data_enc/trainingH2O_TECO_7500_28px.csv")
#h2oValidating<-h2o.importFile("/data_enc/validatingh2o.csv")
#best_model<-h2o.loadModel("/workspace/andrea/exports/models/dl_grid_model_35")


hyper_params <- list(
  activation = c("Rectifier", "Maxout", "Tanh", "RectifierWithDropout", "MaxoutWithDropout", "TanhWithDropout"),
  hidden = list( c(10,10,10,10,10),c(50,50,50,25,10),c(75,75,50,50,10), c(50,50,50,25,25,10)),
  epochs = c(500,650,750,1000,1500),
  l1 = c(0, 0.00001, 0.0001),
  l2 = c(0, 0.00001, 0.0001),
  rate = c(0, 0.05, 0.01),
  rate_annealing = c(1e-8, 1e-7, 1e-6),
  rho = c(0.9, 0.95, 0.99, 0.999),
  epsilon = c(1e-10, 1e-8, 1e-6, 1e-4),
  momentum_start = c(0, 0.5),
  momentum_stable = c(0.99, 0.5, 0),
  input_dropout_ratio = c(0, 0.1, 0.2),
  max_w2 = c(10, 100, 1000, 3.4028235e+38),
  balance_classes = TRUE
)

# search_criteria <- list(strategy = "RandomDiscrete",
#                         max_models = 1000,
#                         max_runtime_secs = 900,
#                         stopping_tolerance = 0.0001,
#                         stopping_rounds = 2500,
#                         seed = 42
# )

search_criteria <- list(strategy = "RandomDiscrete",
                        max_models = 500,
                        max_runtime_secs = 900,
                        stopping_tolerance = 0.01,
                        stopping_rounds = 250,
                        seed = 42
)



dl_grid <- h2o.grid(algorithm = "deeplearning",
                    x = 1:2352,
                    y = "foggy",
                    #weights_column = "x",
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
grid <- h2o.getGrid("dl_grid", sort_by = "f1", decreasing = TRUE)
grid
modelID<-grid@model_ids
best_model <- h2o.getModel(modelID[[1]])
h2o.confusionMatrix(best_model)
h2o.performance(best_model,h2oTrainingFrame)

h2o.saveModel(best_model,"/workspace/andrea/exports/models/7500_m_train/latest_new",force = T)


h2oValidating<-h2o.importFile("/data_enc/validatingh2oWithFilename.csv")



h2o.performance(best_model,h2oValidating)



predictionsTrain <- h2o.predict(best_model, h2oTrainingFrame)

predTrainDT<-as.data.table(predictionsTrain)

trainDT<-fread("/data_enc/trainingh2o.csv")

totalTrainDT<-trainDT[,c("foggy","filepath")]
totalTrainDT<-cbind(totalTrainDT,predTrainDT)


##CROSS VALID SET - doing it on a propert fog to non-fog ratio##

validating<-validSet


files<-sapply(validating$filepath, function(x) gsub(".*/AXIS214/", "oldArchiveDEBILT/",x))
files<-sapply(files, function(x) gsub(".*/CAMERA/", "",x))
files<-sapply(files, function(x) gsub(".*/cabauw/", "oldArchiveCABAUW/cabauw/",x))



setwd("~/share/")

#files<-test


resolutionImg<-28

cl <- makeCluster(40)
registerDoParallel(cl)

clusterEvalQ(cl, library("imager"))

matRWSvalid<-foreach(i=1:length(files)) %dopar%{
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


matRWSvalid<-do.call(rbind,matRWSvalid)


dtMatValid<-data.table(matRWSvalid)
dtMatValid[,foggy:=validating$foggy]
dtMatValid[,filepath:=validating$filepath]
completeValid<-dtMatValid[complete.cases(dtMatValid)]
lastFeature<-resolutionImg*resolutionImg*3
trainData<-completeValid[,1:lastFeature]
groundTruth<-lastFeature+1
testTargets<-completeValid[,groundTruth:groundTruth]

saveRDS(completeValid,"~/nndataH2O/crossValH2O_64px.RDS")



h2o.init(nthreads=-1, max_mem_size="120G")
h2o.removeAll() ## clean slate - just in case the cluster was already running


h2oValidating<-as.h2o(completeValid)


h2o.exportFile(h2oValidating,"/home/pagani/nndataH2O/h2oFrames/validatingh2o_64px.csv", force = T)




h2o.performance(testh2oDL,h2oValidating)

#################















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





