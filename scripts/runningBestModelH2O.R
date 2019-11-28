library(h2o)
library(doParallel)
library(imager)
library(data.table)
library(fogDec)




h2o.init(nthreads=-1, max_mem_size="136G")
h2o.removeAll() ## clean slate - just in cas
h2oTrainingFrame<-h2o.importFile("/data_enc/trainingh2o.csv")
h2oValidating<-h2o.importFile("/data_enc/validatingh2o.csv")
best_model<-h2o.loadModel("/workspace/andrea/exports/models/dl_grid_model_35")

grid<-h2o.getGrid("dl_grid")
#sort_options_1 <- c("mean_per_class_error", "mse", "err")
grid <- h2o.getGrid("dl_grid", sort_by = "f1", decreasing = TRUE)
grid
modelID<-grid@model_ids
best_model <- h2o.getModel(modelID[[1]])
h2o.confusionMatrix(best_model)
h2o.performance(best_model,h2oTrainingFrame)

h2o.performance(best_model,h2oValidating)



predictionsTrain <- h2o.predict(best_model, h2oTrainingFrame)


#getting falsepos and false neg

h2oValidatingFN<-h2o.importFile("/data_enc/validatingh2oWithFilename.csv")
best_model<-h2o.loadModel("/workspace/andrea/exports/models/dl_grid_model_35")
h2o.performance(best_model,h2oValidatingFN)
validPredFileName<-h2o.predict(best_model,h2oValidatingFN)
validPredDT<-as.data.table(validPredFileName)
validationFilenames<-fread("/data_enc/validatingh2oWithFilename.csv")

validFilenamesDT<-validationFilenames[,c("foggy","filepath")]


validPredFilenames_total_DT<-cbind(validFilenamesDT,validPredDT)
FN_valid_dt<-validPredFilenames_total_DT[foggy==TRUE & TRUE.<0.208267]
FP_valid_dt<-validPredFilenames_total_DT[foggy==FALSE & TRUE.>=0.208267]
write.csv(FN_valid_dt,"/workspace/andrea/exports/falseNegValid.csv")
write.csv(FP_valid_dt,"/workspace/andrea/exports/falsePosValid.csv")


