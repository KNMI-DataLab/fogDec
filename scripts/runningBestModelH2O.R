library(h2o)
library(doParallel)
library(imager)
library(data.table)
library(fogDec)




h2o.init(nthreads=-1, max_mem_size="100G")
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