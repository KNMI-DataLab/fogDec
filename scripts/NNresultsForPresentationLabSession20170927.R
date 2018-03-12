#results for presentation
library(darch)

modelDarch<-readRDS("~/development/fogNNmodels/model1.rds")
dataMatTrain<-readRDS("~/development/fogNNmodels/trainingDataMat.RDS")
dataMatTest<-readRDS("~/development/fogNNmodels/testingDataMat.RDS")

testing<-readRDS("~/development/fogNNmodels/testingDataLabels.RDS")
training<-readRDS("~/development/fogNNmodels/trainingDataLabels.RDS")

filesTest<-readRDS("~/development/fogNNmodels/filenamesTest.RDS")


dtMat<-data.table(dataMatTrain)
#dtMat[,vis_class:=res2$vis_class]
dtMat[,foggy:=training$foggy]


###In sample testing#####
predictedRWS<-predict(modelDarch,dataMatTrain, type = "bin")
#predict(net,matRWS)
# 
predictedRWS<-data.table(predictedRWS)
# 
# #predictedRWS[,predictedLabels:=colnames(predictedRWS)[max.col(predictedRWS, ties.method = "first")]]
predictedRWS[,fog:=V2>0]
predictedRWS[,file:=files]



confusion<-data.table(predicted=predictedRWS$fog,fogSensor=dtMat$foggy)

table(confusion$predicted,confusion$fogSensor)

confusionMatrix(confusion$predicted,confusion$fog, mode = "prec_recall", positive = "TRUE")




####OUT of sample testing##
predictedRWSTest<-predict(modelDarch,dataMatTest, type = "bin")#predict(net,matRWSTest)
# 
predictedRWSTest<-data.table(predictedRWSTest)
# 
# #predictedRWS[,predictedLabels:=colnames(predictedRWS)[max.col(predictedRWS, ties.method = "first")]]
predictedRWSTest[,fog:=V2>0]
predictedRWSTest[,file:=filesTest]



confusionTest<-data.table(predicted=predictedRWSTest$fog,fogSensor=testing$foggy)

table(confusionTest$predicted,confusionTest$fog)

confusionMatrix(confusionTest$predicted,confusionTest$fog, mode = "prec_recall", positive = "TRUE")
