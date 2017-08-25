library(foreach)
library(doParallel)
library(imager)
library(nnet)
library(data.table)
library(DBI)
library(jsonlite)
library(caret)



cl <- makeCluster(16)
registerDoParallel(cl)

clusterEvalQ(cl, library("imager"))


#res<-readRDS("~/images/Daniel/Training/ImageDescription.rds")
res2<-readRDS("~/images/Daniel/Training/ImageDescription2.rds")


filesBase<-res2$basename
setwd("~/images/Daniel/Training/")


#files<-list.files("~/images/Daniel/Training/", pattern = ".jpg", full.names = T)
#mat<-NULL

mat<-foreach(i=1:length(filesBase), .combine = rbind) %dopar%{
  image<-load.image(filesBase[[i]])
  image<-resize(image,28,28)
  image<-blur_anisotropic(image, amplitude = 15000)
  df<-as.data.frame(image)
  v<-df$value
  #mat<-rbind(mat,v)
  v
}
dtMat<-data.table(mat)
dtMat[,vis_class:=res2$vis_class]

stopCluster(cl)



feats <- names(dtMat)
feats <- feats[-length(feats)]
#feats<-feats[1:30]

# Concatenate strings
f <- paste(feats,collapse=' + ')
f <- paste('vis_class ~',f)

f <- as.formula(f)


net<-nnet(f,dtMat,size=5, MaxNWts=35000, maxit=800)


#nn <- neuralnet::neuralnet(f,dtMat[,c(1:30,2353)],hidden=c(10,10,10),linear.output=FALSE)



resTest2<-readRDS("~/images/Daniel/Testing/ImageDescription2.rds")


filesBase<-resTest2$basename
setwd("~/images/Daniel/Testing/")

cl <- makeCluster(16)
registerDoParallel(cl)

clusterEvalQ(cl, library("imager"))


matTest<-foreach(i=1:length(filesBase), .combine = rbind) %dopar%{
  image<-load.image(filesBase[[i]])
  image<-resize(image,28,28)
  image<-blur_anisotropic(image, amplitude = 15000)
  df<-as.data.frame(image)
  v<-df$value
  #mat<-rbind(mat,v)
  v
}

stopCluster(cl)
dtMatTest<-data.table(matTest)

dtMatTest[,vis_class:=resTest2$vis_class]



prdicted<-predict(net,dtMatTest)

dtMatTest[,predictedLabels:=colnames(prdicted)[max.col(prdicted, ties.method = "first")]]
confusion<-data.table(predicted=dtMatTest$predictedLabels,vis_class=resTest2$vis_class)

table(confusion$predicted,confusion$vis_class)

confusionMatrix(confusion$predicted,confusion$vis_class, mode = "prec_recall")






#all files (not good have to get the daylight images ;-)
setwd("~/images/RWS")

files<-list.files(".","*.jpg", full.names = T,recursive = T)




setwd("~/development/fogDec/")

dbConfig <- fromJSON("config.json")

con <- dbConnect(RPostgreSQL::PostgreSQL(),
                 dbname = "FOGDB",
                 host = dbConfig[["host"]], port = 9418,
                 user = dbConfig[["user"]], password = dbConfig[["pw"]])



imagesRWSDayLight <- dbGetQuery(con, "SELECT images.image_id, images.filepath, images.timestamp, images.day_phase
                                FROM images
                                WHERE camera_id =326 AND day_phase=1 AND timestamp<='2017-08-24 09:01:00';")



test<-sapply(imagesRWSDayLight$filepath, strsplit, "HM577/")
test<-sapply(test, "[[", 2)


setwd("~/images/RWS/")

files<-test


cl <- makeCluster(16)
registerDoParallel(cl)

clusterEvalQ(cl, library("imager"))

matRWS<-foreach(i=1:length(files), .combine = rbind) %dopar%{
  message(files[[i]])
  image<-load.image(files[[i]])
  image<-resize(image,28,28)
  image<-blur_anisotropic(image, amplitude = 15000)
  df<-as.data.frame(image)
  v<-df$value
  #mat<-rbind(mat,v)
  v
}

stopCluster(cl)


predictedRWS<-predict(net,matRWS)

predictedRWS<-data.table(predictedRWS)

predictedRWS[,predictedLabels:=colnames(predictedRWS)[max.col(predictedRWS, ties.method = "first")]]
predictedRWS[,file:=files]






