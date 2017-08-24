library(foreach)
library(doParallel)
library(imager)
library(data.table)


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


net<-nnet(f,dtMat,size=10, MaxNWts=30000)


#nn <- neuralnet::neuralnet(f,dtMat[,c(1:30,2353)],hidden=c(10,10,10),linear.output=FALSE)






















