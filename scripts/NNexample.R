library(foreach)
library(doParallel)
library(imager)


cl <- makeCluster(6)
registerDoParallel(cl)

clusterEvalQ(cl, library("imager"))


files<-list.files("~/images/Daniel/Training/", pattern = ".jpg", full.names = T)
#mat<-NULL

mat<-foreach(i=1:length(files), .combine = rbind) %dopar%{
  image<-load.image(files[[i]])
  image<-resize(image,28,28)
  df<-as.data.frame(image)
  v<-df$value
  #mat<-rbind(mat,v)
  v
}


