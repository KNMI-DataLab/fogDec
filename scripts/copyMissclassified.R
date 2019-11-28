library(data.table)
library(imager)

table<-fread("~/nndataH2O/TECO/misclassTestSet/falsePositivetestSet7500.csv")


files<-sapply(table$filepath, function(x) gsub(".*/AXIS214/", "oldArchiveDEBILT/",x))
files<-sapply(files, function(x) gsub(".*/CAMERA/", "",x))
files<-sapply(files, function(x) gsub(".*/cabauw/", "oldArchiveCABAUW/cabauw/",x))



setwd("~/share/")
file.copy(files, to="/home/pagani/nndataH2O/TECO/misclassTestSet/falsePositivetestSet7500/")


#image<-load.image("/home/pagani/nndataH2O/toLookInto/picturesFP/A15-HM190-ID12371_20180221_1400.jpg")
#resolutionImg<-28
#  image<-resize(image,resolutionImg,resolutionImg)
#  plot(image)
#  image<-blur_anisotropic(image, amplitude = 10000)
#  plot(image)
  