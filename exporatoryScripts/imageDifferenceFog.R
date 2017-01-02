library(imager)
library(doParallel)
library(data.table)
registerDoParallel(cores=4)


difference<-function(image){
ideal<-load.image("~/development/KNMI/fogDec/fog.jpg")
scotland<-subim(load.image(image), y>39)
idealResize<-resize(ideal,width(scotland),height(scotland))
diffnorm<-idealResize-scotland
meanval<-mean(diffnorm)
meanval
medianval<-median(diffnorm)
list(meanval,medianval)
}

path<-"~/temp/7-715/"

filenames <- list.files(path, recursive = T,
                        pattern=glob2rx("*.jpg"),
                        #pattern=glob2rx("EHTW_201512*.jpg"),
                        full.names=TRUE)


imageSummary <- foreach(file = iter(filenames), .combine = rbind) %dopar% {
  value<-difference(file)#Twente pattern
  file
  data.table(filename=file,meanDiff=value[[1]],medianDiff=value[[2]])
  #print("ABC")
}
