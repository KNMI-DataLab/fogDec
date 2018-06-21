#!/usr/bin/env Rscript
args = commandArgs(trailingOnly=TRUE)


# test if there is at least one argument: if not, return an error
if (length(args)==0) {
  stop("At least one argument must be supplied (input file).jpg", call.=FALSE)
} else if (length(args)==1) {
  fileToAnalyze = args[1]
}


library(RJSONIO)
list1 <- vector(mode="list", length=2)
list1[[1]] <- c("a", "b", "c")
list1[[2]] <- c(1, 2, 3)
list1[[3]] <- fileToAnalyze

#      (?<=\/)(.*)(?=\.jpg)
#       [^\/]*\.(jpg|jpeg)


fileToAnalyze<-"/nas-research.knmi.nl/sensordata/CAMERA/RWS/A2/HM776/ID10915/201806/A2-HM776-ID10915_20180606_0801.jpg"

library(stringr)
fileLocation<-gsub(".*/nas-research.knmi.nl/sensordata/CAMERA/", "~/share/", fileToAnalyze)
partial<-str_extract(pattern = "[^/]*$", string =  fileToAnalyze)
partial<-str_extract(string = partial, pattern = ".*(?=\\.)")
timeStampTemp<-unlist(strsplit(partial, split = "_"))
date<-timeStampTemp[length(timeStampTemp)-1]
time<-timeStampTemp[length(timeStampTemp)]

Sys.setenv(TZ = "UTC")

timeStamp<-as.POSIXct(paste(date,time),format="%Y%m%d %H%M")
originalPath<-fileToAnalyze

locationAndID<-timeStampTemp[1]


home<-"/usr/people/pagani/development/fogVisibility/"

jsonCameras<-paste0(home,"fogDec/inst/extScripts/python/MVPCameras.json")

camerasDF<-jsonlite::fromJSON(jsonCameras)

camerasRWS<-camerasDF$cameras$RWS


pos<-gregexpr("-",locationAndID)
lastDash<-pos[[1]][length(pos[[1]])]
location<-substring(locationAndID,1,lastDash-1)
cameraID<-substring(locationAndID,lastDash+1,str_length(locationAndID))


cameraTarget<-camerasRWS[camerasRWS$location==location & camerasRWS$cameraID==cameraID,]
cbind(cameraTarget,fileLocation,originalPath, timeStamp)

print(fileLocation)

library(imager)
library(h2o)

featuresImage<-fromImageToFeatures(fileLocation)
featuresImage<-t(featuresImage)
prediction<-predictFogClass(featuresImage)









exportJson <- toJSON(list1, pretty = TRUE)
write(exportJson, "/usr/people/pagani/development/fogVisibility/fogDec/results/test.json")




fromImageToFeatures<-function(filename){
  resolutionImg<-28
  image<-tryCatch(
    load.image(filename),
    
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

predictFogClass<-function(features){
Sys.setenv(JAVA_HOME="/usr/lib/jvm/java-1.8.0-openjdk-1.8.0.171-3.b10.fc26.x86_64/")
  h2o.init(nthreads=1)
 h2o.removeAll()
  best_model<-h2o.loadModel(paste0(home,"fogDec/results/models/dl_grid_model_35"))
  predictions <- h2o.predict(best_model, as.h2o(features))
predDF<-data.frame(predictions)
}


