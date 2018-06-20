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


fileToAnalyze<-"/nas-research.knmi.nl/sensordata/CAMERA/RWS/A2/HM776/ID10912/201806/A2-HM776-ID10912_20180606_0805.jpg"

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


camerasDF<-jsonlite::fromJSON("/usr/people/pagani/development/fogVisibility/fogDec/inst/extScripts/python/MVPCameras.json")

camerasRWS<-camerasDF$cameras$RWS


pos<-gregexpr("-",locationAndID)
lastDash<-pos[[1]][length(pos[[1]])]
location<-substring(locationAndID,1,lastDash-1)
cameraID<-substring(locationAndID,lastDash+1,str_length(locationAndID))


#to continue from here
camerasRWS[camerasRWS$location=="A2-HM742",]
cameraInfo<-camerasRWS["location"==location & "cameraID"==cameraID,]


print(fileLocation)


df <- data.frame(matrix(ncol = 7, nrow = 0))
x <- c("TimeStamp", "FileLocation", "OriginalPath", "CameraLocation", "Latitude", "Longitude", "VisibilityClass")
colnames(df) <- x






exportJson <- toJSON(list1, pretty = TRUE)
write(exportJson, "/usr/people/pagani/development/fogVisibility/fogDec/results/test.json")
