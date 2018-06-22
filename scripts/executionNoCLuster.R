# #!/usr/bin/env Rscript
# args = commandArgs(trailingOnly=TRUE)
# 
# 
# # test if there is at least one argument: if not, return an error
# if (length(args)==0) {
#   stop("At least one argument must be supplied (input file).jpg", call.=FALSE)
# } else if (length(args)==1) {
#   fileToAnalyze = args[1]
# }








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





library(RJSONIO)

fileToAnalyze<-"/nas-research.knmi.nl/sensordata/CAMERA/RWS/A2/HM776/ID10915/201806/A2-HM776-ID10915_20180606_0801.jpg"

library(stringr)
fileLocation<-gsub(".*/nas-research.knmi.nl/sensordata/CAMERA/", "~/share/", fileToAnalyze)
#for amazon###
fileLocation<-"/workspace/andrea/exports/A2-HM776-ID10915_20180606_0801.jpg"
#####

partial<-str_extract(pattern = "[^/]*$", string =  fileToAnalyze)
partial<-str_extract(string = partial, pattern = ".*(?=\\.)")
timeStampTemp<-unlist(strsplit(partial, split = "_"))
date<-timeStampTemp[length(timeStampTemp)-1]
time<-timeStampTemp[length(timeStampTemp)]

Sys.setenv(TZ = "UTC")

timeStamp<-as.POSIXct(paste(date,time),format="%Y%m%d %H%M")
originalPath<-fileToAnalyze

locationAndID<-timeStampTemp[1]


home<-"/workspace/andrea/"

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

prediction <- h2o.mojo_predict_df(featuresImage,
                    mojo_zip_path =  "/workspace/andrea/exports/models/dl_grid_model_35.zip", 
                    classpath = "/usr/local/lib/R/site-library/h2o/java/h2o.jar", 
                    genmodel_jar_path = "/usr/local/lib/R/site-library/h2o/java/h2o.jar")




fogClass<-prediction$predict


final<-cbind(cameraTarget,fileLocation,originalPath, timeStamp,fogClass)


exportJson <- toJSON(final, pretty = TRUE)
write(exportJson, "/workspace/andrea/exports/results/predictions/test.json")


jsoninput<-jsonlite::fromJSON("/workspace/andrea/exports/results/predictions/test.json")





###FIRST ATTEMPT OF VISUALIZATION

library(leaflet)
library(unixtools)
library(plyr)

set.tempdir("/workspace/andrea/tmp")

factpal <- colorFactor(topo.colors(2), c(TRUE,FALSE))



icon.bad <- makeAwesomeIcon( markerColor = 'red', icon = "camera")
icon.good <- makeAwesomeIcon(markerColor = 'green', icon = "camera")
myIcons<-awesomeIconList(noFog = icon.good, fog = icon.bad)

inputDF<-data.frame(jsoninput,stringsAsFactors = F)
inputDF

inputDF<-inputDF[rep(1:nrow(inputDF),each=2),] 

inputDF[2,10]=TRUE

inputDF[2,3]=5.1115

inputDF

inputDF$fogClass<-as.factor(inputDF$fogClass)
#inputDF$graphicClass<if(inputDF$fogClass==FALSE){"noFog"}
#inputDF$graphicClass<-as.factor(inputDF$graphicClass)
#inputDF$fogClass<-revalue(inputDF$fogClass, c("FALSE"="nofog", "TRUE"="fog"))

inputDF$icon <- factor(inputDF$fogClass,
                    levels = c("TRUE","FALSE"),
                    labels = c("red", "green")) 

inputDF
inputDF$longitude<-as.numeric(inputDF$longitude)
inputDF$latitude<-as.numeric(inputDF$latitude)


icons <- awesomeIcons(icon = "camera",
                      iconColor = "black",
                      library = "ion",
                      markerColor = inputDF$icon)



inputDF$hyperink<-paste0('<a href="',inputDF$ipAddr,'">View Camera ', inputDF$location," " ,inputDF$cameraID,'</a>')

#icon11<-myIcons[inputDF$graphicClass]
m <- leaflet(inputDF) %>%
  addTiles() %>%  # Add default OpenStreetMap map tiles
  addAwesomeMarkers( ~longitude, ~latitude, icon = icons, popup = ~hyperink)
m  # Print the map


