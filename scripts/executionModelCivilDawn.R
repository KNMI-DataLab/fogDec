#!/usr/bin/env Rscript
args = commandArgs(trailingOnly=TRUE)


# test if there is at least one argument: if not, return an error
if (length(args)==0) {
  stop("At least one argument must be supplied (input file).jpg", call.=FALSE)
} else if (length(args)==1) {
  fileToAnalyze = args[1]
}


log_file<-"/home/pagani/development/fogDec/log/predictionEngine.log"  

library(logging)

logReset()
basicConfig(level='FINEST')
addHandler(writeToFile, file=log_file, level='DEBUG')
with(getLogger(), names(handlers))



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
    #image<-blur_anisotropic(image, amplitude = 10000)
    df<-as.data.frame(image)
    v<-df$value
    #mat<-rbind(mat,v)
    v
  }
}





library(RJSONIO)

#'/nas-research.knmi.nl/sensordata/CAMERA/DEBILT/TESTSITE-SNOWDEPTH/201807/DEBILT-TESTSITE-SNOWDEPTH_20180710_0811.jpg

#fileToAnalyze<-"/nas-research.knmi.nl/sensordata/CAMERA/DEBILT/TESTSITE-SNOWDEPTH/201807/DEBILT-TESTSITE-SNOWDEPTH_20180710_0811.jpg"

#fileToAnalyze<-"/nas-research.knmi.nl/sensordata/CAMERA/RWS/A16/HM211/ID71681/201807/A16-HM211-ID71681_20180726_1220.jpg"

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



remote=FALSE

if(remote==TRUE){
  model_zip_path = "/workspace/andrea/exports/models/dl_grid_model_35.zip"
  h2o_jar_path = "/usr/local/lib/R/site-library/h2o/java/h2o.jar"
  devel_dir<-"/workspace/andrea/"
  #for amazon###
  fileLocation<-"/workspace/andrea/exports/A2-HM776-ID10915_20180606_0801.jpg"
  #####
  results_json<-"/workspace/andrea/exports/results/predictions/test.json"
  temp_directory<-"/workspace/andrea/tmp"
}else{
  model_zip_path = "/home/pagani/nndataH2O/frozenModels/modelDAWN/civil/dl_grid_model_8.zip"
  h2o_jar_path = "/usr/lib64/R/library/h2o/java/h2o.jar"
  devel_dir<-"/home/pagani/development/"
  results_json<-"/home/pagani/nndataH2O/frozenModels/results/predictions/test.json"
  temp_directory<-"/home/pagani/temp/Rtemp/"
  queue_conf_file<-"/home/pagani/development/fogDec/inst/extScripts/python/queueConfig.json"
  
}







jsonCameras<-paste0(devel_dir,"fogDec/inst/extScripts/python/MVPCameras.json")

camerasDF<-jsonlite::fromJSON(txt = jsonCameras)

camerasRWS<-camerasDF$cameras$RWS


pos<-gregexpr("-",locationAndID)
lastDash<-pos[[1]][length(pos[[1]])]
location<-substring(locationAndID,1,lastDash-1)
cameraID<-substring(locationAndID,lastDash+1,str_length(locationAndID))

logdebug("looking if camera is in the allowed list")

cameraTarget<-camerasRWS[camerasRWS$location==location & camerasRWS$cameraID==cameraID,]
if(dim(cameraTarget)[1]==0){
  stop("camera not in the list for fog detection")
}
cbind(cameraTarget,fileLocation,originalPath, timeStamp)

print(fileLocation)

library(imager)
library(h2o)

featuresImage<-fromImageToFeatures(fileLocation)
featuresImage<-t(featuresImage)




logdebug(paste("starting prediction for", args))
prediction <- h2o.mojo_predict_df(featuresImage,
                    mojo_zip_path =  model_zip_path, 
                    classpath = h2o_jar_path, 
                    genmodel_jar_path = h2o_jar_path)


logdebug(paste("finished prediction for", args))


fogClass<-prediction$predict

predTRUE<-prediction$TRUE.
predFALSE<-prediction$FALSE.


final<-cbind(cameraTarget,fileLocation,originalPath, timeStamp,fogClass,predTRUE,predFALSE)

logdebug(paste("making a dataframe for", args))


message(final)


#final<-cbind(cameraTarget,fileLocation,originalPath, timeStamp,fogClass)

logdebug(paste("making JSON in prediction for", args))
exportJson <- jsonlite::toJSON(final)
#write(exportJson, results_json)
logdebug(paste("written JSON results export object", args))



library(rgdal)
library(raster)

###TEST STUFF GEO JSON: have to use the writeOGR from rgdal since the package geojsonio requires GDAL 2.0 that is not
###available on the redhat at the moment
###with geojsonio would be easier to produce a geojson string without need to write and read a file
coordinates(final)=cbind(as.numeric(final$longitude),as.numeric(final$latitude))
crs(final)<-"+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
tempFileName<-uuid::UUIDgenerate()
tempPathJsonFile<-paste0(temp_directory,tempFileName)
writeOGR(final,tempPathJsonFile,layer = "final", driver = "GeoJSON")
##################################



#library(messageQueue)



jsonQueue<-jsonlite::fromJSON(queue_conf_file)

#set of options not working at the moment



charPayload<-readr::read_file(tempPathJsonFile)

if (file.exists(tempPathJsonFile)) file.remove(tempPathJsonFile)

#OLD VERSION NO GEO JSON BUT READING STRING
#charPayload<-as.character(exportJson)

logdebug(paste("read JSON export object for", args))

#OLD non geo json
#escapedPayload<-gsub("([\\\"])",'\\\\"', charPayload)

escapedPayload<-gsub("([\"])",'\\\\"', charPayload)


#escapedPayload<-gsub("\\\\","",escapedPayload)


#escapedPayload<-charPayload

logdebug(paste("sending JSON to RabbitMQ for", args))

#putting a message in the queue via POST since R library working with rabbit is not working
command<-paste0('curl -u ', jsonQueue$user,':', jsonQueue$pw, ' -H "Content-type: application/json" -X POST -d\'{"properties":{"delivery_mode":1, "content_type": "application/json"},
"routing_key":"","payload":"',escapedPayload,'","payload_encoding":"string"}\' http://',jsonQueue$host,':8080/api/exchanges/%2f/detectionOutput/publish')
#command
tryCatch({
system(command)
},
error=function(cond) {
  message(paste("error executing CURL command"))
  #message("Here's the original error message:")
  #message(cond)
  # Choose a return value in case of error
  return(NA)
})
message("prediction sent to the queue for visualization")
logdebug(paste("prediction sent to the queue for visualization for", args))



# ###FIRST ATTEMPT OF VISUALIZATION
# 
# library(leaflet)
# library(unixtools)
# library(plyr)
# 
# set.tempdir(temp_directory)
# 
# factpal <- colorFactor(topo.colors(2), c(TRUE,FALSE))
# 
# 
# 
# icon.bad <- makeAwesomeIcon( markerColor = 'red', icon = "camera")
# icon.good <- makeAwesomeIcon(markerColor = 'green', icon = "camera")
# myIcons<-awesomeIconList(noFog = icon.good, fog = icon.bad)
# 
# inputDF<-data.frame(jsoninput,stringsAsFactors = F)
# inputDF
# 
# inputDF<-inputDF[rep(1:nrow(inputDF),each=2),] 
# 
# inputDF[2,10]=TRUE
# 
# inputDF[2,3]=5.1115
# 
# inputDF
# 
# inputDF$fogClass<-as.factor(inputDF$fogClass)
# #inputDF$graphicClass<if(inputDF$fogClass==FALSE){"noFog"}
# #inputDF$graphicClass<-as.factor(inputDF$graphicClass)
# #inputDF$fogClass<-revalue(inputDF$fogClass, c("FALSE"="nofog", "TRUE"="fog"))
# 
# inputDF$icon <- factor(inputDF$fogClass,
#                     levels = c("TRUE","FALSE"),
#                     labels = c("red", "green")) 
# 
# inputDF
# inputDF$longitude<-as.numeric(inputDF$longitude)
# inputDF$latitude<-as.numeric(inputDF$latitude)
# 
# 
# icons <- awesomeIcons(icon = "camera",
#                       iconColor = "black",
#                       library = "ion",
#                       markerColor = inputDF$icon)
# 
# 
# 
# inputDF$hyperink<-paste0('<a href="',inputDF$ipAddr,'">View Camera ', inputDF$location," " ,inputDF$cameraID,'</a>')
# 
# #icon11<-myIcons[inputDF$graphicClass]
# m <- leaflet(inputDF) %>%
#   addTiles() %>%  # Add default OpenStreetMap map tiles
#   addAwesomeMarkers( ~longitude, ~latitude, icon = icons, popup = ~hyperink)
# m  # Print the map


