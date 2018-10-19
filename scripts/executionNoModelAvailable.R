#######################################
#Script called in the conditions
#in which a model is not available
######################################

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
  model_zip_path = "/home/pagani/nndataH2O/frozenModels/dl_grid_model_35.zip"
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



fogClass<-"UNKNOWN"

predTRUE<-"NA"
predFALSE<-"NA"


final<-cbind(cameraTarget,fileLocation,originalPath, timeStamp,fogClass,predTRUE,predFALSE)

logdebug(paste("making a dataframe for", args))


message(final)


#final<-cbind(cameraTarget,fileLocation,originalPath, timeStamp,fogClass)

logdebug(paste("making JSON in prediction for", args))
exportJson <- jsonlite::toJSON(final)
#write(exportJson, results_json)
logdebug(paste("written JSON results export object", args))


#jsoninput<-jsonlite::fromJSON(results_json)


library(messageQueue)


jsonQueue<-jsonlite::fromJSON(queue_conf_file)

#set of options not working at the moment


charPayload<-as.character(exportJson)
logdebug(paste("read JSON export object for", args))

escapedPayload<-gsub("([\\\"])",'\\\\"', charPayload)


logdebug(paste("sending JSON to RabbitMQ for", args))

#putting a message in the queue via POST since R library working with rabbit is not working
command<-paste0('curl -u ', jsonQueue$user,':', jsonQueue$pw, ' -H "Content-type: application/json" -X POST -d\'{"properties":{"delivery_mode":1, "content_type": "application/json"},
"routing_key":"RTvisual", "declare_queue":"RTvisual", "payload":"',escapedPayload,'","payload_encoding":"string"}\' http://',jsonQueue$host,':8080/api/exchanges/%2f/amq.default/publish')
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
message("model not available setting detection to NA")
logdebug(paste("model not available setting detection to NA", args))




