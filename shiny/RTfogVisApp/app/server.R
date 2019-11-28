library(data.table)
library(leaflet)
library(unixtools)
library(plyr)
library(shiny)
library(logging)
library(mapview)
library(DBI)
library(jsonlite)
library(mongolite)

library(RJSONIO)
library(stringr)
library(logging)
library(imager)
library(h2o)
##########
#local and remote implementation variable naming/setting
##########


firstOccurrence<<-TRUE

  temp_directory<-"/external/temp/"
  df_debug_file<-"/external/debug/debugDF.csv"
  log_file<-"/external/log/logFile.log"
  state_file<-"/external/state/currentState.json"
  cameras_for_detection_file<-"/external/config/MVPCameras.json"
  queue_conf_file<-"/external/config/queueConf.json"
  DB_conf_file<-"/external/config/configDB.json"
  #tempImagesStorage<-"/external/tempImageStorage/"
  imagesLocationDetection<-"/external/pictures/detection/"
  imagesLocationValidation<-"/external/pictures/validation/"
  modelsPath<-"/external/models/"
  h2o_jar_path = "/usr/local/lib/R/site-library/h2o/java/h2o.jar"
  ####AWS config part###
  aws_S3_config = "/external/config/S3config.json"
  archive_detection_DB_config = "/external/config/mongoConfig.json"
  
  

  
  
  #local config
  # temp_directory<-"/home/pagani/temp/"
  # df_debug_file<-"/home/pagani/temp/debug/debugDF.csv"
  # log_file<-"/home/pagani/temp/log/logFile.log"
  # state_file<-"/home/pagani/temp/state/currentState.json"
  # cameras_for_detection_file<-"/home/pagani/temp/config/MVPCameras.json"
  # queue_conf_file<-"/home/pagani/temp/config/queueConfig.json"
  # DB_conf_file<-"/home/pagani/temp/config/configDB.json"
  # temp_directory<-"/home/pagani/temp/Rtemp"
  # temp_directory<-"/tmp"
  # imagesLocationDetection<-"/home/pagani/share/"
  # tempImagesStorage<-"/data2/temp/tempPicFogVis/"
  # modelsPath<-"/home/pagani/nndataH2O/frozenModels/usedModelsInPOC/"
  # h2o_jar_path = "/usr/lib64/R/library/h2o/java/h2o.jar"
  # aws_S3_config<- "/home/pagani/temp/S3config.json"
  # imagesLocationDetection<-"/data2/pictures/detection/"
  # imagesLocationValidation<-"/data2/pictures/validation/"
  # 
  
  model_zip_path_day <- paste0(modelsPath,"dl_grid_model_35.zip")
  model_zip_path_civil_dawn <- paste0(modelsPath,"dl_grid_model_8.zip")
  model_zip_path_nautical_dawn <- paste0(modelsPath,"dl_grid_model_15.zip")
  model_zip_path_night <-paste0(modelsPath, "dl_grid_model_NIGHT_15.zip")
  
  
  
  
  library(aws.s3)
  ##AWS S3 section to access to images
  S3config<-jsonlite::fromJSON(aws_S3_config)
  
  Sys.setenv("AWS_DEFAULT_REGION" = S3config$region_name,
             "AWS_ACCESS_KEY_ID" = S3config$aws_access_key_id,
             "AWS_SECRET_ACCESS_KEY" = S3config$aws_secret_access_key
  )
  
  print("initialized S3 credentials")
  
  


message("visualization platform ready")
logReset()
basicConfig(level='FINEST')
addHandler(writeToFile, file=log_file, level='DEBUG')
with(getLogger(), names(handlers))


set.tempdir(temp_directory)


prepareDBconnection<-function(){
  dbConfig <- fromJSON(DB_conf_file)
  
  connectionSetup <- dbConnect(RPostgreSQL::PostgreSQL(),
                               dbname = "FOGDB",
                               host = dbConfig[["host"]], port = 9418, # use 9418 within KNMI, default would be 5432. At the moment set to 9418
                               user = dbConfig[["user"]], password = dbConfig[["pw"]])
  connectionSetup
}



query_camera_id<-function(mongoCameraID){
connectionSetup <-prepareDBconnection()
queryString<- paste0("select camera_id from cameras where camera_name='",mongoCameraID,"';")
camera_id<- dbGetQuery(connectionSetup, queryString)
dbDisconnect(connectionSetup)
camera_id
}

queryDBforImage<-function(){


connectionSetup <-prepareDBconnection()

#queryString<- "select * from images where random()<0.001 and day_phase in (1,0,20,10) and camera_id>260 limit 1;"

#the view that is used is created as follows in the database:
#
#create view non_evaluated_images_RWS as SELECT t1.*
#FROM images t1
#LEFT JOIN manual_annotations t2 ON t2.image_id = t1.image_id
#WHERE t2.image_id IS NULL and t1.camera_id>260  limit 10000;

queryString<- "select * from non_evaluated_images_rws where random()<0.01 and day_phase in (1,0,20,10) and camera_id>260 limit 1;"


imageToValidate <- dbGetQuery(connectionSetup, queryString)

dbDisconnect(connectionSetup)

print(imageToValidate)

imageToValidate
}

queryMongoDetectionArchive <- function(){
mongoConfig <- fromJSON(archive_detection_DB_config)
m <- mongo("collection", url = paste0("mongodb://",mongoConfig[["host"]],":",mongoConfig[["port"]],"/fogDetectionArchive"))
#sample one foggy case in the archive
foggyCase <- m$aggregate('[{"$match":{"features.properties.fogClass":1}},{"$sample":{"size":1}}]')

dfFoggy<-lapply(foggyCase$features,function(x){(x$properties)})
dfFoggy<-do.call(rbind,dfFoggy)

dfFoggy


}




#######Run prediciton on picture#########



logReset()
basicConfig(level='FINEST')
addHandler(writeToFile, file=log_file, level='DEBUG')
with(getLogger(), names(handlers))



fromImageToFeatures<-function(filename){
  #print(filename)
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


convertToLocalFilepath<-function(remoteFilepath){
  fileLocation<-gsub(".*/RWS/", "", remoteFilepath)
  fileLocation
}

predictImage<-function(filename, dayPhaseImage){
  

partial<-str_extract(pattern = "[^/]*$", string =  filename)
partial<-str_extract(string = partial, pattern = ".*(?=\\.)")
timeStampTemp<-unlist(strsplit(partial, split = "_"))
date<-timeStampTemp[length(timeStampTemp)-1]
time<-timeStampTemp[length(timeStampTemp)]

Sys.setenv(TZ = "UTC")

timeStamp<-as.POSIXct(paste(date,time),format="%Y%m%d %H%M")
originalPath<-filename

locationAndID<-timeStampTemp[1]



# remote=FALSE
# 
# if(remote==TRUE){
#   model_zip_path = "/workspace/andrea/exports/models/dl_grid_model_35.zip"
#   h2o_jar_path = "/usr/local/lib/R/site-library/h2o/java/h2o.jar"
#   devel_dir<-"/workspace/andrea/"
#   #for amazon###
#   fileLocation<-"/workspace/andrea/exports/A2-HM776-ID10915_20180606_0801.jpg"
#   #####
#   results_json<-"/workspace/andrea/exports/results/predictions/test.json"
#   temp_directory<-"/workspace/andrea/tmp"
#   ##TO BE CHNAGED THE LOCATIONS
#   model_zip_path_day = "/home/pagani/nndataH2O/frozenModels/usedModelsInPOC/dl_grid_model_35.zip"
#   model_zip_path_civil_dawn = "/home/pagani/nndataH2O/frozenModels/usedModelsInPOC/dl_grid_model_8.zip"
#   model_zip_path_nautical_dawn = "/home/pagani/nndataH2O/frozenModels/usedModelsInPOC/dl_grid_model_15.zip"
#   model_zip_path_night = "/home/pagani/nndataH2O/frozenModels/usedModelsInPOC/dl_grid_model_NIGHT_15.zip"
# }else{
#   model_zip_path_day <- paste0(modelsPath,"dl_grid_model_35.zip")
#   model_zip_path_civil_dawn <- paste0(modelsPath,"dl_grid_model_8.zip")
#   model_zip_path_nautical_dawn <- paste0(modelsPath,"dl_grid_model_15.zip")
#   model_zip_path_night <-paste0(modelsPath, "dl_grid_model_NIGHT_15.zip")
#   #devel_dir<-"/home/pagani/development/"
#   #results_json<-"/home/pagani/nndataH2O/frozenModels/results/predictions/test.json"
# 
# }





#print(filename)



featuresImage<-fromImageToFeatures(filename)
featuresImage<-t(featuresImage)

model <- c(model_zip_path_day, model_zip_path_civil_dawn, model_zip_path_nautical_dawn, model_zip_path_night)
names(model) <- c("1", "10", "20","0")



modelPath<-model[[as.character(dayPhaseImage)]]
message(modelPath)
message(dayPhaseImage)

#logdebug(paste("starting prediction for", args))
prediction <- h2o.mojo_predict_df(featuresImage,
                                  mojo_zip_path =  modelPath, 
                                  classpath = h2o_jar_path, 
                                  genmodel_jar_path = h2o_jar_path)


#logdebug(paste("finished prediction for", args))


fogClass<-prediction$predict

predTRUE<-prediction$TRUE.
predFALSE<-prediction$FALSE.

return(list(fogClass,predTRUE,predFALSE,modelPath))

}







shinyServer(function(input, output, session) {
  #initialization (have to check what is needed after development, might not be needed) 
  
  
  
  jsonCameras<-jsonlite::fromJSON(cameras_for_detection_file)
  dfCameras<-data.frame(jsonCameras$cameras$RWS)
  dfCameras$longitude<-as.numeric(dfCameras$longitude)
  dfCameras$latitude<-as.numeric(dfCameras$latitude)
  
  iconsInit <- awesomeIcons(icon = "camera",
                            iconColor = "black",
                            library = "ion",
                            markerColor = "gray")
  
  
  #output$timeString<-renderUI({HTML('<div class="centered">Last Updated:', as.character(as.POSIXlt(Sys.time(), "UTC")),"UTC  </div><br>")})
  
  
  html_legend <- paste0("<link href='shared/font-awesome/css/font-awesome.min.css'/><div class='bold'>
                        <img src='iconGreenNoBack.png' style='width:15px;height:20px;'>   NO FOG<br/>
                        <img src='iconRedNoBack.png'style='width:15px;height:20px;'>  FOG<br/>
                        <img src='iconGreyNoBack.png' style='width:15px;height:20px;'>  NA<br/></div>")
  
  
  
  
  dfCameras$hyperink<-paste0('<a href="',dfCameras$ipAddr,'" target="_blank">View Camera ', dfCameras$location," " ,dfCameras$cameraID,  '</a>')
  
  
  
  
  mapInit<-leaflet(dfCameras) %>% addTiles() %>%  addAwesomeMarkers( ~longitude, ~latitude, icon = iconsInit, popup = ~hyperink ) %>% addControl(html= html_legend, position = "topright")
  
  
  
  output$map<-renderLeaflet(mapInit)
  
  
  
  
  
  jsonQueue<-jsonlite::fromJSON(queue_conf_file)
  
  
  
  ###FIRST ATTEMPT OF VISUALIZATION
  
  #commandretrieve<-'curl -i -u guest:guest -H "content-type:application/json" -X POST http://145.23.219.231:8080/api/queues/%2f/RTvisual/get -d\'{"count":5,"requeue":true,"encoding":"auto","truncate":500000}\''
  #testRec<-system(commandretrieve)
  
  #fetching messages
  library(curl)
  h <- new_handle()
  handle_setopt(h,USERNAME=jsonQueue$user, PASSWORD=jsonQueue$pw, POST=1, POSTFIELDS='{"count":10000,"ackmode":"ack_requeue_false","encoding":"auto"}' )
  handle_setheaders(h,"Content-Type" = "application/json")
  
  
  
  
  fromJSONtoDF<-function(text){
    parsedJSON<-jsonlite::fromJSON(text)
    #print(parsedJSON)
    jsoninput<-lapply(parsedJSON$payload,jsonlite::fromJSON)
    #print(df)
    df <- data.frame(matrix(unlist(jsoninput), nrow=length(jsoninput), byrow=T), stringsAsFactors = F)
    #extracting only the relevant columns for the purpose of visualization from the GeoJson data-framed structure
    df<-df[,6:17]
    #and setting the related names
    colnames(df)<-c("id","location", "cameraID", "longitude", "latitude", "ipAddr", "_note", "fileLocation", "originalPath", "timeStamp", "fogClass", "predTRUE")
    #names(jsoninput[[1]][[3]][[3]])
    df
  }
  
  
  
  
  
  visualizeResults<-function(df)
  {
    if(is.null(df)==FALSE){
      message(max(df$timeStamp))
      output$timeString<-renderUI({HTML('<div class="centered">Last Updated:', as.character(max(df$timeStamp)),"UTC  </div><br>")})
      
      
      #set.tempdir(temp_directory)
      
      #setting icons to relevant colors and design
      # factpal <- colorFactor(topo.colors(2), c(TRUE,FALSE))
      # icon.bad <- makeAwesomeIcon( markerColor = 'red', icon = "camera")
      # icon.good <- makeAwesomeIcon(markerColor = 'green', icon = "camera")
      # icon.na<- makeAwesomeIcon(markerColor = 'gray', icon = "camera")
      #
      # myIcons<-awesomeIconList(noFog = icon.good, fog = icon.bad, notAv = icon.na)
      
      
      #
      #inputDF<-data.frame(jsoninput,stringsAsFactors = F)
      #inputDF
      
      #inputDF<-inputDF[rep(1:nrow(inputDF),each=2),]
      #inputDF[2,10]=TRUE
      #inputDF[2,3]=5.1115
      #inputDF
      
      
      
      
      jsonCameras<-jsonlite::fromJSON(cameras_for_detection_file)
      dfCameras<-data.frame(jsonCameras$cameras$RWS)
      dfCameras$longitude<-as.numeric(dfCameras$longitude)
      dfCameras$latitude<-as.numeric(dfCameras$latitude)
      
      numCamerasToMonitor<-dim(dfCameras)[[1]]
      numCamerasRetrieved<-dim(df)[[1]]
      
      if(numCamerasRetrieved!=numCamerasToMonitor){
        loginfo(paste(numCamerasRetrieved,"retrieved cameras, time of retrieval",as.character(max(df$timeStamp)),"UTC"))
        loginfo(paste("missing cameras first 100:",head(dfCameras$location[!(dfCameras$cameraID %in% df$cameraID)]),100))
      }
      
      
      
      
      df$fogClass<-as.factor(df$fogClass)
      #inputDF$graphicClass<if(inputDF$fogClass==FALSE){"noFog"}
      #inputDF$graphicClass<-as.factor(inputDF$graphicClass)
      #inputDF$fogClass<-revalue(inputDF$fogClass, c("FALSE"="nofog", "TRUE"="fog"))
      
      df$icon <- factor(df$fogClass,
                        levels = c("1","0","UNKNOWN"),
                        labels = c("red", "green","gray"))
      
      df
      df$longitude<-as.numeric(df$longitude)
      df$latitude<-as.numeric(df$latitude)
      
      
      #message("################TESTTTTTTTTTTTTTTTTTTTTTT################")
      
      
      message("dataframe created")
      message(paste("saving dataframe for debug purposes in ",df_debug_file))
      write.csv(df,file = df_debug_file )
      
      missing<-dfCameras [ !dfCameras$cameras.RWS.cameraID %in% df$cameraID ,]
      
      iconsMissing <- awesomeIcons(icon = "camera",
                                   iconColor = "black",
                                   library = "ion",
                                   markerColor = "gray")
      
      icons <- awesomeIcons(icon = "camera",
                            iconColor = "black",
                            library = "ion",
                            markerColor = df$icon
      )
      
      
      #print('#######')
      #print(df[1])
      #print('#######')
      
      
      #df$localFileLocation<-gsub("pictures/","/external/pictures/",df$fileLocation)
      ######TO BE REMOVED####
      #unlink(tempImagesStorage)
      df$localFileLocation<-gsub("pictures/",imagesLocationDetection,df$fileLocation)
      df$filename<-basename(df$localFileLocation)
      
      
      localImageFilepath<-convertToLocalFilepath(df$fileLocation)
      #filenameImage<-basename(localImageFilepath)
#	print(df$fileLocation)
#	print(localImageFilepath)


      
      
      
      
      #file.copy(df$localFileLocation,tempImagesStorage)
      
      ################
      localTempSavedLocation <- paste0(imagesLocationDetection,df$filename)


      saveMultipleObjects<-function(AWSPath){
      localTempSavedLocation <-paste0(imagesLocationDetection,basename(AWSPath))
      save_object(object = AWSPath, bucket = 'knmi-fogdetection-dataset',
                  file = localTempSavedLocation)
    #  print(paste("file location",localTempSavedLocation))
    #  print("object saved real time detection")
	}
      

      lapply(localImageFilepath, saveMultipleObjects)	
      
      
      
      
      #################
      
      
      df$localFileLocation<-localTempSavedLocation
      ###########
      
      #print(df$localFileLocation)
      objs <- data.table(filenames=rownames(file.info(df$localFileLocation)),file.info(df$localFileLocation))
      #print(objs)
      goodPics<-objs[objs$size > 10] #bigger than 10 bytes
      goodPics<-data.table(goodPics)
      setkey(goodPics,filenames)
      df<-data.table(df)
      setkey(df,localFileLocation)
      dfGoodPics<-df[goodPics,nomatch=0]
      #print(dfGoodPics)
      popupFilenames<-as.vector(dfGoodPics$localFileLocation)
      
      
      
      #print(popupFilenames)
      
      
      dfGoodPics$hyperink<-paste0('<a href="',dfGoodPics$ipAddr,'" target="_blank">View Camera ', dfGoodPics$location," " ,dfGoodPics$cameraID,  '</a>')
      
      #message('##############before  output maps#######################')
      
      
      if(nrow(missing)!=0){
        missing$hyperink<-paste0('<a href="',missing$cameras.RWS.ipAddr,'">View Camera ',missing$cameras.RWS.location," " ,missing$cameras.RWS.cameraID,'</a>')
        m <- leaflet() %>%
          addTiles() %>%  # Add default OpenStreetMap map tiles
          addAwesomeMarkers(data=dfGoodPics, ~longitude, ~latitude, icon = icons, popup =mapview::popupImage(popupFilenames,src = "local", embed = T)) %>% addAwesomeMarkers(data=missing,~cameras.RWS.longitude, ~cameras.RWS.latitude, icon = iconsMissing, popup=~hyperink) %>% addControl(html= html_legend, position = "topright")
        #addCircleMarkers(data=dfGoodPics, ~longitude, ~latitude, popup = ~hyperink) #%>% addAwesomeMarkers(data=missing,~cameras.RWS.longitude, ~cameras.RWS.latitude, icon = iconsMissing, popup=~hyperink) %>% addControl(html= html_legend, position = "topright")

      }else{
        #message('##############no missing rows#######################')
        m <- leaflet() %>%
          addTiles() %>%  # Add default OpenStreetMap map tiles
          addAwesomeMarkers(data=dfGoodPics, ~longitude, ~latitude, icon = icons, popup =mapview::popupImage(popupFilenames,src = "local", embed = T)) %>%
        addControl(html= html_legend, position = "topright")
        #addCircleMarkers(data=dfGoodPics, ~longitude, ~latitude,  popup = ~hyperink) #%>% 
         # addAwesomeMarkers(data=missing,~cameras.RWS.longitude, ~cameras.RWS.latitude, icon = iconsMissing, popup=~hyperink) %>% addControl(html= html_legend, position = "topright")
        
      }
      
      
      
      #message('##############printing output maps#######################')
      #output$timeString<-renderUI({HTML('<div class="centered">Last Updated:', as.character(as.POSIXlt(Sys.time(), "UTC")),"UTC  </div><br>")})
      
      #icon11<-myIcons[inputdfGoodPics$graphicClass]
      
      output$map <-renderLeaflet(m) 
    }
  }
  
  

    
    ###################MANAGING OF THE VALIDATION PART#######################
    

  getAndShowNewImage<-function(){

  #random sample from the metadataDB and from the archive of foggy detected images
  #15% of the times a detected foggy image should be called
  randNum<-sample(1:100,1)
  if(randNum<=15){
  mongoRecord<-queryMongoDetectionArchive()
  imagename<-mongoRecord$originalPath

  camera_id<-query_camera_id(mongoRecord$cameraID)

  #camera_id<-mongoRecord$cameraID
  timestamp<-mongoRecord$timeStampMongoFormat
  image_id<-NA
  if(mongoRecord$fogClass==1){
	    fogChar<-"FOG"
   }else{
	    fogChar<-"NO FOG"
	  }
  visibility_qualitative_detection_model<-fogChar
  detection_model_name<-NA
  probFog<-mongoRecord$predTRUE
  probNoFog<-mongoRecord$predFALSE


  }else{
  imageDBrecord<-queryDBforImage()
  imagename<-imageDBrecord$filepath
  dayPhaseImage<-imageDBrecord$day_phase
  image_id<-imageDBrecord$image_id
  camera_id<-imageDBrecord$camera_id
  timestamp<-imageDBrecord$timestamp
  }
  

  #message(paste0("camera_id is ",camera_id))
  localImageFilepath<-convertToLocalFilepath(imagename)
  filenameImage<-basename(localImageFilepath)
  localTempSavedLocation <- paste0(imagesLocationValidation,filenameImage) 
  head_obj<-head_object(object = localImageFilepath, bucket = 'knmi-fogdetection-dataset')

  if(head_obj==TRUE){
  save_object(object = localImageFilepath, bucket = 'knmi-fogdetection-dataset',
              file = localTempSavedLocation)
  } else{
    #print("inside error")
    DFannotation<-NULL
    #print(DFannotation)
    return(DFannotation)
  }
  #print(paste("file location",localTempSavedLocation))
  #print("object saved")
  
  
  
  visibility_qualitative_annotator<-NA
  annotator_name<-Sys.getenv("SHINYPROXY_USERNAME")
  loginfo(paste("annotator",annotator_name))
     
  if(randNum>15){
  fogginess<-predictImage(localTempSavedLocation, dayPhaseImage)
  if(fogginess[[1]]){
    fogChar<-"FOG"
  }else{
    fogChar<-"NO FOG"
    }
  visibility_qualitative_detection_model<-fogChar
  detection_model_name<-basename(fogginess[[4]])
  probFog<-fogginess[[2]]
  probNoFog<-fogginess[[3]]
  }

  

  DFannotation<-data.frame(camera_id,timestamp,image_id,visibility_qualitative_annotator,annotator_name,visibility_qualitative_detection_model,detection_model_name)
  
  output$FogBinary<-renderUI({HTML('Machine classification is:', fogChar)})
  output$probFog<-renderUI({HTML('Probability of fog in the  image:',as.character(round(100*probFog,2)),"%")})
  output$probNoFog<-renderUI({HTML('Probability of non-fog in the  image:',as.character(round(100*probNoFog,2)),"%")})
  
  #print(fogginess)
  
  
  output$images <- renderImage({
    # Return a list containing the filename
    list(src = localTempSavedLocation,
         contentType = 'image/png',
         #width = 400,
         #height = 300,
         alt = "This is alternate text")
  }, deleteFile = TRUE)
  DFannotation
  
  }

  
  
  
  
  dfInitial<<-getAndShowNewImage()
  while(is.null(dfInitial)){
    unlink(paste0(imagesLocationValidation,"*.jpg"))
    dfInitial<<-getAndShowNewImage()  
    
  }
  dfValid<<-NULL
  observeEvent(input$FOGbutton, {
    if(is.null(dfInitial)){
    if(is.null(dfValid)==FALSE){
    dfValid$visibility_qualitative_annotator<-"FOG"
    print(dfValid)
    con<-prepareDBconnection()
    dbWriteTable(con, "manual_annotations", dfValid, append = TRUE, row.names = FALSE, match.cols = TRUE)
    dbDisconnect(con)
    }
    unlink(paste0(imagesLocationValidation,"*.jpg"))
    dfValid<<-getAndShowNewImage()
    print(dfValid)
    Sys.sleep(0.3)
    } else{
      
      if(is.null(dfInitial)==FALSE){
        dfInitial$visibility_qualitative_annotator<-"FOG"
        print(dfInitial)
      con<-prepareDBconnection()
      dbWriteTable(con, "manual_annotations", dfInitial, append = TRUE, row.names = FALSE, match.cols = TRUE)
      dbDisconnect(con)
      }
      dfInitial<<-NULL
      unlink(paste0(imagesLocationValidation,"*.jpg"))
      dfValid<<-getAndShowNewImage()
      print(dfValid)
    }

  })

  observeEvent(input$NOFOGbutton, {
    if(is.null(dfInitial)){
      
      if(is.null(dfValid)==FALSE){
        dfValid$visibility_qualitative_annotator<-"NO FOG"
        print(dfValid)
      con<-prepareDBconnection()
      dbWriteTable(con, "manual_annotations", dfValid, append = TRUE, row.names = FALSE, match.cols = TRUE)
      dbDisconnect(con)
      }
      unlink(imagesLocationValidation)
      dfValid<<-getAndShowNewImage()
      print(dfValid)
      Sys.sleep(0.3)

    } else{
      
      if(is.null(dfInitial)==FALSE){
        dfInitial$visibility_qualitative_annotator<-"NO FOG"
        print(dfInitial)
      con<-prepareDBconnection()
      dbWriteTable(con, "manual_annotations", dfInitial, append = TRUE, row.names = FALSE, match.cols = TRUE)
      dbDisconnect(con)
      }
      dfInitial<<-NULL
      unlink(paste0(imagesLocationValidation,"*.jpg"))
      dfValid<<-getAndShowNewImage()
      print(dfValid)
    }

  })

  observeEvent(input$cannotButton, {
    if(is.null(dfInitial)){
      
      if(is.null(dfValid)==FALSE){
        dfValid$visibility_qualitative_annotator<-"CANNOT SAY"
        print(dfValid)
      con<-prepareDBconnection()
      dbWriteTable(con, "manual_annotations", dfValid, append = TRUE, row.names = FALSE, match.cols = TRUE)
      dbDisconnect(con)
      }
      unlink(paste0(imagesLocationValidation,"*.jpg"))
      dfValid<<-getAndShowNewImage()
      print(dfValid)
      Sys.sleep(0.3)

    } else{
      
      if(is.null(dfInitial)==FALSE){
        dfInitial$visibility_qualitative_annotator<-"CANNOT SAY"
        print(dfInitial)
      con<-prepareDBconnection()
      dbWriteTable(con, "manual_annotations", dfInitial, append = TRUE, row.names = FALSE, match.cols = TRUE)
      dbDisconnect(con)
      }
      dfInitial<<-NULL
      unlink(paste0(imagesLocationValidation,"*.jpg"))
      dfValid<<-getAndShowNewImage()
      print(dfValid)
    }
  })

    
    #############################################
  
  
  
  
  
  
  fetchNewFogDetection<-function(queueJson, handler){
    minReminder<-minute(Sys.time())%%10
   print(minReminder)
    
    if(firstOccurrence==TRUE){
      firstOccurrence<<-FALSE
      message("First Occurrence, queue is empty: using last retrieved values")
      df<-tryCatch({
        fromJSONtoDF(state_file)
      },
      error=function(cond){
        message("state file not available")
        output$timeString<-renderUI({HTML('<div class="centered">Last Updated:', as.character(as.POSIXlt(Sys.time(), "UTC")),"UTC  </div><br>")})
        df<-NULL
        return(df)
      })  
      visualizeResults(df)
    }
    
    
    ##CHANGE HERE FOR THE FIRST OCCURRENCE
    #if(minReminder==3 | minReminder==4 ){
   if(minReminder==3| minReminder==2) {
     
     #removing the pictures previously temporary stored
     unlink(paste0(imagesLocationDetection,"*.jpg"))
      
      
      req <- curl_fetch_memory(paste0("http://",jsonQueue$host,":8080/api/queues/%2f/RTvisual/get"), handle = h)
      
      text<-rawToChar(req$content)
      if(text!="[]"){

	      print("INSIDE ERROR EMPTY")
	     print(text)
        #message(text)
        writeLines(text,state_file)
        df<-fromJSONtoDF(text)
        #output$timeString<-renderUI({HTML('<div class="centered">Last Updated:', as.character(max(df$timeStamp)),"UTC  </div><br>")})
        
      }else{
        message("Queue is empty: using last retrieved values")
        df<-tryCatch({
          fromJSONtoDF(state_file)
        },
        error=function(cond){
          message("state file not available")
          output$timeString<-renderUI({HTML('<div class="centered">Last Updated:', as.character(as.POSIXlt(Sys.time(), "UTC")),"UTC  </div><br>")})
          df<-NULL
          return(df)
        })   
      }
      
      
      visualizeResults(df)
      

      
    }
 }
  #m  # Print the map
  
  
  
  #fetchNewFogDetection(jsonQueue,h)
  
  
  
  #reactivePoll(120000, session, checkFunc = fetchNewFogDetection )
 react_fetch_det<-reactivePoll(120000, session, checkFunc = fetchNewFogDetection)
 reactive(react_fetch_det())
  })


