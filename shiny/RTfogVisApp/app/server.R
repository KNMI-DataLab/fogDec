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
library(aws.s3)

#library(h2o)
##########
#local and remote implementation variable naming/setting
##########


firstOccurrence<<-TRUE

  #temp_directory<-"/external/temp/"
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
  foggyDataLocation<-"/external/data/foggyImagesDetected.RDS"
  promisingFoggyDaysLocation<-"/external/data/promisingFoggyDays.RDS"
  scriptDayCivilDD<-"/visApp/fogDec/shiny/RTfogVisApp/supportScripts/predictLabelDay.py"
  scriptNightAstroNautDD<-"/visApp/fogDec/shiny/RTfogVisApp/supportScripts/predictLabelNight.py"
  

  
  
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


#set.tempdir(temp_directory)


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

queryString<- "select * from non_evaluated_images_rws where random()<0.08 and day_phase in (1,0,20,10) and camera_id>260 limit 1;"



imageToValidate <- dbGetQuery(connectionSetup, queryString)

dbDisconnect(connectionSetup)

print("QUERY EXECUTED")

print(imageToValidate)

imageToValidate
}


dataFoggy<<-readRDS(foggyDataLocation)

promisingFoggyDays<<-readRDS(promisingFoggyDaysLocation)

sampleFoggyCases<-function(dtFoggy){
  sampledCase<-dtFoggy[sample(.N, 1)]
  sampledCase
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



##########Dealing with feedback textbox##########

#observeEvent(input$submitMessageButton, {
#  cat("Showing", input$item)
#})

#########################



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


if (dayPhaseImage %in% c(1,10,11)){
selectedScript<-scriptDayCivilDD
} else{
  selectedScript<-scriptNightAstroNautDD
}
  


system(paste0('python3 ',selectedScript,' ',filename), wait = T)

prediction<-jsonlite::fromJSON("/tmp/predicitonLabel.json")

fogClass<-prediction$fogClass

predTRUE<-prediction$predTrue
predFALSE<-prediction$predFalse

modelPath<-prediction$model_id

pippo<-list(fogClass,predTRUE,predFALSE,modelPath)

return(list(fogClass,predTRUE,predFALSE,modelPath))

}


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
  #print(df)
  df
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
                         <img src='iconBlackNoBack.png'style='width:15px;height:20px;'>  NO VIDEO<br/>
                        <img src='iconGreyNoBack.png' style='width:15px;height:20px;'>  NA<br/></div>")
  dfCameras$hyperink<-paste0('<a href="',dfCameras$ipAddr,'" target="_blank">View Camera ', dfCameras$location," " ,dfCameras$cameraID,  '</a>')
  mapInit<-leaflet(dfCameras) %>% addTiles() %>%  addAwesomeMarkers( ~longitude, ~latitude, icon = iconsInit, popup = ~hyperink ) %>% addControl(html= html_legend, position = "topright")
  output$map<-renderLeaflet(mapInit)
  #jsonQueue<-jsonlite::fromJSON(queue_conf_file)
  #added for debug##############
  firstOccurrence<<-TRUE
  #############################
  
  visualizeResults<-function(df)
  {
    if(is.null(df)==FALSE){
      message(max(df$timeStamp))
      output$timeString<-renderUI({HTML('<div class="centered">Last Updated:', as.character(max(df$timeStamp)),"UTC  </div><br>")})
      
    
      
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
      
      
      df$icon <- factor(df$fogClass,
                        levels = c("1","0","UNKNOWN", "3"),
                        labels = c("red", "green","gray","black"))
      
      df
      df$longitude<-as.numeric(df$longitude)
      df$latitude<-as.numeric(df$latitude)
   
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
      
      
     
      df$localFileLocation<-gsub("pictures/",imagesLocationDetection,df$fileLocation)
      df$filename<-basename(df$localFileLocation)
      
      
      localImageFilepath<-convertToLocalFilepath(df$fileLocation)
 
      localTempSavedLocation <- paste0(imagesLocationDetection,df$filename)


      saveMultipleObjects<-function(AWSPath){
      localTempSavedLocation <-paste0(imagesLocationDetection,basename(AWSPath))
      save_object(object = AWSPath, bucket = 'knmi-fogdetection-dataset',
                  file = localTempSavedLocation)
    #  print(paste("file location",localTempSavedLocation))
    #  print("object saved real time detection")
	}
      

      lapply(localImageFilepath, saveMultipleObjects)	
      
      

      
      
      df$localFileLocation<-localTempSavedLocation
      ###########
      
      #print(df$localFileLocation)
      objs <- data.table(filenames=rownames(file.info(df$localFileLocation)),file.info(df$localFileLocation))
      #print(objs)
      
      #####TRY TO IDENTIFY THE CAUSE OF THE MARKER SWITCH IN COLOR########
      # goodPics<-objs[objs$size > 10] #bigger than 10 bytes
      # goodPics<-data.table(goodPics)
      # setkey(goodPics,filenames)
      # df<-data.table(df)
      # setkey(df,localFileLocation)
      # dfGoodPics<-df[goodPics,nomatch=0]
      # #print(dfGoodPics)
      # popupFilenames<-as.vector(dfGoodPics$localFileLocation)
      
      dfGoodPics<-df
      popupFilenames<-as.vector(dfGoodPics$localFileLocation)
      
      #####TRY TO IDENTIFY THE CAUSE OF THE MARKER SWITCH IN COLOR########
      
      
      #print(popupFilenames)
      
      
      dfGoodPics$hyperink<-paste0('<a href="',dfGoodPics$ipAddr,'" target="_blank">View Camera ', dfGoodPics$location," " ,dfGoodPics$cameraID,  '</a>')
      
      #message('##############before  output maps#######################')
      
      #print(dfGoodPics)
      
      
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
      
     
      output$map <-renderLeaflet(m) 
    }
  }
  
    
    ###################MANAGING OF THE VALIDATION PART#######################
    

  getAndShowNewImage<-function(){

  #random sample from the metadataDB and from the archive of foggy detected images
  #15% of the times a detected foggy image should be called
  randNum<-sample(1:100,1)
  # print("--------------------")
  # print(randNum)
  # print("--------------------")
  if(randNum<=10){
  #fogArchiveRecord<-queryMongoDetectionArchive()
  fogArchiveRecord<-sampleFoggyCases(dataFoggy)
  imagename<-fogArchiveRecord$originalPath
  camera_id<-query_camera_id(fogArchiveRecord$cameraID)

  #camera_id<-fogArchiveRecord$cameraID
  timestamp<-fogArchiveRecord$timeStampMongoFormat
  image_id<-NA
  if(fogArchiveRecord$fogClass==1){
	    fogChar<-"FOG"
   }else{
	    fogChar<-"NO FOG"
	  }
  visibility_qualitative_detection_model<-fogChar
  detection_model_name<-NA
  probFog<-fogArchiveRecord$predTRUE
  probNoFog<-fogArchiveRecord$predFALSE


  }else if(randNum<=12){
    potentialFoggyRecord<-sampleFoggyCases(promisingFoggyDays)
    
    ########
    imagename<-potentialFoggyRecord$filepath
    
    camera_id<-potentialFoggyRecord$camera_id
    dayPhaseImage<-potentialFoggyRecord$day_phase
    #camera_id<-fogArchiveRecord$cameraID
    timestamp<-potentialFoggyRecord$timestamp
    image_id<-potentialFoggyRecord$image_id
    ###########
    
  }else{ 
  #print("I AM IN >45")  
  imageDBrecord<-queryDBforImage()
  imagename<-imageDBrecord$filepath
  dayPhaseImage<-imageDBrecord$day_phase
  image_id<-imageDBrecord$image_id
  camera_id<-imageDBrecord$camera_id
  timestamp<-imageDBrecord$timestamp
  }
  
  localImageFilepath<-convertToLocalFilepath(imagename)
  filenameImage<-basename(localImageFilepath)
  localTempSavedLocation <- paste0(imagesLocationValidation,filenameImage) 
  head_obj<-head_object(object = localImageFilepath, bucket = 'knmi-fogdetection-dataset')
  
  ##removing NoVideo and NoStream from the images shown to annotators
  #obj_size<-object_size(object = localImageFilepath, bucket = 'knmi-fogdetection-dataset')
  #print("object size:")
  object_size<-as.numeric(attr(head_obj,'content-length'))
  ##
  
  if(head_obj==TRUE & object_size>=9000){
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
  print(localTempSavedLocation)
  fogginess<-predictImage(localTempSavedLocation, dayPhaseImage)
  if(fogginess[[1]]==1){
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
  
  #debug
  output$pictureTimestamp<-renderUI({HTML('Picture take at:',as.character(timestamp))})
  
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

  print("HERE")
  dfInitial<<-getAndShowNewImage()
  print("DFINITIAL")
  print(dfInitial)
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

    

  fetchNewFogDetection<-function(){
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
    if(minReminder==4) {
      
      #removing the pictures previously temporary stored
      unlink(paste0(imagesLocationDetection,"*.jpg"))
      
      
      df<-fromJSONtoDF(state_file)
      
      #print(df)
      print("calling visualize results")
      
      visualizeResults(df)
      
    }
  }
  
  # autoInvalidate <- reactiveTimer(10000)
  # observe({
  #   autoInvalidate()
  #   cat(".")
  # })
  
  
 react_fetch_det<-reactivePoll(60000, session, checkFunc = fetchNewFogDetection)
 reactive(react_fetch_det())
  })


