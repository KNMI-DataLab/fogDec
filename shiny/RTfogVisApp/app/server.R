library(data.table)
library(leaflet)
library(unixtools)
library(plyr)
library(shiny)
library(logging)
library(mapview)
library(DBI)
library(jsonlite)


##########
#local and remote implementation variable naming/setting
##########


firstOccurrence = TRUE

  # temp_directory<-"/external/temp/"
  # df_debug_file<-"/external/debug/debugDF.csv"
  # log_file<-"/external/log/logFile.log"
  # state_file<-"/external/state/currentState.json"
  # cameras_for_detection_file<-"/external/config/MVPCameras.json"
  # queue_conf_file<-"/external/config/queueConfig.json"
  # DB_conf_file<-"/external/config/configDB.json"

  
  
  #local config
  temp_directory<-"/home/pagani/temp/"
  df_debug_file<-"/home/pagani/temp/debug/debugDF.csv"
  log_file<-"/home/pagani/temp/log/logFile.log"
  state_file<-"/home/pagani/temp/state/currentState.json"
  cameras_for_detection_file<-"/home/pagani/temp/config/MVPCameras.json"
  queue_conf_file<-"/home/pagani/temp/config/queueConfig.json"
  DB_conf_file<-"/home/pagani/temp/config/configDB.json"
  temp_directory<-"/home/pagani/temp/Rtemp"
  temp_directory<-"/tmp"
  


message("visualization platform ready")
logReset()
basicConfig(level='FINEST')
addHandler(writeToFile, file=log_file, level='DEBUG')
with(getLogger(), names(handlers))


set.tempdir(temp_directory)



# dbConfig <- fromJSON(DB_conf_file)
# 
# connectionSetup <- dbConnect(RPostgreSQL::PostgreSQL(),
#                 dbname = "FOGDB",
#                 host = dbConfig[["host"]], port = 9418, # use 9418 within KNMI, default would be 5432. At the moment set to 9418
# user = dbConfig[["user"]], password = dbConfig[["pw"]])
# 
# 
# queryDBforPics<-function(){
#   queryString<- "select * from images where random()<0.001 and day_phase in (1,0,20,10) and camera_id>260 limit 1;"
#   
#   tableLocations <- dbGetQuery(connectionSetup, queryString)
# }






#######Run prediciton on picture#########

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
    image<-blur_anisotropic(image, amplitude = 10000)
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


predictImage<-function(filename){
  
imagesLocation<-"/home/pagani/share/"
  
fileLocation<-gsub(".*/nas-research.knmi.nl/sensordata/CAMERA/", imagesLocation, filename)


partial<-str_extract(pattern = "[^/]*$", string =  filename)
partial<-str_extract(string = partial, pattern = ".*(?=\\.)")
timeStampTemp<-unlist(strsplit(partial, split = "_"))
date<-timeStampTemp[length(timeStampTemp)-1]
time<-timeStampTemp[length(timeStampTemp)]

Sys.setenv(TZ = "UTC")

timeStamp<-as.POSIXct(paste(date,time),format="%Y%m%d %H%M")
originalPath<-filename

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



#jsonCameras<-paste0(devel_dir,"fogDec/inst/extScripts/python/MVPCameras.json")

#camerasDF<-jsonlite::fromJSON(txt = jsonCameras)

#camerasRWS<-camerasDF$cameras$RWS


#pos<-gregexpr("-",locationAndID)
#lastDash<-pos[[1]][length(pos[[1]])]
#location<-substring(locationAndID,1,lastDash-1)
#cameraID<-substring(locationAndID,lastDash+1,str_length(locationAndID))

logdebug("looking if camera is in the allowed list")

#cameraTarget<-camerasRWS[camerasRWS$location==location & camerasRWS$cameraID==cameraID,]
#if(dim(cameraTarget)[1]==0){
#  stop("camera not in the list for fog detection")
#}
#cbind(cameraTarget,fileLocation,originalPath, timeStamp)

print(filename)

library(imager)
library(h2o)

featuresImage<-fromImageToFeatures(filename)
featuresImage<-t(featuresImage)




#logdebug(paste("starting prediction for", args))
prediction <- h2o.mojo_predict_df(featuresImage,
                                  mojo_zip_path =  model_zip_path, 
                                  classpath = h2o_jar_path, 
                                  genmodel_jar_path = h2o_jar_path)


#logdebug(paste("finished prediction for", args))


fogClass<-prediction$predict

predTRUE<-prediction$TRUE.
predFALSE<-prediction$FALSE.

return(list(fogClass,predTRUE,predFALSE))

}






#####################












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
  handle_setopt(h,USERNAME=jsonQueue$user, PASSWORD=jsonQueue$pw, POST=1, POSTFIELDS='{"count":10000,"requeue":false,"encoding":"auto"}' )
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
      
      
      
      df$localFileLocation<-gsub("pictures/","/external/pictures/",df$fileLocation)
      ######TO BE REMOVED####
      #df$localFileLocation<-gsub("pictures/","/home/pagani/share/",df$fileLocation)
      ###########
      
      print(df$localFileLocation)
      objs <- data.table(filenames=rownames(file.info(df$localFileLocation)),file.info(df$localFileLocation))
      print(objs)
      goodPics<-objs[objs$size > 10] #bigger than 10 bytes
      goodPics<-data.table(goodPics)
      setkey(goodPics,filenames)
      df<-data.table(df)
      setkey(df,localFileLocation)
      dfGoodPics<-df[goodPics,nomatch=0]
      print(dfGoodPics)
      popupFilenames<-as.vector(dfGoodPics$localFileLocation)
      
      
      
      
      
      
      dfGoodPics$hyperink<-paste0('<a href="',dfGoodPics$ipAddr,'" target="_blank">View Camera ', dfGoodPics$location," " ,dfGoodPics$cameraID,  '</a>')
      
      if(nrow(missing)!=0){
        missing$hyperink<-paste0('<a href="',missing$cameras.RWS.ipAddr,'">View Camera ',missing$cameras.RWS.location," " ,missing$cameras.RWS.cameraID,'</a>')
        m <- leaflet() %>%
          addTiles() %>%  # Add default OpenStreetMap map tiles
          addAwesomeMarkers(data=dfGoodPics, ~longitude, ~latitude, icon = icons, popup = popupImage(popupFilenames,src = "local", embed = T)) %>% addAwesomeMarkers(data=missing,~cameras.RWS.longitude, ~cameras.RWS.latitude, icon = iconsMissing, popup=~hyperink) %>% addControl(html= html_legend, position = "topright")
        #addCircleMarkers(data=dfGoodPics, ~longitude, ~latitude, popup = ~hyperink) #%>% addAwesomeMarkers(data=missing,~cameras.RWS.longitude, ~cameras.RWS.latitude, icon = iconsMissing, popup=~hyperink) %>% addControl(html= html_legend, position = "topright")
        
      }else{
        m <- leaflet() %>%
          addTiles() %>%  # Add default OpenStreetMap map tiles
          addAwesomeMarkers(data=dfGoodPics, ~longitude, ~latitude, icon = icons, popup =  popupImage(popupFilenames,src = "local", embed = T))  %>% addControl(html= html_legend, position = "topright")
        #addCircleMarkers(data=dfGoodPics, ~longitude, ~latitude,  popup = ~hyperink) #%>% addAwesomeMarkers(data=missing,~cameras.RWS.longitude, ~cameras.RWS.latitude, icon = iconsMissing, popup=~hyperink) %>% addControl(html= html_legend, position = "topright")
        
      }
      
      
      
      
      #output$timeString<-renderUI({HTML('<div class="centered">Last Updated:', as.character(as.POSIXlt(Sys.time(), "UTC")),"UTC  </div><br>")})
      
      #icon11<-myIcons[inputdfGoodPics$graphicClass]
      
      output$map <-renderLeaflet(m) 
    }
  }
  
  
  # #validation<-function(){
  #   output$images <- renderUI({
  #     #print("BBBBBB")
  #     #if(is.null(input$file)) return(NULL)
  #     #print
  #     renderImage(imagename)
  #   })
  # #}
  # 
    
    ##########################################
    
  imagename<-"/home/pagani/share/RWS/A1/HM43/ID13972/201907/A1-HM43-ID13972_20190715_1620.jpg"
  
  
  
  fogginess<-predictImage(imagename)
  if(fogginess[[1]]){
    fogChar<-"FOG"
  }else{
    fogChar<-"NO FOG"
  }
  
  output$FogBinary<-renderUI({HTML('Machine classification is:', fogChar)})
  output$probFog<-renderUI({HTML('Probability of fog in the  image:',as.character(fogginess[[2]]))})
  output$probNoFog<-renderUI({HTML('Probability of non-fog in the  image:',as.character(fogginess[[3]]))})
  
  print(fogginess)
  
  
      output$images <- renderImage({
       
        
        
        
        # Return a list containing the filename
        list(src = imagename,
             contentType = 'image/png',
             #width = 400,
             #height = 300,
             alt = "This is alternate text")
      }, deleteFile = TRUE)
    
  
    
    #############################################
  
  
  
  
  
  
  
  
  #output$images<- renderUI(HTML("<strong>FOG</strong>"))
  
  
  
  
  fetchNewFogDetection<-function(queueJson, handler){
    
    minReminder<-minute(Sys.time())%%10
    
    
    if(firstOccurrence==TRUE){
      firstOccurrence=FALSE
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
    if(minReminder==3 | minReminder==4 ){
      
      
      req <- curl_fetch_memory(paste0("http://",jsonQueue$host,":8080/api/queues/%2f/RTvisual/get"), handle = h)
      
      text<-rawToChar(req$content)
      if(text!="[]"){
        message(text)
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
      
      # visualizeResults<-function()
      # {
      # if(is.null(df)==FALSE){
      #   message(max(df$timeStamp))
      #   output$timeString<-renderUI({HTML('<div class="centered">Last Updated:', as.character(max(df$timeStamp)),"UTC  </div><br>")})
      #  
      #
      # set.tempdir(temp_directory)
      #
      # #setting icons to relevant colors and design
      # # factpal <- colorFactor(topo.colors(2), c(TRUE,FALSE))
      # # icon.bad <- makeAwesomeIcon( markerColor = 'red', icon = "camera")
      # # icon.good <- makeAwesomeIcon(markerColor = 'green', icon = "camera")
      # # icon.na<- makeAwesomeIcon(markerColor = 'gray', icon = "camera")
      # #
      # # myIcons<-awesomeIconList(noFog = icon.good, fog = icon.bad, notAv = icon.na)
      #
      #
      # #
      # #inputDF<-data.frame(jsoninput,stringsAsFactors = F)
      # #inputDF
      #
      # #inputDF<-inputDF[rep(1:nrow(inputDF),each=2),]
      # #inputDF[2,10]=TRUE
      # #inputDF[2,3]=5.1115
      # #inputDF
      #  
      #  
      #  
      #  
      #   jsonCameras<-jsonlite::fromJSON(cameras_for_detection_file)
      #   dfCameras<-data.frame(jsonCameras$cameras$RWS)
      #   dfCameras$longitude<-as.numeric(dfCameras$longitude)
      #   dfCameras$latitude<-as.numeric(dfCameras$latitude)
      #  
      #   numCamerasToMonitor<-dim(dfCameras)[[1]]
      #   numCamerasRetrieved<-dim(df)[[1]]
      #  
      #   if(numCamerasRetrieved!=numCamerasToMonitor){
      #     loginfo(paste(numCamerasRetrieved,"retrieved cameras, time of retrieval",as.character(max(df$timeStamp)),"UTC"))
      #     loginfo(paste("missing cameras:",dfCameras$location[!(dfCameras$cameraID %in% df$cameraID)]))
      #   }
      #  
      #  
      #  
      #
      # df$fogClass<-as.factor(df$fogClass)
      # #inputDF$graphicClass<if(inputDF$fogClass==FALSE){"noFog"}
      # #inputDF$graphicClass<-as.factor(inputDF$graphicClass)
      # #inputDF$fogClass<-revalue(inputDF$fogClass, c("FALSE"="nofog", "TRUE"="fog"))
      #
      # df$icon <- factor(df$fogClass,
      #                        levels = c("1","0","UNKNOWN"),
      #                        labels = c("red", "green","gray"))
      #
      # df
      # df$longitude<-as.numeric(df$longitude)
      # df$latitude<-as.numeric(df$latitude)
      #
      # message("dataframe created")
      # message(paste("saving dataframe for debug purposes in ",df_debug_file))
      # write.csv(df,file = df_debug_file )
      #
      # missing<-dfCameras [ !dfCameras$cameras.RWS.cameraID %in% df$cameraID ,]
      #
      # iconsMissing <- awesomeIcons(icon = "camera",
      #                       iconColor = "black",
      #                       library = "ion",
      #                       markerColor = "gray")
      #
      # icons <- awesomeIcons(icon = "camera",
      #                       iconColor = "black",
      #                       library = "ion",
      #                       markerColor = df$icon
      #                       )
      #
      #
      #
      # df$hyperink<-paste0('<a href="',df$ipAddr,'" target="_blank">View Camera ', df$location," " ,df$cameraID,  '</a>')
      #
      # if(nrow(missing)!=0){
      # missing$hyperink<-paste0('<a href="',missing$cameras.RWS.ipAddr,'">View Camera ',missing$cameras.RWS.location," " ,missing$cameras.RWS.cameraID,'</a>')
      # m <- leaflet() %>%
      #   addTiles() %>%  # Add default OpenStreetMap map tiles
      #   addAwesomeMarkers(data=df, ~longitude, ~latitude, icon = icons, popup = ~hyperink) %>% addAwesomeMarkers(data=missing,~cameras.RWS.longitude, ~cameras.RWS.latitude, icon = iconsMissing, popup=~hyperink) %>% addControl(html= html_legend, position = "topright")
      #   #addCircleMarkers(data=df, ~longitude, ~latitude, popup = ~hyperink) #%>% addAwesomeMarkers(data=missing,~cameras.RWS.longitude, ~cameras.RWS.latitude, icon = iconsMissing, popup=~hyperink) %>% addControl(html= html_legend, position = "topright")
      #
      # }else{
      #   m <- leaflet() %>%
      #     addTiles() %>%  # Add default OpenStreetMap map tiles
      #     addAwesomeMarkers(data=df, ~longitude, ~latitude, icon = icons, popup = ~hyperink)  %>% addControl(html= html_legend, position = "topright")
      #     #addCircleMarkers(data=df, ~longitude, ~latitude,  popup = ~hyperink) #%>% addAwesomeMarkers(data=missing,~cameras.RWS.longitude, ~cameras.RWS.latitude, icon = iconsMissing, popup=~hyperink) %>% addControl(html= html_legend, position = "topright")
      #  
      #   }
      #
      #
      #
      #
      # #output$timeString<-renderUI({HTML('<div class="centered">Last Updated:', as.character(as.POSIXlt(Sys.time(), "UTC")),"UTC  </div><br>")})
      #
      # #icon11<-myIcons[inputDF$graphicClass]
      #
      # output$map <-renderLeaflet(m) 
      # }
      # }
      #
      
    }
  }
  #m  # Print the map
  
  
  
  #fetchNewFogDetection(jsonQueue,h)
  
  
  
  #reactivePoll(120000, session, checkFunc = fetchNewFogDetection )
  
})


