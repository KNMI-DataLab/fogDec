library(data.table)
library(leaflet)
library(unixtools)
library(plyr)
library(shiny)

message("test alive?")

shinyServer(function(input, output, session) {
#initialization (have to check what is needed after development, might not be needed)  
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
    h2o_jar_path = "/home/pagani/R/x86_64-redhat-linux-gnu-library/3.4/h2o/java/h2o.jar"
    devel_dir<-"/home/pagani/development/"
    results_json<-"/home/pagani/nndataH2O/frozenModels/results/predictions/test.json"
    temp_directory<-"/tmp/" #"/home/pagani/temp/Rtemp"
    queue_conf_file<-"/home/pagani/development/fogDec/inst/extScripts/python/queueConfig.json"
    cameras_for_detection_file<-"/home/pagani/development/fogDec/inst/extScripts/python/MVPCameras.json"
    df_debug_file<-"/home/pagani/development/fogDec/shiny/debug/debugDF.csv"
    shiny_dir<-"/home/pagani/development/fogDec/shiny"
    state_file<-"/home/pagani/development/fogDec/shiny/state/currentState.json"
  }
  
  
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
    jsoninput<-lapply(parsedJSON$payload,jsonlite::fromJSON)
    #reading from file to be replaced with reading from queue
    #jsoninput<-jsonlite::fromJSON(results_json)
    df <- data.frame(matrix(unlist(jsoninput), nrow=length(jsoninput), byrow=T), stringsAsFactors = F)
    colnames(df)<-names(jsoninput[[1]])
    df
  }
  
  
  
  
  
  fetchNewFogDetection<-function(queueJson, handler){
  
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
  
  
  if(is.null(df)==FALSE){
    message(max(df$timeStamp))
    output$timeString<-renderUI({HTML('<div class="centered">Last Updated:', as.character(max(df$timeStamp)),"UTC  </div><br>")})
    
  
  set.tempdir(temp_directory)
  
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
  
  df$fogClass<-as.factor(df$fogClass)
  #inputDF$graphicClass<if(inputDF$fogClass==FALSE){"noFog"}
  #inputDF$graphicClass<-as.factor(inputDF$graphicClass)
  #inputDF$fogClass<-revalue(inputDF$fogClass, c("FALSE"="nofog", "TRUE"="fog"))
  
  df$icon <- factor(df$fogClass,
                         levels = c("TRUE","FALSE"),
                         labels = c("red", "green")) 
  
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
                        markerColor = "grey")
  
  icons <- awesomeIcons(icon = "camera",
                        iconColor = "black",
                        library = "ion",
                        markerColor = df$icon
                        )
  
  

  df$hyperink<-paste0('<a href="',df$ipAddr,'" target="_blank">View Camera ', df$location," " ,df$cameraID,  '</a>')
  
  if(nrow(missing)!=0){
  missing$hyperink<-paste0('<a href="',missing$cameras.RWS.ipAddr,'">View Camera ',missing$cameras.RWS.location," " ,missing$cameras.RWS.cameraID,'</a>')
  m <- leaflet() %>%
    addTiles() %>%  # Add default OpenStreetMap map tiles
    addAwesomeMarkers(data=df, ~longitude, ~latitude, icon = icons, popup = ~hyperink) %>% addAwesomeMarkers(data=missing,~cameras.RWS.longitude, ~cameras.RWS.latitude, icon = iconsMissing, popup=~hyperink) %>% addControl(html= html_legend, position = "topright")
    #addCircleMarkers(data=df, ~longitude, ~latitude, popup = ~hyperink) #%>% addAwesomeMarkers(data=missing,~cameras.RWS.longitude, ~cameras.RWS.latitude, icon = iconsMissing, popup=~hyperink) %>% addControl(html= html_legend, position = "topright")
  
  }else{
    m <- leaflet() %>%
      addTiles() %>%  # Add default OpenStreetMap map tiles
      addAwesomeMarkers(data=df, ~longitude, ~latitude, icon = icons, popup = ~hyperink)  %>% addControl(html= html_legend, position = "topright")
      #addCircleMarkers(data=df, ~longitude, ~latitude,  popup = ~hyperink) #%>% addAwesomeMarkers(data=missing,~cameras.RWS.longitude, ~cameras.RWS.latitude, icon = iconsMissing, popup=~hyperink) %>% addControl(html= html_legend, position = "topright")
    
    }
  
  
  
  
  #output$timeString<-renderUI({HTML('<div class="centered">Last Updated:', as.character(as.POSIXlt(Sys.time(), "UTC")),"UTC  </div><br>")})
  
  #icon11<-myIcons[inputDF$graphicClass]

  output$map <-renderLeaflet(m)  
  }
  }
  #m  # Print the map
  

  
  #fetchNewFogDetection(jsonQueue,h)
  
  
  observeEvent(input$goButton, {
    
    selectedFile<-input$file
    
    selectedRow<-targetPicturesFeatures[filename==selectedFile$name]
    
    result<-predict(rfModel,selectedRow)
    
    if(result==FALSE){
      output$FOG <- renderUI({
        HTML("<strong>NO FOG</strong>")
      })
    }
    else {
      output$FOG <- renderUI({
        HTML("<strong>FOG</strong>")
      })
    }
    
    
    
  })
  
  
  #observe({ toggle(id="goButton", condition=!is.null(input$location))})
  
  
  
  reactivePoll(120000, session, checkFunc = fetchNewFogDetection )
  
  

  
  # observe({
  #   if(is.null(input$file)) return(NULL)
  #   for (i in 1:nrow(file()))
  #   {
  #     #print(i)
  #     local({
  #       my_i <- i
  #       imagename = paste0("image", my_i)
  #       print(file()$datapath[my_i])
  #       output[[imagename]] <- 
  #         renderImage({
  #           list(src = file()$datapath[my_i],
  #                alt = "Image failed to render")
  #         }, deleteFile = FALSE)
  #       print("AAAAAA")
  #     })
  #     #print(output)
  #   }
  # })
  
})

