library(data.table)
library(leaflet)
library(unixtools)
library(plyr)
library(shiny)
shinyServer(function(input, output) {
  
  
  
  
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
    temp_directory<-"/home/pagani/temp/Rtemp/"
    
  }
  
  
  ###FIRST ATTEMPT OF VISUALIZATION
  
  jsoninput<-jsonlite::fromJSON(results_json)
  
  set.tempdir(temp_directory)
  
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
  #m  # Print the map
  
  
  
  
  
  
  
  
  
  
  
  
  #shinyjs::useShinyjs()
  #shinyjs::disable("goButton")
  
  
  
  
  
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
  
  
  
  
 
  
  output$map <- renderUI({
    print("BBBBBB")
    
    print("CCCCCC")
    m
  })
  
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

