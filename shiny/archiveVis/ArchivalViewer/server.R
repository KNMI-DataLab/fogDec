#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyTime)

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
  
  
  
  observeEvent(input$do, {
    session$sendCustomMessage(type = 'testmessage',
                              message = 'Thank you for clicking')
    dateToFetch<-input$date
    timeToFetch<-input$time4
    timeForQuery1<-strftime(timeToFetch,format = "%H:%M:%S")
    timeForQuery2<-strftime(timeToFetch+600,format = "%H:%M:%S")
    
    dateForQuery<-strftime(dateToFetch,format = "%Y-%m-%d")
    
    #time2<-strsplit(timeToFetch," ")
    
    
    con<-mongo(collection = "collectionTestTIME", db = "fogDetectionArchive", url = "mongodb://145.23.219.231:27017")
    con$count()
    con$iterate()$one()
    
    d1 <- as.integer(as.POSIXct(strptime(paste0(dateForQuery,"T",timeForQuery1),"%Y-%m-%dT%H:%M:%S"))) * 1000
    d2 <- as.integer(as.POSIXct(strptime(paste0(dateForQuery,"T",timeForQuery2),"%Y-%m-%dT%H:%M:%S"))) * 1000
    
    data <- con$find(paste0('{"features.timeMongo":{"$gte": { "$date" : { "$numberLong" : "', d1, '" } }, "$lt": { "$date" : { "$numberLong" : "', d2, '" } }} }'))
    
 
    test<-data$features
    test22<-lapply(test,function(x){data.table(x$properties)})
    df<-rbindlist(test22)
    df<-data.table(df)
    
    
    
    df$fogClass<-as.factor(df$fogClass)
    #inputDF$graphicClass<if(inputDF$fogClass==FALSE){"noFog"}
    #inputDF$graphicClass<-as.factor(inputDF$graphicClass)
    #inputDF$fogClass<-revalue(inputDF$fogClass, c("FALSE"="nofog", "TRUE"="fog"))
    
    df$icon <- factor(df$fogClass,
                      levels = c("1","0","UNKNOWN"),
                      labels = c("red", "green","gray"))
    
    #df
    df$longitude<-as.numeric(df$longitude)
    df$latitude<-as.numeric(df$latitude)
    
    timeString<-paste0('<div class="centered">Situation at: ', as.character(max(df$timeStamp))," UTC  </div><br>")
    
    
    iconLegendPath<-"/home/pagani/development/fogDec/shiny/RTfogVisApp/test/Apptest/www/"
    
    html_legend <- HTML("<link href='shared/font-awesome/css/font-awesome.min.css'/><div class='bold'>
                        <img src='/home/pagani/development/fogDec/shiny/RTfogVisApp/test/Apptest/www/iconGreenNoBack.png' style='width:15px;height:20px;'>   NO FOG<br/>
                        <img src='/home/pagani/development/fogDec/shiny/RTfogVisApp/test/Apptest/www/iconRedNoBack.png'style='width:15px;height:20px;'>  FOG<br/>
                        <img src='/home/pagani/development/fogDec/shiny/RTfogVisApp/test/Apptest/www/iconGreyNoBack.png' style='width:15px;height:20px;'>  NA<br/></div>")
    
    
    iconsMissing <- awesomeIcons(icon = "camera",
                                 iconColor = "black",
                                 library = "ion",
                                 markerColor = "gray")
    
    icons <- awesomeIcons(icon = "camera",
                          iconColor = "black",
                          library = "ion",
                          markerColor = df$icon
    )
    
    
    
    df$hyperink<-paste0('<a href="',df$ipAddr,'" target="_blank">View Camera ', df$location," " ,df$cameraID,  '</a>')
    
    #if(nrow(missing)!=0){
    #missing$hyperink<-paste0('<a href="',missing$cameras.RWS.ipAddr,'">View Camera ',missing$cameras.RWS.location," " ,missing$cameras.RWS.cameraID,'</a>')
    m <- leaflet() %>%
      addTiles() %>%  # Add default OpenStreetMap map tiles
      addAwesomeMarkers(data=df, ~longitude, ~latitude, icon = icons, popup = ~hyperink) %>% addControl(html= html_legend, position = "topright")  %>% addControl(html= timeString, position = "bottomleft")
    #addCircleMarkers(data=df, ~longitude, ~latitude, popup = ~hyperink) #%>% addAwesomeMarkers(data=missing,~cameras.RWS.longitude, ~cameras.RWS.latitude, icon = iconsMissing, popup=~hyperink) %>% addControl(html= html_legend, position = "topright")
    
    output$map <-renderLeaflet(m) 
    
    
    
    
    
    
    

  })
  
  
  
  
  output$dateText  <- renderText({
    paste("input$date is", as.character(input$date))
  })
  
  
  # Print the time in [hh]:[mm] everytime it changes
  observe(print(strftime(input$time4, "%R")))
  
   
  output$distPlot <- renderPlot({
    
    # generate bins based on input$bins from ui.R
    x    <- faithful[, 2] 
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    
    # draw the histogram with the specified number of bins
    hist(x, breaks = bins, col = 'darkgray', border = 'white')
    
  })
  
})
