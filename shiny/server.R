library(data.table)
library(randomForest)
library(caret)
shinyServer(function(input, output) {
  
  
  
  rfModel <- readRDS("rfModel.RDS")
  targetPicturesFeatures<-readRDS("targetPicturesFeatures.RDS")
  
  
  
  
  observeEvent(input$goButton, {
    
    selectedFile<-input$file
    
    selectedRow<-targetPicturesFeatures[filename==selectedFile$name]
    
    prova<-predict(rfModel,selectedRow)
    
    print(prova)
    
    
    
  })
  
  
  
  
  
  
  output$file <- renderTable(input$file)
  
  file <- reactive({
    file <- input$file
    file$datapath <- gsub("\\\\", "/", file$datapath)
    file
  })
  
  
  output$images <- renderUI({
    if(is.null(input$file)) return(NULL)
    image_output_list <- 
      lapply(1:nrow(file()),
             function(i)
             {
               imagename = paste0("image", i)
               imageOutput(imagename)
             })
    
    do.call(tagList, image_output_list)
  })
  
  observe({
    if(is.null(input$file)) return(NULL)
    for (i in 1:nrow(file()))
    {
      #print(i)
      local({
        my_i <- i
        imagename = paste0("image", my_i)
        #print(imagename)
        output[[imagename]] <- 
          renderImage({
            list(src = file()$datapath[my_i],
                 alt = "Image failed to render")
          }, deleteFile = FALSE)
      })
    }
  })
  
})