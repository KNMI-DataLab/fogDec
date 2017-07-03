library(data.table)
library(randomForest)
library(caret)
shinyServer(function(input, output) {
  
  
  rfModel <- readRDS("rfModel.RDS")
  targetPicturesFeatures<-readRDS("targetPicturesFeatures.RDS")
  #shinyjs::useShinyjs()
  #shinyjs::disable("goButton")
  
  output$pippo<-reactive({0
    })
  
  
  outputOptions(output, "pippo", suspendWhenHidden = FALSE)  
  
  
  observeEvent(input$goButton, {
    
    selectedFile<-input$file
    
    selectedRow<-targetPicturesFeatures[filename==selectedFile$name]
    
    prova<-predict(rfModel,selectedRow)
    
    print(prova)
    
    
    
  })
  
  
  #observe({ toggle(id="goButton", condition=!is.null(input$location))})
  
  
  
  
  output$file <- renderTable(input$file)
  
  file <- reactive({
    file <- input$file
    file$datapath <- gsub("\\\\", "/", file$datapath)
    output$pippo <- reactive({1
    })
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