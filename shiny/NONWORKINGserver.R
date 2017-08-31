library(data.table)
library(randomForest)
library(caret)
shinyServer(function(input, output) {
  
  
  rfModel <- readRDS("rfModel.RDS")
  targetPicturesFeatures<-readRDS("targetPicturesFeatures.RDS")
  #shinyjs::useShinyjs()
  #shinyjs::disable("goButton")
  
  output$visButton<-reactive({0
    })
  
  
  outputOptions(output, "visButton", suspendWhenHidden = FALSE)  
  
  
  
  
  #print(selectedFile)
  #print(test)
  

  
  observeEvent(input$goButton, {
    
    
    
    
    
    ################################
    #selectedFile<-input$file
    
    
    
    
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
  
  
  shinyFileChoose(input, 'file', roots=c(wd='.'), filetypes=c('', 'jpg'))

  
  output$file <- renderTable(input$file)
  
  
  observeEvent(input$file, {
    inFile <- parseFilePaths(roots=c(wd='.'), input$file)
    print("HELLOOOOOOO")
    print(inFile$datapath[[1]])
    
    
  
              
             output$images<-renderImage({
                src = inFile$datapath
                     alt = "Image failed to render"
                
                
                
                
                list(src = "ABC",
                     contentType = 'image/png',
                     width = 400,
                     height = 300,
                     alt = "This is alternate text")
                
                
                
               }, deleteFile = FALSE)
              
              print("AAAAAAAAAAAA")
              #print(out)
              #tagList(as.character(out))
           })
    
      
      
      
      #renderTable(inFile$datapath, envir=.GlobalEnv)
    

  
  # file <- reactive({
  #   file <- input$file
  #   file$datapath <- gsub("\\\\", "/", file$datapath)
  #   output$visButton <- reactive({1
  #   })
  #   file
  # })
  # 
  
  #output$images <- renderUI({
   # if(is.null(input$file)) return(NULL)
    #image_output_list <-
      #lapply(1:nrow(file()),
             #function()
             #{
               #imagename = paste0("image")
               #imageOutput(imagename)
            #}

    #do.call(tagList, image_output_list)
  #})
#   
#   observe({
#     if(is.null(input$file)) return(NULL)
#     for (i in 1:nrow(file()))
#     {
#       print(i)
#       local({
#         my_i <- i
#         imagename = paste0("image", my_i)
#         #print(imagename)
#         output[[imagename]] <- 
#           renderImage({
#             list(src = file()$datapath[my_i],
#                  alt = "Image failed to render")
#           }, deleteFile = FALSE)
#       })
#     }
#   })
#   
})