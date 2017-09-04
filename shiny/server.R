library(data.table)
library(randomForest)
library(caret)
library(shinyFiles)
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
  
  
  shinyFileChoose(input, 'file', roots=c(wd='/home/pagani/development/fogDec/shiny/picturesForApp'), filetypes=c('', 'jpg'))

  
  output$file <- renderTable(input$file)
  
  
  # observeEvent(input$file, {
  #   inFile <- parseFilePaths(roots=c(wd='/home/pagani/development/fogDec/shiny/picturesForApp'), input$file)
  #   print("HELLOOOOOOO")
  #   print(inFile$datapath[[1]])
  #   
  #   
  # 
  #             
  #            output$images<-renderImage({
  #               #src = inFile$datapath[[1]]
  #               #alt = "Image failed to render"
  #               
  #               
  #               
  #               
  #               list(src = as.character(inFile$datapath[[1]]),
  #                    contentType = 'image/png',
  #                    width = 400,
  #                    height = 300,
  #                    alt = "This is alternate text")
  #               
  #               
  #               
  #              }, deleteFile = FALSE)
  #             
  #             print("AAAAAAAAAAAA")
  #             #print(out)
  #             #tagList(as.character(out))
  #          })
  #   
  #     
  #     
  #     
  #     #renderTable(inFile$datapath, envir=.GlobalEnv)
  #   

  
  file <- reactive({
    print("HELLOOOOO")
    file<-input$file
    inFile <- parseFilePaths(roots=c(wd='/home/pagani/development/fogDec/shiny/picturesForApp'), file)
    #file <- input$file
    print("HELLOOOOO")
    file$datapath <- inFile$datapath[[1]]
    output$visButton <- reactive({1
    })
    print(file)
    file
  })

  
  output$images <- renderUI({
    if(is.null(input$file)) return(NULL)
    print("GGGGG")
    file()
    image_output_list <- imageOutput("image1")
    print(image_output_list)
    
    do.call(tagList, image_output_list)
  })
  
  
  
  
  
  

  observe({
    if(is.null(input$file)) return(NULL)
    inFile <- parseFilePaths(roots=c(wd='/home/pagani/development/fogDec/shiny/picturesForApp'), input$file)
    src<-as.character(inFile$datapath[[1]])
    src<-"/home/pagani/development/fogDec/shiny/picturesForApp/Meetterrein_20151102_0740.jpg"
    picURL<-paste0('<img src="',src,'">')
    output$imageTest<-renderText({picURL})
    
  #   print("hello world")
  #   print(file()$datapath[[1]])
  #     #print(i)
  #       #print(imagename)
  #   for (i in 1:1)
  #   {
  #     my_i <- i
  #     imagename = paste0("image", my_i)
  #     print(file()$datapath[my_i])
  #   local({
  #       output[[imagename]] <- 
  #         renderImage({
  #           list(src = '/home/pagani/development/fogDec/shiny/picturesForApp/Meetterrein_20151102_0740.jpg')#,#file()$datapath[my_i],#file()$datapath[[1]],#as.character(inFile$datapth[[1]]),
  #                #alt = "Image failed to render")
  #         }, deleteFile = FALSE)
  #       #print(output[[image1]])
  #   
  # })
  #   }
  })
  
  
  output$file <- renderTable(input$file)
  
  
  
  
  
#   
})