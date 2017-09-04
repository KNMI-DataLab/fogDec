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
    selectedFile<-input$file$files$`0`[[2]]
    
    
    
    
    selectedRow<-targetPicturesFeatures[filename==selectedFile]
    
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
  
  
  shinyFileChoose(input, 'file', roots=c(wd='/home/pagani/development/fogDec/shiny/www/'), filetypes=c('', 'jpg'))

  
  output$file <- renderTable(input$file)
  
  
  
  file <- reactive({
    print("HELLOOOOO")
    file<-input$file
    inFile <- parseFilePaths(roots=c(wd='/home/pagani/development/fogDec/shiny/www/'), file)
    #file <- input$file
    print("HELLOOOOO")
    file$datapath <- inFile$datapath[[1]]
    output$visButton <- reactive({1
    })
    print(file)
    file
  })

  

  

  observe({
    if(is.null(input$file)) return(NULL)
    output$FOG <- renderUI({
      HTML("<strong> </strong>")})
    inFile <- parseFilePaths(roots=c(wd='/home/pagani/development/fogDec/shiny/www/'), input$file)
    print("hello world")
    print(file()$datapath[[1]])
      #print(i)
        #print(imagename)
    src <- tail(strsplit(as.character(file()$datapath[1]),"/")[[1]],1)
    
    picURL<-paste0('<img src="',src,'">')
    output$images<-renderText({picURL})
    
  })
  
})