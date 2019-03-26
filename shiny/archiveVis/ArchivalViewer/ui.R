#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyTime)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  column(4, wellPanel(
    dateInput('date',
              label = 'Date input: yyyy-mm-dd',
              value = Sys.Date()
    ))),
  
  
  
  timeInput("time4", "Time:", value = strptime("12:34:56", "%T"), seconds = FALSE),

  
  column(6,
         verbatimTextOutput("dateText")
  ),
  
  actionButton("do", "Click Me"),
  
  tabsetPanel(
    tabPanel("Analysis",
             absolutePanel(id="controls",
                           style="z-index:200;",
                           class = "titlePane",
                           draggable = FALSE, 
                           htmlOutput("timeString")
                           #HTML('<div class="centered">',htmlOutput("timeString"),'</div><br>')
             ),
             tags$style(type = "text/css", "#map {height: calc(100vh - 80px) !important;}"),
             leafletOutput("map", width="100%", height="100%")
             
    )
    # tags$div(HTML("<img src='/home/pagani/development/fogDec/shiny/RTfogVisApp/test/Apptest/www/iconGreenNoBack.png' style='width:15px;height:20px;'>  NO FOG<br/>
    #              <img src='iconRedNoBack.png'style='width:15px;height:20px;'>  FOG<br/>
    #              <img src='iconGreyNoBack.png' style='width:15px;height:20px;'>  NA<br/>"))
    #fluidRow(column(12,uiOutput('images')
    #)),
  )#,
  
  
  
  
  # Application title
  # titlePanel("Old Faithful Geyser Data"),
  # 
  # # Sidebar with a slider input for number of bins 
  # sidebarLayout(
  #   sidebarPanel(
  #      sliderInput("bins",
  #                  "Number of bins:",
  #                  min = 1,
  #                  max = 50,
  #                  value = 30)
  #   ),
  #   
  #   # Show a plot of the generated distribution
  #   mainPanel(
  #      plotOutput("distPlot")
  #   )
  # )
))
