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
library(shinydashboard)
library(leaflet)

# Define UI for application that draws a histogram
#shinyUI(fluidPage(
  
  
  
  

  
  dashboardPage(
    dashboardHeader(title = "Archival visualization of Fog Detection", titleWidth = 500),
    dashboardSidebar(width = 500,  h4("The goal of this app is to retrieve the archive of fog detection from camera pictures from 
                                    traffic surveillance cameras."),h4("Select a date and time in the past and click the button."),
                     h4("Please note that dayphases avavilable are: day, night, nautical and civil dawn only"),
                     wellPanel(
                       tags$style("#date{color: black;
                                 }"),
                       tags$style("#time4{color: black;
                                 }"),
                       dateInput('date',
                                 label = 'Date input: yyyy-mm-dd',
                                 value = Sys.Date()),
                       timeInput("time4", "Time:", value = strptime("12:00:00", "%T"), seconds = FALSE),
                       actionButton("do", "Retrieve Fog Detection"))
                     # h5("Select a file in the upload menu below and then click \"Analyze the image\" 
                     #    to have an automatic answer of the presence of fog." )
                     # fluidRow(
                     #res<-shinyFilesButton('file', label='File select', title='Please select a file', multiple=FALSE)
                     #),
                     
                     #enable this text once the image is selected
                     
    ),
    
    # wellPanel(
    #   dateInput('date',
    #             label = 'Date input: yyyy-mm-dd',
    #             value = Sys.Date()),
    #   timeInput("time4", "Time:", value = strptime("12:34:56", "%T"), seconds = FALSE)),
    
    dashboardBody(
      tags$style(HTML("
 .centered {
    position: relative;
    top: 50%;
    left: 50%;
  font-weight: bold;
 }
.bold{
  font-weight: bold;

}
                ")),
  
  
  
  
      # column(4, wellPanel(
      #   dateInput('date',
      #             label = 'Date input: yyyy-mm-dd',
      #             value = Sys.Date()),
      #   timeInput("time4", "Time:", value = strptime("12:34:56", "%T"), seconds = FALSE))
      # ),  
  
  

  
  # column(6,
  #        verbatimTextOutput("dateText")
  # ),
  # 
  
  
  tabsetPanel(
    tabPanel("Fog archive map",
             absolutePanel(id="controls",
                           style="z-index:200;",
                           class = "titlePane",
                           draggable = FALSE, 
                           htmlOutput("timeString")
                           #HTML('<div class="centered">',htmlOutput("timeString"),'</div><br>')
             ),
             tags$style(type = "text/css", "#map {height: calc(100vh - 80px) !important;}"),
             leafletOutput("map", width="100%", height="100%")
             #uiOutput("message")
             
             
    )
    # tags$div(HTML("<img src='/home/pagani/development/fogDec/shiny/RTfogVisApp/test/Apptest/www/iconGreenNoBack.png' style='width:15px;height:20px;'>  NO FOG<br/>
    #              <img src='iconRedNoBack.png'style='width:15px;height:20px;'>  FOG<br/>
    #              <img src='iconGreyNoBack.png' style='width:15px;height:20px;'>  NA<br/>"))
    #fluidRow(column(12,uiOutput('images')
    #)),
  )#,
)
)

