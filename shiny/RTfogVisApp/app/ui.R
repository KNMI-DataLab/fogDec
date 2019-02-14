library(shinydashboard)
#library(shinyFiles)
library(leaflet)





dashboardPage(
  dashboardHeader(title = "Real-Time Fog Detection", titleWidth = 500),
  dashboardSidebar(width = 500,  h4("The goal of this app is to detect fog from camera pictures from 
                                    traffic surveillance cameras.")
                   # h5("Select a file in the upload menu below and then click \"Analyze the image\" 
                   #    to have an automatic answer of the presence of fog." )
                   # fluidRow(
                   #res<-shinyFilesButton('file', label='File select', title='Please select a file', multiple=FALSE)
                   #),
                   
                   #enable this text once the image is selected
                   
                   ),

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
    # Boxes need to be put in a row (or column)
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
               
      ))))
