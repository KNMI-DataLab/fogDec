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
      tabPanel("Fog detection: current situation on the highway",
               absolutePanel(id="controls",
                             style="z-index:200;",
                             class = "titlePane",
                             draggable = FALSE, 
                             htmlOutput("timeString")
                             #HTML('<div class="centered">',htmlOutput("timeString"),'</div><br>')
               ),
               tags$style(type = "text/css", "#map {height: calc(100vh - 80px) !important;}"),
               leafletOutput("map", width="100%", height="100%")
               
      ),
      tabPanel("Fog detection: images validation",
               absolutePanel(id="controls",
                             style="z-index:200;",
                             class = "titlePane",
                             draggable = FALSE
                             #htmlOutput("timeString")
                             #HTML('<div class="centered">',htmlOutput("timeString"),'</div><br>')
               ),
               #tags$style(type = "text/css", "#map {height: calc(100vh - 80px) !important;}"),
               #leafletOutput("map", width="100%", height="100%")

               #tabPanel("Analysis",tableOutput('files'),
                        HTML('<br>',"<br>"),
                        imageOutput("images"),
                        #fluidRow(column(12,uiOutput('images')
                        #)),
               fluidRow(column(12,htmlOutput("FogBinary"))),
               fluidRow(column(12,htmlOutput("probFog"))),
               fluidRow(column(12,htmlOutput("probNoFog"))),
               #debug
               #fluidRow(column(12,htmlOutput("pictureTimestamp"))),
               ###

               HTML('<br>',"<br>"),
               HTML('<expert>Please provide your expert judgement on the picture too!<br>
                    Click on the button below to provide your input.</expert>'),
               HTML('<br>'),
               HTML('<nbmex>NB: consider FOG a visibility of 250m or less</nbmex>'),
               HTML('<br>'),
               actionButton("FOGbutton", "FOG"),
               actionButton("NOFOGbutton", "NO FOG"),
               actionButton("cannotButton", "Cannot say")
               ),
               tags$style(type="text/css", "#images { display: flex; justify-content: center; text-align:center}"),
               tags$style(type='text/css', "#FogBinary { margin-top: 10px; text-align: left; font-size: 30px; font-family: Comic Sans MS, cursive, sans-serif;}"),
               tags$style(type='text/css', "expert { margin-top: 10px; text-align: left; font-size: 15px; font-family: Comic Sans MS, cursive, sans-serif;}"),
               tags$style(type='text/css', "nbmex { margin-top: 10px; text-align: left; font-size: 20px; font-family: Comic Sans MS, cursive, sans-serif;}")
      
               
      
               
      )))
