library(shinydashboard)
library(shinyFiles)
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
               
      )
               # tags$div(HTML("<img src='/home/pagani/development/fogDec/shiny/RTfogVisApp/test/Apptest/www/iconGreenNoBack.png' style='width:15px;height:20px;'>  NO FOG<br/>
               #              <img src='iconRedNoBack.png'style='width:15px;height:20px;'>  FOG<br/>
               #              <img src='iconGreyNoBack.png' style='width:15px;height:20px;'>  NA<br/>"))
               #fluidRow(column(12,uiOutput('images')
               #)),
      )#,
      #tags$style(type="text/css", "#images { display: flex; justify-content: center;}"),
      #tags$style(type='text/css', "#FOG { margin-top: 80px; text-align: center; font-size: 80px; font-family: Comic Sans MS, cursive, sans-serif;}"),
      
      
      
      # tabPanel("Approach description", 
      #          h4("The analysis to determine the presence of fog is done using a supervised machine learning approach. In such approach first a set of", 
      #             strong("features"), "are identified that are used to discriminate a foggy situation.", 
      #             p("In this case typical element of an image are used as features:"),
      #             HTML("<li>Mean Edges: for finding the boundaries of objects within images. It works by detecting discontinuities in the image (e.g., foreground and background elements).</li>
      #                  <li>Mean Brightness: perception of a source of radiating/reflecting light.</li>
      #                  <li>Mean Saturation: is a measure of the purity of the color. The purest (most saturated) color is achieved by using just one wavelength, less pure come from a combination at different wavelengths.</li>
      #                  <li>Mean HUE: perception of a source of being similar to one of the perceived colors: red, yellow, green, and blue, or to a combination of two of them.</li>
      #                  <li>Fractal Dimension: self similarity in filling space.</li>
      #                  <li>Transmission smoothness: transmission of the darkchannel of the image (smoothed indicator).</li>
      #                  <li>Transmission changepoint: horizontal point where the transmission of the dark channel is subject to change.</li>"),
      #             p(HTML("Once the features are extracted from the images, a Random Forest classifier is trained using as 
      #                    labels for the fog/no fog classification the observations of the meteorological optical range 
      #                    through a  forward scatterometer. <strong>The label fog is assigned when the visibility is less than 250m, 
      #                    whereas the label no-fog is assigned otherwise. </strong>")
      #             ),
      #             p("For this applicaiton the Random Forest method is applied to train the model on the data from 2016 of the De Bilt 
      #               camera (+23000 pictures). To test the model, the pictures provided in the analysis directory
      #               are a subset of the pictures of 2015.")
      #             )
      #          
      #          
      #             )
      )
  )
