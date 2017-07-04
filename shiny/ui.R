# shinyUI(fluidPage(
#   titlePanel(fluidRow(column(12,"Fog Detection from Camera Images"))),
#   sidebarLayout(
#     sidebarPanel(
#       fileInput(inputId = 'file', 
#                 label = 'Select an Image',
#                 multiple = TRUE,
#                 accept=c('image/png', 'image/jpeg')),
#      h5(strong("Analyze the image selected")),
#       actionButton("goButton", "Analyze the image")
#       #look here fr enable/disable buttons
#       #https://stackoverflow.com/questions/40621393/disabling-buttons-in-shiny
#     ),
#     mainPanel(
#       h4("The app works as follows..."),
#       tableOutput('files'),
#       uiOutput('images')
#     )
#   )   
# ))


library(shinydashboard)


dashboardPage(
  
  dashboardHeader(title = "Fog Detection from Camera Images", titleWidth = 500),
  dashboardSidebar(width = 500,  h4("The goal of this app is to illustrate 
                                    the detection of fog conditions from camera images DataLab project."),
                   h5("Select a file in the upload menu below and then click \"Analyze the image\" 
                      to have an automatic answer of the presence of fog." ),
    fluidRow(
     #shinyjs::useShinyjs(),
      #sess <- getSession(),
      #id <- sess$ns(id),
      #shinyjs::toggleState('goButton'),
      
      #shinyjs::disable("goButton"),
     
    res<-fileInput(inputId = 'file', 
              label = 'Select an Image',
              multiple = TRUE,
              accept=c('image/png', 'image/jpeg'))
    #if(){
              
    #}
    #look here fr enable/disable buttons
    #https://stackoverflow.com/questions/40621393/disabling-buttons-in-shiny
  ),
  
  
  
  #print(output$file),
  conditionalPanel(condition='output.pippo!=0',
  h5(strong("Analyze the image selected")),
  actionButton("goButton", "Analyze the image")),
  
  
  
  #enable this text once the image is selected
  h6("The fog condion is considered when the visibility is less than 250m according to the reference measurement of the MOR 
     horizontal scatterometer.")
  ),
  dashboardBody(
    # Boxes need to be put in a row (or column)
    
    
           tableOutput('files'),
           uiOutput('images')
    )
  )
#)






# fluidPage(
#   
#   title = "Diamonds Explorer",
#   
#   plotOutput('plot'),
#   
#   hr(),
#   
#   fluidRow(
#     column(3,
#            h4("Diamonds Explorer"),
#            sliderInput('sampleSize', 'Sample Size', 
#                        min=1, max=nrow(dataset), value=min(1000, nrow(dataset)), 
#                        step=500, round=0),
#            br(),
#            checkboxInput('jitter', 'Jitter'),
#            checkboxInput('smooth', 'Smooth')
#     ),
#     column(4, offset = 1,
#            selectInput('x', 'X', names(dataset)),
#            selectInput('y', 'Y', names(dataset), names(dataset)[[2]]),
#            selectInput('color', 'Color', c('None', names(dataset)))
#     ),
#     column(4,
#            selectInput('facet_row', 'Facet Row', c(None='.', names(dataset))),
#            selectInput('facet_col', 'Facet Column', c(None='.', names(dataset)))
#     )
#   )
# )