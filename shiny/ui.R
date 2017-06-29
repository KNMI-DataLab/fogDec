shinyUI(fluidPage(
  titlePanel("Uploading Files"),
  sidebarLayout(
    sidebarPanel(
      fileInput(inputId = 'files', 
                label = 'Select an Image',
                multiple = TRUE,
                accept=c('image/png', 'image/jpeg')),
      actionButton("goButton", "Analyze the image")
      #look here fr enable/disable buttons
      #https://stackoverflow.com/questions/40621393/disabling-buttons-in-shiny
    ),
    mainPanel(
      tableOutput('files'),
      uiOutput('images')
    )
  )
))