# Load necessary libraries
library(shiny)
library(leaflet)
library(readr)
library(dplyr)

# Define UI
ui <- fluidPage(
  titlePanel("Garden Locations"),
  
  absolutePanel(bottom = 10, right = 10, # place the panel in the top right corner
                fileInput('file', 'Choose CSV File',
                          accept=c('.csv')),
                tags$hr(),
                checkboxInput('header', 'Header', TRUE),
                fixed = FALSE
  ),
  
  leafletOutput("map", height = 550) # adjust the map size to take up the whole page
)

# Define server logic
server <- function(input, output) {
  data <- reactive({
    file <- input$file
    if (is.null(file)) {
      return(NULL)
    }
    
    # Load the data and filter out null latitude/longitude values
    read_csv(file$datapath, col_names = input$header) |>
      filter(!is.na(Longitude), !is.na(Latitude))
  })
  
  output$map <- renderLeaflet({
    data <- data()
    if (is.null(data)) {
      return(NULL)
    }
    
    leaflet(data) |>
      addTiles() |>
      addMarkers(lng = ~Longitude, lat = ~Latitude, popup = ~`Garden Name`)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)