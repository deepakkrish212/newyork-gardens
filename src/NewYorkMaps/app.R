# Load necessary libraries
library(shiny)
library(leaflet)
library(readr)
library(dplyr)

# Define UI
ui <- fluidPage(
  titlePanel("Garden and Grocery Locations"),
  
  absolutePanel(bottom = 10, right = 10, # place the panel in the top right corner
                fileInput('file1', 'Choose Garden CSV File',
                          accept=c('.csv')),
                tags$hr(),
                checkboxInput('header1', 'Header', TRUE)
  ),
  
  absolutePanel(bottom = 10, left = 10, # place the panel a bit lower
                fileInput('file2', 'Choose Grocery CSV File',
                          accept=c('.csv')),
                tags$hr(),
                checkboxInput('header2', 'Header', TRUE)
  ),
  
  leafletOutput("map", height = 550) # adjust the map size to take up the whole page
)

# Define server logic
server <- function(input, output) {
  gardenData <- reactive({
    file <- input$file1
    if (is.null(file)) {
      return(NULL)
    }
    
    # Load the garden data and filter out null latitude/longitude values
    read_csv(file$datapath, col_names = input$header1) |>
      filter(!is.na(Longitude), !is.na(Latitude))
  })
  
  groceryData <- reactive({
    file <- input$file2
    if (is.null(file)) {
      return(NULL)
    }
    
    # Load the grocery data and filter out null latitude/longitude values
    read_csv(file$datapath, col_names = input$header2) |>
      filter(!is.na(Longitude), !is.na(Latitude))
  })
  
  output$map <- renderLeaflet({
    gardenData <- gardenData()
    groceryData <- groceryData()
    
    # Create an empty map to start with
    map <- leaflet() |> addTiles()
    
    # If garden data is loaded, add it to the map
    if (!is.null(gardenData)) {
      map <- map |> 
        addMarkers(data = gardenData, lng = ~Longitude, lat = ~Latitude, 
                   icon = ~leaflet::makeIcon(iconUrl = 'https://icon-library.com/images/tree-icon-png/tree-icon-png-3.jpg', 
                                             iconWidth = 25, iconHeight = 25)) 
    }
    
    # If grocery data is loaded, add it to the map
    if (!is.null(groceryData)) {
      map <- map |> 
        addMarkers(data = groceryData, lng = ~Longitude, lat = ~Latitude, 
                   icon = ~leaflet::makeIcon(iconUrl = 'https://www.iconfinder.com/icons/3018711/ecommerce_platform_shopify_applications_online_retail_stores_icon', iconWidth = 25, iconHeight = 25)) 
    }
    
    map
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
