# Load necessary libraries
library(shiny)
library(leaflet)
library(readr)
library(dplyr)
library(tidyr)  # for replace_na

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
  
  leafletOutput("map", height = 550) # adjust the map Size to take up the whole page
)

# Define server logic
server <- function(input, output) {
  gardenData <- reactive({
    file <- input$file1
    if (is.null(file)) {
      return(NULL)
    }
    
    # Load the garden data and filter out null latitude/longitude values
    df <- read_csv(file$datapath, col_names = input$header1) |>
      filter(!is.na(Longitude), !is.na(Latitude))
    
    # If 'Size' column exists, replace NA with median value
    if ('Size' %in% colnames(df)) {
      df$Size <- replace_na(df$Size, median(df$Size, na.rm = TRUE))
    }
    
    df
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
        addCircleMarkers(data = gardenData, lng = ~Longitude, lat = ~Latitude, popup = ~`Garden Name`, radius = ~Size * 5) 
    }
    
    # If grocery data is loaded, add it to the map
    # Need to fix grocery icons
    if (!is.null(groceryData)) {
      map <- map |> 
        addMarkers(data = groceryData, lng = ~Longitude, lat = ~Latitude, popup = ~`Entity Name`,
                   icon = ~leaflet::makeIcon(iconUrl = 'shop-icon.png', iconWidth = 10, iconHeight = 10)) 
    }
    
    map
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
