# Load necessary libraries
library(shiny)
library(leaflet)
library(readr)
library(dplyr)
library(tidyr)  # for replace_na
library(geosphere)

# Define UI
ui <- fluidPage(
  
  titlePanel("Garden and Grocery Locations"),
  
  sidebarLayout(
    sidebarPanel( 
      bottom = 10, right = 10,
      sliderInput("lat", "Latitude", 40.705, min = 40.49, max = 40.92),
      sliderInput("lng", "Longitude", -73.975, min = -74.27, max=-73.68),
      actionButton("calculate", "Calculate")
    ),
    mainPanel(
      leafletOutput("map"), # adjust the map Size to take up the whole page
      textOutput("grocery_dist"),
      textOutput("garden_dist"),
    )
  ),
  
  
  
  
)

# Define server logic
server <- function(input, output) {

    
    # Load the garden data and filter out null latitude/longitude values
    df <- read_csv("gardens_nyc.csv", col_names =TRUE) |>
      filter(!is.na(Longitude), !is.na(Latitude))
    
    # If 'Size' column exists, replace NA with median value
    if ('Size' %in% colnames(df)) {
      df$Size <- replace_na(df$Size, median(df$Size, na.rm = TRUE))
    }
    
    gardenData <- df

  
  groceryData <- read_csv("retail-food-stores-cleaned.csv", col_names=TRUE) |>
      filter(!is.na(Longitude), !is.na(Latitude))

  
  output$grocery_dist <- renderText({ "The closest grocery store is" }) # initialize the output
  output$garden_dist <- renderText({ "The closest garden is" }) # initialize the output
  
  observeEvent(input$calculate, {
    output$map <- renderLeaflet({
      gardenData <- gardenData
      groceryData <- groceryData
      
      # Create an empty map to start with
      map <- leaflet() |> addTiles()
      
      # If garden data is loaded, add it to the map
      if (!is.null(gardenData)) {
        map <- map |> 
          addCircleMarkers(data = gardenData, lng = ~Longitude, lat = ~Latitude, popup = ~`Garden Name`, radius = ~Size * 5) 
      }
      
      # If grocery data is loaded, add it to the map
      if (!is.null(groceryData)) {
        map <- map |> 
          addMarkers(data = groceryData, lng = ~Longitude, lat = ~Latitude, popup = ~`Entity Name`,
                     icon = ~leaflet::makeIcon(iconUrl = 'shop-icon.png', iconWidth = 20, iconHeight = 20)) 
      }
      
      # Add a FontAwesome user icon at the user-specified coordinates
      map <- map |> 
        addAwesomeMarkers(lng = input$lng, lat = input$lat, icon = awesomeIcons(icon = 'user', library = 'fa', markerColor = 'red'))
      
      # Set the map view to the user's input latitude and longitude
      map |> setView(lng = input$lng, lat = input$lat, zoom = 10)
      
      map
    })
    
    if (!is.null(groceryData)) {
      # Calculate the grocery_dists from the user-specified coordinates to all grocery stores
      grocery_dists <- distm(c(input$lng, input$lat), groceryData[, c("Longitude", "Latitude")], fun = distVincentySphere)
      
      # Find the minimum grocery_dist
      min_grocery_dist <- min(grocery_dists)
      
      # Convert the grocery_dist to kilometers
      min_grocery_dist_km <- min_grocery_dist / 1000
      
      # Update the output
      output$grocery_dist <- renderText({
        paste0("The closest grocery store is ", round(min_grocery_dist_km, 2), " kilometers away.")
      })
    }
    
    if (!is.null(gardenData)) {
      # Calculate the grocery_dists from the user-specified coordinates to all grocery stores
      garden_dists <- distm(c(input$lng, input$lat), gardenData[, c("Longitude", "Latitude")], fun = distVincentySphere)
      
      # Find the minimum grocery_dist
      min_garden_dist <- min(garden_dists)
      
      # Convert the grocery_dist to kilometers
      min_garden_dist_km <- min_garden_dist / 1000
      
      # Update the output
      output$garden_dist <- renderText({
        paste0("The closest garden is ", round(min_garden_dist_km, 2), " kilometers away.")
      })
    }
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
