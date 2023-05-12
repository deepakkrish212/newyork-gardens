# Load necessary libraries
library(shiny)
library(leaflet)
library(readr)
library(dplyr)
library(tidyr)  # for replace_na
library(geosphere)
library(plotly)
library(shinydashboard)
library(DT)

# Define UI
ui <- dashboardPage(
  dashboardHeader(title = "Garden and Grocery Locations"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Map", tabName = "map", icon = icon("globe")),
      menuItem("Time Series", tabName = "timeseries", icon = icon("line-chart"))
    )
  ),
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = "map",
              fluidRow(
                column(3, 
                       sliderInput("lat", "Latitude", 40.705, min = 40.49, max = 40.92),
                       sliderInput("lng", "Longitude", -73.975, min = -74.27, max=-73.68),
                       actionButton("calculate", "Calculate"),
                       textOutput("grocery_dist"),
                       textOutput("garden_dist")
                ),
                column(9, 
                       leafletOutput("map"),
                       dataTableOutput("closest_gardens"),
                       dataTableOutput("closest_groceries"))
              )
      ),
      
      # Second tab content
      tabItem(tabName = "timeseries",
              fluidRow(
                fileInput("timeseries", "Choose CSV Files",
                          multiple = TRUE,
                          accept = c('text/csv', 
                                     'text/comma-separated-values,text/plain', 
                                     '.csv')),
                plotlyOutput("plots"),
                plotlyOutput("bar_plot")
              )
      )
    )
  )
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
  output$map <- renderLeaflet({
    leaflet() |> addTiles()
  })
  
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
      
      # Calculate the distances from the user-specified coordinates to each grocery store
      grocery_dists_km <- sapply(1:nrow(groceryData), function(i) {
        distVincentySphere(c(input$lng, input$lat), groceryData[i, c("Longitude", "Latitude")]) / 1000
      })
      
      # Add distances to the groceryData dataframe
      groceryData$Distance <- grocery_dists_km
      
      # Order groceryData by distance
      groceryData <- groceryData[order(groceryData$Distance), ]
      
      # Display the table
      output$closest_groceries <- renderDataTable({
        datatable(groceryData[, c("Entity Name", "Distance")], options = list(pageLength = 5))
      })
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
      
      garden_dists_km <- sapply(1:nrow(gardenData), function(i) {
        distVincentySphere(c(input$lng, input$lat), gardenData[i, c("Longitude", "Latitude")]) / 1000
      })
      
      # Add distances to the gardenData dataframe
      gardenData$Distance <- garden_dists_km
      
      # Order gardenData by distance
      gardenData <- gardenData[order(gardenData$Distance), ]
      
      # Display the table
      output$closest_gardens <- renderDataTable({
        datatable(gardenData[, c("Garden Name", "Distance")], options = list(pageLength = 5))
      })
      
      
      # Update the output
      output$garden_dist <- renderText({
        paste0("The closest garden is ", round(min_garden_dist_km, 2), " kilometers away.")
      })
    }
  })
  
  pricesData <- reactive({
    files <- input$timeseries
    if (is.null(files)) {
      return(NULL)
    }
    
    lapply(files$datapath, function(file) {
      df <- read_csv(file, col_names = TRUE, col_types = cols(
        Date = col_date(format = "%d-%m-%Y"),
        `Price min` = col_double(),
        `Price max` = col_double()
      ))
      
      df$Name <- tools::file_path_sans_ext(basename(file)) # add a column with the name of the file
      
      df
    }) |> bind_rows()  # combine all data frames into one
  })
  
  output$plots <- renderPlotly({
    req(input$timeseries)
    
    plots <- lapply(input$timeseries$datapath, function(file) {
      # Read the CSV file into a data frame
      df <- read_csv(file, col_names = TRUE, col_types = cols(.default = "c"))
      
      # Convert the 'Date' column to a date object
      df$Date <- as.Date(df$Date, format = "%d-%m-%Y")
      
      # Create a time series plot
      plot_ly(df, x = ~Date, y = ~`Price min`, type = 'scatter', mode = 'lines', name = 'Price min') |>
        add_trace(y = ~`Price max`, name = 'Price max', mode = 'lines') |>
        layout(title = tools::file_path_sans_ext(basename(file)), 
               xaxis = list(title = "Date"), 
               yaxis = list(title = "Price"))
    })
    
    subplot(plots, nrows = length(plots), margin = 0.05)
  })
  
  percentageChangeData <- reactive({
    df <- pricesData()
    if (is.null(df)) {
      return(NULL)
    }
    
    df |>
      group_by(Name) |>
      summarise(PercentageChange = 100 * (first(`Price max`) - last(`Price max`)) / first(`Price max`))  # calculate percentage change
  })
  
  output$bar_plot <- renderPlotly({
    df <- percentageChangeData()
    if (is.null(df)) {
      return(NULL)
    }
    
    plot_ly(df, x = ~Name, y = ~PercentageChange, type = 'bar') |>
      layout(title = "Percentage Change in Price")
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

                              