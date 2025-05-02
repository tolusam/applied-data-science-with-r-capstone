# Install and import required libraries
require(shiny)
require(ggplot2)
require(leaflet)
require(tidyverse)
require(httr)
require(scales)

# Import model_prediction R which contains methods to call OpenWeather API
# and make predictions
source("model_prediction.R")

# Function to test weather data generation
test_weather_data_generation <- function() {
  city_weather_bike_df <- generate_city_weather_bike_data()
  stopifnot(length(city_weather_bike_df) > 0)
  print(head(city_weather_bike_df))
  return(city_weather_bike_df)
}

# Create a Shiny server
shinyServer(function(input, output, session) {
  # Define a city list
  city_list <- c("All", "Seoul", "New York", "Paris", "Suzhou", "London")
  # Define color factor
  color_levels <- colorFactor(c("green", "yellow", "red"),
                              levels = c("small", "medium", "large"))
  
  # Test generate_city_weather_bike_data() function
  city_weather_bike_df <- test_weather_data_generation()
  
  # Calculate the minimum and maximum FORECASTDATETIME
  min_date <- min(city_weather_bike_df$FORECASTDATETIME)
  max_date <- max(city_weather_bike_df$FORECASTDATETIME)
  
  # Update the app title with the minimum and maximum dates
  output$app_title <- renderText({
    title <- paste("Bike sharing demand forecast from", 
                   substr(min_date, 1, 10), "to", substr(max_date, 1, 10))
  })
  
  # Create another data frame called `cities_max_bike` with each row containing city location info and max bike
  # prediction for the city
  cities_max_bike <- city_weather_bike_df %>%
    group_by(CITY_ASCII) %>%
    summarize(
      LNG = first(LNG),
      LAT = first(LAT),
      max_bike = max(BIKE_PREDICTION),
      BIKE_PREDICTION_LEVEL = first(BIKE_PREDICTION_LEVEL),
      LABEL = first(LABEL)
    )
  
  # Observe drop-down event
  observeEvent(input$city, {
    selected_city <- input$city
    if (selected_city == "All") {
      # Render a leaflet map with circle markers and popup weather LABEL for all cities
      output$city_bike_map <- renderLeaflet({
        leaflet() %>%
          addTiles() %>%
          addCircleMarkers(
            data = cities_max_bike,
            lng = ~LNG,
            lat = ~LAT,
            color = ~color_levels(BIKE_PREDICTION_LEVEL),
            radius = ~ifelse(BIKE_PREDICTION_LEVEL == "small", 6,
                             ifelse(BIKE_PREDICTION_LEVEL == "medium", 10, 12)),
            popup = ~LABEL
          ) %>%
          addPopups(data=cities_max_bike, lng = ~LNG, lat = ~LAT, popup = ~LABEL, 
                    options = popupOptions(closeButton=FALSE))
      })

    } else {
      # Render a leaflet map with one marker on the map and a popup with DETAILED_LABEL displayed
      choice_city_weather_bike_df <- city_weather_bike_df[city_weather_bike_df$CITY_ASCII == selected_city, ]
      choice_city_weather_bike_df$FDATETIME <- as_datetime(choice_city_weather_bike_df$FORECASTDATETIME)
      choice_city_weather_bike_df <- choice_city_weather_bike_df %>% mutate(index = row_number())
      output$city_bike_map <- renderLeaflet({
        leaflet() %>%
          addTiles() %>%
          addMarkers(
            data = choice_city_weather_bike_df,
            lng = ~LNG,
            lat = ~LAT,
            popup = ~DETAILED_LABEL
          ) %>%
          addPopups(data=choice_city_weather_bike_df[1,], lng = ~LNG, lat = ~LAT,
                    popup = ~DETAILED_LABEL, options = popupOptions(closeButton=FALSE))
      })
      
      # Render a plot with bike prediction over time for the selected city
      output$bike_line <- renderPlot({
        ggplot(choice_city_weather_bike_df, aes(x = FDATETIME, y = BIKE_PREDICTION, group = 1)) +
          geom_point() + #color = "purple") +
          geom_line(color = "green") +
          geom_text(aes(label = BIKE_PREDICTION), vjust = -1, hjust = 1, size = 3) +
          labs(title = paste("Bike Count Demand Prediction for", selected_city),
               x = "Time (3 hours ahead)", y = "Predicted Bike Count") +
          theme_minimal() +
          scale_x_datetime(date_labels = "%Y-%m-%d")
      })

      # Render a plot with temperature over time for the selected city
      output$temp_line <- renderPlot({
        ggplot(choice_city_weather_bike_df, aes(x = index, y = TEMPERATURE, group = 1)) +
          geom_point() + #color = "black") +
          geom_line(color = "yellow") +
          geom_text(aes(label = TEMPERATURE), vjust = -1, hjust = 1, size = 3) +
          labs(title = paste("Temperature Trend for", selected_city),
               x = "Time (3 hours ahead)",
               y = "Temperature (°C)") +
          theme_minimal() #+
          # scale_x_datetime(date_labels = "%Y-%m-%d")
      })
        
      # Render the humidity and bike-sharing demand prediction correlation plot
      output$humidity_pred_chart <- renderPlot({
        ggplot(choice_city_weather_bike_df, aes(x = HUMIDITY, y = BIKE_PREDICTION)) +
          geom_point() +
          geom_smooth(method = "lm", formula = y ~ poly(x, 4), color = "red") +
          labs(title = paste("Humidity vs Bike Demand Prediction for", selected_city),
               x = "Humidity",
               y = "Bike Demand Prediction") +
          theme_minimal()
      })
      
      # Render the text output for the clicked point
      output$bike_date_output <- renderText({
        req(input$plot_click)
        click_data <- input$plot_click
        nearest_index <- which.min(sqrt((as.numeric(choice_city_weather_bike_df$FDATETIME) - as.numeric(click_data$x))**2 +
                                     (choice_city_weather_bike_df$BIKE_PREDICTION - click_data$y)**2))
        clicked_point <- choice_city_weather_bike_df[nearest_index,] #%>%
          # filter(FDATETIME == click_data$x & BIKE_PREDICTION == click_data$y)
        
        if (nrow(clicked_point) > 0) {
          paste("DateTime:", clicked_point$FDATETIME[1], "\nBike Prediction:", clicked_point$BIKE_PREDICTION[1])
          #paste("DateTime:", click_data$x, "\nBike Prediction:", click_data$y)
        } else {
          "No data available for the clicked point."
        }
      })
    }
  })
})

