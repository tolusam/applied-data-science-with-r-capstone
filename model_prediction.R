# Load necessary libraries
library(tidyverse)
library(httr)

# Function to get weather forecast data by cities
get_weather_forecast_by_cities <- function(city_names) {
  # Initialize empty lists to hold data temporarily
  weather_data <- list(
    city = character(),
    weather = character(),
    temperature = numeric(),
    visibility = numeric(),
    humidity = numeric(),
    wind_speed = numeric(),
    season = character(),
    hour = numeric(),
    forecast_date = character(),
    weather_label = character(),
    weather_detail_label = character()
  )
  
  # Get 5-day forecast data for each city
  for (city_name in city_names) {
    url_get <- 'https://api.openweathermap.org/data/2.5/forecast'
    api_key <- "bcad02b9630c5429c7e5a0ce820424a8"  # Replace with your own API key
    forecast_query <- list(q = city_name, appid = api_key, units = "metric")
    response <- GET(url_get, query = forecast_query)
    json_list <- content(response, as = "parsed")
    results <- json_list$list
    
    handle_null <- function(x) {
      if (is.null(x)) 10000 else x
    }
    
    for (result in results) {
      # Extract weather data
      weather_data$city <- c(weather_data$city, city_name)
      weather_data$weather <- c(weather_data$weather, result$weather[[1]]$main)
      weather_data$temperature <- c(weather_data$temperature, result$main$temp)
      weather_data$visibility <- c(weather_data$visibility, handle_null(result$visibility))
      weather_data$humidity <- c(weather_data$humidity, result$main$humidity)
      weather_data$wind_speed <- c(weather_data$wind_speed, result$wind$speed)
      
      forecast_datetime <- result$dt_txt
      weather_data$forecast_date <- c(weather_data$forecast_date, forecast_datetime)
      weather_data$hour <- c(weather_data$hour, as.numeric(strftime(forecast_datetime, format = "%H")))
      
      # Determine season
      month <- as.numeric(strftime(forecast_datetime, format = "%m"))
      season <- ifelse(month >= 3 & month <= 5, "SPRING",
                       ifelse(month >= 6 & month <= 8, "SUMMER",
                              ifelse(month >= 9 & month <= 11, "AUTUMN", "WINTER")))
      weather_data$season <- c(weather_data$season, season)
      
      # Create HTML labels for Leaflet visualization
      weather_data$weather_label <- c(weather_data$weather_label, paste(
        "<b><a href=''>", city_name, "</a></b>", "</br>",
        "<b>", result$weather[[1]]$main, "</b></br>"
      ))
      
      weather_data$weather_detail_label <- c(weather_data$weather_detail_label, paste(
        "<b><a href=''>", city_name, "</a></b>", "</br>",
        "<b>", result$weather[[1]]$main, "</b></br>",
        "Temperature: ", result$main$temp, " °C </br>",
        "Visibility: ", result$visibility, " m </br>",
        "Humidity: ", result$main$humidity, " % </br>",
        "Wind Speed: ", result$wind$speed, " m/s </br>",
        "Datetime: ", forecast_datetime, " </br>"
      ))
    }
  }
  
  # Create and return a tibble
  weather_df <- tibble(
    CITY_ASCII = weather_data$city,
    WEATHER = weather_data$weather,
    TEMPERATURE = weather_data$temperature,
    VISIBILITY = weather_data$visibility,
    HUMIDITY = weather_data$humidity,
    WIND_SPEED = weather_data$wind_speed,
    SEASONS = weather_data$season,
    HOURS = weather_data$hour,
    FORECASTDATETIME = weather_data$forecast_date,
    LABEL = weather_data$weather_label,
    DETAILED_LABEL = weather_data$weather_detail_label
  )
  
  return(weather_df)
}

# Function to load a saved regression model from a CSV file
load_saved_model <- function(model_name) {
  model <- read_csv(model_name)
  model <- model %>%
    mutate(Variable = gsub('"', '', Variable))
coefs <- setNames(model$Coef, as.list(model$Variable))
return(coefs)
}

# Function to predict bike-sharing demand using a saved regression model
predict_bike_demand <- function(TEMPERATURE, HUMIDITY, WIND_SPEED, VISIBILITY, SEASONS, HOURS) {
  model <- load_saved_model("model.csv")
  
  # Calculate weather-related regression terms
  weather_terms <- model['Intercept'] + TEMPERATURE * model['TEMPERATURE'] +
    HUMIDITY * model['HUMIDITY'] + WIND_SPEED * model['WIND_SPEED'] +
    VISIBILITY * model['VISIBILITY']
  
  # Calculate season-related regression terms
  season_terms <- sapply(SEASONS, function(season) {
    switch(season,
           'SPRING' = model['SPRING'],
           'SUMMER' = model['SUMMER'],
           'AUTUMN' = model['AUTUMN'],
           'WINTER' = model['WINTER'],
           0)
  })
  
  # Calculate hour-related regression terms
  hour_terms <- sapply(HOURS, function(hour) {
    switch(as.character(hour),
           '0' = model['0'], '1' = model['1'], '2' = model['2'], '3' = model['3'],
           '4' = model['4'], '5' = model['5'], '6' = model['6'], '7' = model['7'],
           '8' = model['8'], '9' = model['9'], '10' = model['10'], '11' = model['11'],
           '12' = model['12'], '13' = model['13'], '14' = model['14'], '15' = model['15'],
           '16' = model['16'], '17' = model['17'], '18' = model['18'], '19' = model['19'],
           '20' = model['20'], '21' = model['21'], '22' = model['22'], '23' = model['23'],
           0)
  })
  
  # Combine all terms and ensure non-negative values
  regression_terms <- as.integer(weather_terms + season_terms + hour_terms)
  regression_terms[regression_terms < 0] <- 0
  regression_terms[regression_terms = NA] <- 0
  
  return(regression_terms)
}

# Function to define bike-sharing demand level for Leaflet visualization
calculate_bike_prediction_level <- function(predictions) {
  levels <- sapply(predictions, function(prediction) {
    if (prediction <= 1000 && prediction >= 0) {'small'}
    else if (prediction > 1000 && prediction < 3000) {'medium'}
    else {'large'}
  })
  return(levels)
}

# Function to generate a data frame containing weather forecasting and bike prediction data
generate_city_weather_bike_data <- function() {
  cities_df <- read_csv("selected_cities.csv")
  weather_df <- get_weather_forecast_by_cities(cities_df$CITY_ASCII)
  
  results <- weather_df %>%
    mutate(BIKE_PREDICTION = predict_bike_demand(TEMPERATURE, HUMIDITY, WIND_SPEED, VISIBILITY, SEASONS, HOURS)) %>%
    mutate(BIKE_PREDICTION_LEVEL = calculate_bike_prediction_level(BIKE_PREDICTION))
  
  cities_bike_pred <- cities_df %>%
    left_join(results) %>%
    select(CITY_ASCII, LNG, LAT, TEMPERATURE, HUMIDITY, BIKE_PREDICTION, BIKE_PREDICTION_LEVEL, LABEL, DETAILED_LABEL, FORECASTDATETIME)
  
  return(cities_bike_pred)
}


