# Load required libraries
require(leaflet)


# Create a RShiny UI
shinyUI(
  fluidPage(padding=5,
            titlePanel(textOutput("app_title")), 
            # Create a side-bar layout
            sidebarLayout(
              # Create a main panel to show cities on a leaflet map
              mainPanel(
                # leaflet output with id = 'city_bike_map', height = 1000
                leafletOutput("city_bike_map", height = 1000, width = 1600)
              ),
              # Create a side bar to show detailed plots for a city
              sidebarPanel(
                # select drop down list to select city
                selectInput("city", width = 720,
                            label = "Select a City", 
                            choices = c("All", "Seoul", "New York", "Paris", "Suzhou", "London"), 
                            selected = "All"), 
                # Seoul, South Korea, New York, USA, Paris, France, Suzhou, China, and London, UK
                # Placeholder for displaying additional city details (like plots or info)
                # plotOutput("city_detail_plot")
                plotOutput("temp_line", height = 300, width = 720),
                plotOutput("bike_line", height = 300, width = 720, click = "plot_click"),
                verbatimTextOutput("bike_date_output"),
                plotOutput("humidity_pred_chart", height = 300, width = 720)
              ))
  ))