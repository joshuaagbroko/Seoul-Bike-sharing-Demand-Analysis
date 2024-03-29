
# Create a RShiny UI
ui <- shinyUI(
  fluidPage(padding=5,
            titlePanel("Bike-sharing demand prediction app"), 
            # Create a side-bar layout
            sidebarLayout(
              # Create a main panel to show cities on a leaflet map
              mainPanel(
                # leaflet output with id = 'city_bike_map', height = 1000
                leafletOutput("city_bike_map", height = 1000)
              ),
              # Create a side bar to show detailed plots for a city
              sidebarPanel(
                # select drop down list to select city
                selectInput(inputId = "city_dropdown",
                            label = "Country Name", choices = c("All", "Seoul", "Suzhou", "London", "New York", "Paris")
                ),
                
                plotOutput('temp_line', height=250),
                plotOutput('bike_line', click = "plot_click", height=250),
                verbatimTextOutput("bike_date_output"),
                plotOutput('humidity_pred_chart', height = 250)
                
                
                
              ))
  ))





