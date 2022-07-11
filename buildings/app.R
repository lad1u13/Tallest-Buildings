### Shiny application for world top 100 buildings
# Luke Day
#################################################
# Aim is to visualise where these buildings are with relevant filters
# Also visualise country information - it is of interest how other country level indicators relate to the number of tallest buildings
# e.g., are there more tall buildings in countries with a larger GDP?
#################################################

# Load packages
library(shiny)
library(shinyWidgets)
library(tidyverse)
library(plotly)
library(shinythemes)
library(maps)

# Access token for mapbox in plotly - set in global environment
Sys.setenv(MAPBOX_TOKEN = readLines('Mapbox_Token')) 

# Source file that reads and prepares data
source('read_and_prepare_data.R')

# Define UI for application
ui <- fluidPage(
                sidebarLayout(
                  sidebarPanel(
                  titlePanel("Top 100 Tallest Buildings"), # Add title
                
                  # Add material filter as a drop down box with an option for all
                  pickerInput('material',
                              label = 'Material',
                              choices = unique(full_data$Material),
                              options = list(`actions-box` = TRUE),
                              multiple = TRUE,
                              selected = unique(full_data$Material)),
                  
                  # Add function filter with drop down box (with option for all)  
                  pickerInput('func',
                              label = 'Function',
                              choices = unique(full_data$Function),
                              options = list(`actions-box` = TRUE),
                              multiple = TRUE,
                              selected = unique(full_data$Function)),
                  
                  # Add completion year filter as a slider
                  sliderInput('year',
                              label = 'Completion year',
                              min = min(full_data$Completion),
                              max = max(full_data$Completion),
                              value = c(min(full_data$Completion), max(full_data$Completion)), sep = "")
                  ),
                  mainPanel(
                    # Use two separate tabs in first page to display the map and distribution of heights separately
                    plotlyOutput('map', width = "1000px", height = "600px")
                  )
            )
                      )                                  

           
# Define server logic for application
server <- function(input, output, session){
 
 # For first tab: make reactive so that rank can be recalcuated based on chosen filters
 selected <- reactive(full_data %>%
                        dplyr::select(one_of('Country', 'City', 'Rank', 'Name', 'Material', 'Height', 'Floors', 'Function', 'Completion', 'lat', 'long')) %>%
                        filter(Material %in% input$material) %>% # Filter based on user input
                        filter(Function %in% input$func) %>% # Filter based on user input
                        filter(Completion >= input$year[1] & Completion <= input$year[2]) %>% # Filter based on user input
                        mutate(Rank = dense_rank(desc(Height)))) # Recalculate rank
 
 # Output for first tab visualisation (plotly map using mapbox)
 output$map <- renderPlotly({
   selected() %>% plot_mapbox() %>% # Use reactive data named selected to generate plotly visualisation that updates based on filters
     add_markers(
       x = ~long, # Longitude
       y = ~lat, # Latitude
       text = ~paste('Country:', Country, # Information to include in tooltip
                     '\nCity:', City,
                     '\nRank:', Rank,
                     '\nBuilding:', Name,
                     '\nHeight (m):', Height,
                     '\nFloors:', Floors,
                     '\nFunction:', Function,
                     '\nCompletion year:', Completion,
                     '\nMaterial:',  input$material),
       color = ~Rank, # Make both size of makers and colour relative to rank
       size = ~ -Rank
     )  %>%
     layout(mapbox = list(style = "dark", # Dark theme to make markers more visable
                          center = list( # Centre the plot based on median
                            lat = ~ median(lat),
                            lon = ~ median(long))))
  })
} 

# Run the application 
shinyApp(ui = ui, server = server)
