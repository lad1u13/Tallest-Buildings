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
ui <- fluidPage(title = "Worlds Tallest Buildings",
                theme = shinytheme('slate'),
        navbarPage(title = "Worlds Tallest Buildings",
                   tabPanel("Tallest Buildings", icon = icon("globe-americas"), 
                            # Include relevant filters - rank is recalculated adjusting for filters
                            sidebarLayout(
                              sidebarPanel(
                                titlePanel("Top 100 Tallest Buildings"), # Add title
                                helpText('Select the following filters to update the visualisation (ranks in map are recalculated):'),
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
                                tabsetPanel(type = 'tabs',
                                            tabPanel('Map', plotlyOutput("map", width = "900px", height = "550px")),
                                            tabPanel('Height Chart', plotlyOutput('heightplot', width = "900px", height = "550px"))),
                                htmlOutput("text")
                              )
                            )                                  
                   ),
                # Second tab in application
                tabPanel("Country Information", icon = icon('bar-chart-o'),
                         sidebarLayout(
                           sidebarPanel(
                             titlePanel("Countries with the top 100 tallest buildings"), # Second tab title
                             pickerInput('variable', # Filter option for country level variable (without option for all)
                                         label = 'Variable',
                                         choices = c('Number of top 100 tallest buildings', names(countries)[!names(countries) %in% c('Country', 'Region')]),
                                         options = list(`actions-box` = TRUE),
                                         multiple = FALSE,
                                         selected = 'Number of top 100 tallest buildings')
                           )
                           ,
                           mainPanel(plotlyOutput("map2", width = "1000px", height = "700px")) # Include main visualisation for second tab
                         )
                ),
                # Third tab in application
                tabPanel("City Population", icon = icon('chart-line'),
                         sidebarLayout(
                           sidebarPanel(
                             titlePanel("Population (with forecast) of cities with top 100 tallest buildings"), # Title for third tab
                             helpText('Choose a country and then a city (multiple cities are allowed) to visualise population time-series (where data is available):'),
                             pickerInput('countries', 
                                         label = 'Country',
                                         choices = unique(full_data$Country),
                                         options = list(`actions-box` = TRUE),
                                         multiple = FALSE,
                                         selected = 'USA'),
                             
                             pickerInput('cities',
                                         label = 'City',
                                         choices = unique(full_data$City),
                                         options = list(`actions-box` = TRUE),
                                         multiple = TRUE,
                                         selected = 'New York')
                           ),
                           mainPanel(plotlyOutput("pop_chart", width = "1000px", height = "700px"))
                         )  
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
 
 # Barplot to display the height (m) in order (updates based on filters)
 output$heightplot <- renderPlotly({
   selected() %>%
     plot_ly(x=~fct_inorder(Name),
             y =~Height,
             text =~ paste('Country:', Country, '\nCity:', City, '\nCompletion Year:', Completion),
             textposition = 'none') %>%
     layout(xaxis = list(title = 'Building name'),
            yaxis = list(title = 'Height (m)'))
 })
 
 # Output text
 output$text <- renderUI({HTML('<br> Rank displayed in the density legend of map corresponds to the rank of buildings in terms of height (where 1 is the tallest). This is recalculated based on filters selected on the left hand side. Note this visualisation is interactive and the tooltip displays upon hovering over markers. Some cities have multiple tallest buildings, which may require zooming in to see - the tooltip is useful in this case. <br> <br> <b> See other tabs at the top for country and population information. <b>')})
 
 
 # Output for visualisation in second tab (another plotly map but this time at the country level)
 output$map2 <- renderPlotly({
   full_data %>%
     plot_ly() %>%
     add_trace(type = 'choropleth',
               locations = ~CODE, # Code is taken from ISO3166 to locate regions on map based on country
               z = full_data[, input$variable], # Heatmap density is a function of selected input variable
               colorscale = "Viridis",
               text = ~paste('Country:', Country, # Tooltip information
                             '\n', paste(input$variable, ':', sep = ''), full_data[, input$variable],
                             '\nNumber of top 100 tallest buildings:', full_data[, 'Number of top 100 tallest buildings']) # Inlude metric aggregated at the country level (count of tallest buildings)
     ) %>%
     layout(mapbox = list(style = 'carto-positron'))
 })
 
 # Choices for conditional filter in third tab
 choices <- reactive(full_data %>%
                       filter(Country == input$countries) %>%
                       pull(City) %>%
                       unique())
 
 # Update selection of cities based on chosen country
 observeEvent(choices(), {
   choices <- choices()
   updatePickerInput(session = session, inputId = 'cities', choices = choices, selected = choices)
 })
 
 # Output 'pop_chart' is visualisation for third tab - displaying population per city based on conditional filters
 # Pivot the country data (but only for countries that have tallest buildings) and use ggplot to visualise - later convert to plotly using ggplotly
 output$pop_chart <- renderPlotly({
   p <- full_data %>%
     dplyr::select(one_of('City', 'Country', names(full_data)[names(full_data) %in% c(1931:2035)])) %>%
     pivot_longer(cols = names(full_data)[names(full_data) %in% c(1931:2035)], names_to = 'Year', values_to = 'Population') %>%
     filter(Country %in% input$countries) %>%
     filter(City %in% input$cities) %>%
     ggplot(aes(x = Year, y = Population, color = City)) + # Colour by city
     geom_point(aes(color = City)) +
     theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) # Rotate x-axis labels to improve appearance
   
   breaks <- round(seq(1950, 2035, length.out = 20),0) # Set breaks to avoid overlay of x-axis labels
   
   p <- p + scale_x_discrete(breaks = breaks, labels = as.character(breaks)) +
     geom_vline(xintercept = '2022')
   
   ggplotly(p) # Convert ggplot object to plotly to make interactive and display
 })
 
} 



# Run the application 
shinyApp(ui = ui, server = server)
