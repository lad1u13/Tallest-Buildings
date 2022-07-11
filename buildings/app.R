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


# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Old Faithful Geyser Data"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("bins",
                        "Number of bins:",
                        min = 1,
                        max = 50,
                        value = 30)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white')
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
