#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(moderndive)
library(colourpicker)
library(patchwork)
library(nycflights13)

weather <- weather

themes <- list("grey" = theme_grey(), "bw" = theme_bw(), "Light" = theme_light(),
               "Minimal" = theme_minimal(),
               "dark" = theme_dark())

# Define UI for application that draws a histogram
ui <- fluidPage(
    
    # Application title
    titlePanel("Seattle house prices"),
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("bins",
                        "Number of bins:",
                        min = 1,
                        max = 20,
                        value = 10),
            colourInput("color", "Select Fill colour", value = "orange"),
            colourInput("colorLine", "Select Line colour", value = "white"),
            selectInput("theme", label = h4("Select theme for plot"), choices = names(themes))
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
        bins <- input$bins
        colorfill <- input$color
        colorline <- input$colorLine
        
        # draw the histogram with the specified number of bins
        p1<- ggplot(weather, aes(temp))+ geom_histogram(bins = bins, 
                                                        col=colorline,
                                                        fill=colorfill,
                                                        size=1) +  # change binwidth
            labs(title="Temperature") 
        
        p2<- ggplot(weather, aes(humid))+ 
            geom_density(aes(fill=factor(month)), alpha=0.5) + 
            labs(title="Density plot",
                 subtitle="Humidity Grouped by months",
                 caption="Source: weather",
                 x="Humidity",
                 fill="Months of year")
        
        p1 + p2
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
