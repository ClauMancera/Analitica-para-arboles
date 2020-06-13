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

d1= weather %>%
    mutate(grupo = case_when(hour %in% 0:5 ~ "A",
                             hour %in% 6:11 ~ "B",
                             hour %in% 12:17 ~ "C",
                             TRUE ~ "D"))  %>%
    filter(grupo== "A")

d2= weather %>%
    mutate(grupo = case_when(hour %in% 0:5 ~ "A",
                             hour %in% 6:11 ~ "B",
                             hour %in% 12:17 ~ "C",
                             TRUE ~ "D")) %>%
    filter(grupo== "B")

d3= weather %>%
    mutate(grupo = case_when(hour %in% 0:5 ~ "A",
                             hour %in% 6:11 ~ "B",
                             hour %in% 12:17 ~ "C",
                             TRUE ~ "D")) %>%
    filter(grupo== "C")

d4= weather %>%
    mutate(grupo = case_when(hour %in% 0:5 ~ "A",
                             hour %in% 6:11 ~ "B",
                             hour %in% 12:17 ~ "C",
                             TRUE ~ "D")) %>%
    filter(grupo== "D")


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
        theme <- input$theme
        
        # draw the histogram with the specified number of bins
        p1<- ggplot(d1, aes(x= temp)) + geom_histogram(bins = bins, color = colorline, fill = colorfill) + 
            labs(title="Hour A")+ themes[[theme]]
        
        p2<- ggplot(d2, aes(x= temp)) + geom_histogram(bins = bins, color = colorline, fill = colorfill) + 
            labs(title="Hour B")+ themes[[theme]]
        
        p3<- ggplot(d3, aes(x= temp)) + geom_histogram(bins = bins, color = colorline, fill = colorfill) + 
            labs(title="Hour C")+ themes[[theme]]
        
        p4<- ggplot(d4, aes(x= temp)) + geom_histogram(bins = bins, color = colorline, fill = colorfill) + 
            labs(title="Hour D")+ themes[[theme]]
        
        (p1 + p2)/(p3 + p4)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
