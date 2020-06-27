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
library(imager)
library(base64enc)


# Define UI for application that draws a histogram
ui <- fluidPage(
    titlePanel("Extracting dominant colours from pictures"),
                  mainPanel(
                    p("We will use k-means in the RGB space  as way a to find the most common colours in a picture. Clusters with the largest amount of elements will correspond to the dominant colours as you can see in the next image: "),
                    fluidRow(
                      column(3,img(src="autumn.jpg", height=200 , width= 150)),
                      column(3,plotOutput("pallete1", height=200 , width= 300))
                             ),
                    br(),
                    p("If you want to see the main colors of another image, enter the image link:"),
                    p(em("For example: https://concepto.de/wp-content/uploads/2015/03/paisaje-e1549600034372.jpg")),
                    textInput("link", h3("Image link"), 
                               value = "Enter text..."),
                    actionButton("runScript", "Run link"),
                    fileInput("file",h3("Upload Image")),
                    actionButton("runScript2", "Run file"),
                    br(),
                    br(),
                    fluidRow(
                      column(3, uiOutput("image_input", height=200 , width= 300)),
                      column(3,plotOutput("pallete_input", height=200 , width= 300)),
                    ),
                    fluidRow(
                      column(3, uiOutput("image_input_file")),
                      column(3,plotOutput("pallete_input_file", height=200 , width= 300)),
                    )
                   
                  )
    )



server <- function(input, output) {
  file <- "./www/autumn.jpg"
  im <- load.image(file)
  bdf <- as.data.frame(im, wide="c")
  
  set.seed(123)
  k2 <- kmeans(bdf, centers = 5, nstart = 25)
  C1 <- rgb(k2$centers[1,3],k2$centers[1,4],k2$centers[1,5])
  C2 <- rgb(k2$centers[2,3],k2$centers[2,4],k2$centers[2,5])
  C3 <- rgb(k2$centers[3,3],k2$centers[3,4],k2$centers[3,5])
  C4 <- rgb(k2$centers[4,3],k2$centers[4,4],k2$centers[4,5])
  C5 <- rgb(k2$centers[5,3],k2$centers[5,4],k2$centers[5,5])
  
  output$pallete1 <- renderPlot({
    barplot(c(1,1,1,1,1), col=c(C1,C2,C2,C4,C5))
  })

########## K MEANS FOR AN IMAGE ON A LINK  
  kmeans_ <- eventReactive(input$runScript, {
    file_input <- input$link
    im_input <- load.image(file_input)
    bdf_input <- as.data.frame(im_input, wide="c")
    set.seed(123)
    k2_input <- kmeans(bdf_input, centers = 5, nstart = 25)
    C1_input <- rgb(k2_input$centers[1,3],k2_input$centers[1,4],k2_input$centers[1,5])
    C2_input <- rgb(k2_input$centers[2,3],k2_input$centers[2,4],k2_input$centers[2,5])
    C3_input <- rgb(k2_input$centers[3,3],k2_input$centers[3,4],k2_input$centers[3,5])
    C4_input <- rgb(k2_input$centers[4,3],k2_input$centers[4,4],k2_input$centers[4,5])
    C5_input <- rgb(k2_input$centers[5,3],k2_input$centers[5,4],k2_input$centers[5,5])
    color <- c(C1_input,C2_input,C2_input,C4_input,C5_input)

  })
  # Prueba: https://concepto.de/wp-content/uploads/2015/03/paisaje-e1549600034372.jpg
  kmeans_image <- eventReactive(input$runScript, {
    input$link
    
  })
  
  output$image_input <- renderUI({
    
    tags$img(src=kmeans_image(), height=200 , width= 150)
    
  })
  
  output$pallete_input <- renderPlot({
    barplot(c(1,1,1,1,1), col=kmeans_())
    
  })
  
  
  ########## K MEANS FOR AN IMAGE ON A FILE 
  
  base64 <- reactive({
    inFile <- input[["file"]]
    if(!is.null(inFile)){
      dataURI(file = inFile$datapath, mime = "image/png")
    }
  })
  
  output[["image_input_file"]] <- renderUI({
    if(!is.null(base64())){
      tags$div(
        tags$img(src= base64(), width="100%"),
        style = "width: 400px;"
      )
    }
  })
  
  output[["pallete_input_file"]] <- renderUI({
    if(!is.null(base64())){
      file_input <- base64()
      im_input <- load.image(file_input)
      bdf_input <- as.data.frame(im_input, wide="c")
      set.seed(123)
      k2_input <- kmeans(bdf_input, centers = 5, nstart = 25)
      C1_input <- rgb(k2_input$centers[1,3],k2_input$centers[1,4],k2_input$centers[1,5])
      C2_input <- rgb(k2_input$centers[2,3],k2_input$centers[2,4],k2_input$centers[2,5])
      C3_input <- rgb(k2_input$centers[3,3],k2_input$centers[3,4],k2_input$centers[3,5])
      C4_input <- rgb(k2_input$centers[4,3],k2_input$centers[4,4],k2_input$centers[4,5])
      C5_input <- rgb(k2_input$centers[5,3],k2_input$centers[5,4],k2_input$centers[5,5])
      color <- c(C1_input,C2_input,C2_input,C4_input,C5_input)
      
      
      tags$div(
        barplot(c(1,1,1,1,1), col=kmeans_file()) 
      )
    }
  })
  
  
  # output$pallete_input_file <- renderPlot({
  #   barplot(c(1,1,1,1,1), col=kmeans_file())
  # })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
