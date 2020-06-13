#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
    titlePanel("Claudia Patricia Mancera"),
    sidebarLayout(position = "left",
                  sidebarPanel(img(src="Capture.PNG", height=150 , width= 200),
                               h4("Ingenieria Financiera"),
                               h4("ITESO"),
                               br(),
                               p("You can find more about me in", a("Linkedin Profile.",href = "htts://www.google.com"))),
                  mainPanel(
                      
                      h1("Current role", align="center"),
                      h3("Financial Analyst Senior"),
                      h5("Customer Support Amercias"),
                      h5("HP INC"),
                      br(),
                      h3(strong("Professional Profile")),
                      p("Financial Engineer with corporate finance expertise in controllership and cost accounting organizations. Expert in Reporting, macros development, and processes improvements and automation. Passionate about data analysis, projects and continuous improvement."),
                      br(),
                      h3(strong("Skills")),
                      fluidRow(
                          column(3,tags$div(
                              tags$ul(em("Soft:"),
                                      tags$li("Critical Thinking"),
                                      tags$li("Growth Mindset")
                                      )
                          
                                            )
                                ),
                          column(3,tags$div(
                              tags$ul(em("Hard:"),
                                        tags$li("R"),
                                        tags$li("Python")
                                      )
                                            )
                                )
                              )
                      
                  )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {}

# Run the application 
shinyApp(ui = ui, server = server)
