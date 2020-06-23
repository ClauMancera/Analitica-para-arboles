
# https://www.tidymodels.org/
#https://shiny.rstudio.com/images/shiny-cheatsheet.pdf
library(shiny)
library(tidymodels)

d_tidymodels <- "tidymodels is a “meta-package” for modeling and statistical analysis that share the underlying design philosophy, grammar, and data structures of the tidyverse.

It includes a core set of packages that are loaded on startup:

-broom takes the messy output of built-in functions in R, such as lm, nls, or t.test, and turns them into tidy data frames.

-dials has tools to create and manage values of tuning parameters.

-dplyr contains a grammar for data manipulation.

-ggplot2 implements a grammar of graphics.

-infer is a modern approach to statistical inference.

-parsnip is a tidy, unified interface to creating models.

-purrr is a functional programming toolkit.

-recipes is a general data preprocessor with a modern interface. It can create model matrices that incorporate feature engineering, imputation, and other help tools.

-rsample has infrastructure for resampling data so that models can be assessed and empirically validated.

-tibble has a modern re-imagining of the data frame.

-tune contains the functions to optimize model hyper-parameters.

-workflows has methods to combine pre-processing steps and models into a single object.

-yardstick contains tools for evaluating models (e.g. accuracy, RMSE, etc.)


There are a few modeling packages that are also installed along with tidymodels (but are not attached on startup):

*tidypredict translates some model prediction equations to SQL for high-performance computing.

*tidyposterior can be used to compare models using resampling and Bayesian analysis.

*tidytext contains tidy tools for quantitative text analysis, including basic text summarization, sentiment analysis, and text modeling.
"

d_rsample <- "rsample contains a set of functions to create different types of resamples and corresponding classes for their analysis. The goal is to have a modular set of methods that can be used across different R packages for:

*traditional resampling techniques for estimating the sampling distribution of a statistic and
*estimating model performance using a holdout set
The scope of rsample is to provide the basic building blocks for creating and analyzing resamples of a data set but does not include code for modeling or calculating statistics. The “Working with Resample Sets” vignette gives demonstrations of how rsample tools can be used.

Note that resampled data sets created by rsample are directly accessible in a resampling object but do not contain much overhead in memory. Since the original data is not modified, R does not make an automatic copy."

d_parsnip <-"The goal of parsnip is to provide a tidy, unified interface to models that can be used to try a range of models without getting bogged down in the syntactical minutiae of the underlying packages."

d_recipes <- "The recipes package is an alternative method for creating and preprocessing design matrices that can be used for modeling or visualization. While R already has long-standing methods for creating these matrices (e.g. formulas and model.matrix), there are some limitations to what the existing infrastructure can do.

The idea of the recipes package is to define a recipe or blueprint that can be used to sequentially define the encodings and preprocessing of the data (i.e. “feature engineering”). "

d_workflow <- "A workflow is an object that can bundle together your pre-processing, modeling, and post-processing requests. For example, if you have a recipe and parsnip model, these can be combined into a workflow. 
The advantages are:

-You don’t have to keep track of separate objects in your workspace.

-The recipe prepping and model fitting can be executed using a single call to fit().

-If you have custom tuning parameter settings, these can be defined using a simpler interface when combined with tune.

-In the future, workflows will be able to add post-processing operations, such as modifying the probability cutoff for two-class models."

d_tune <- "The goal of tune is to facilitate the tuning of hyper-parameters the tidymodels packages. It relies heavily on recipes, parsnip, and dials."

d_yardstick <- "yardstick is a package to estimate how well models are working using tidy data principles. See the package webpage for more information."

d_broom <- "broom summarizes key information about models in tidy tibble()s. 
broom provides three verbs to make it convenient to 
interact with model objects:
- tidy() summarizes information about model components
- glance() reports information about the entire model
- augment() adds informations about observations to a dataset
For a detailed introduction, please see vignette(\"broom\").

broom tidies 100+ models from popular modelling packages and almost all of the model
objects in the stats package that comes with base R. vignette(\"available-methods\")
lists method availability.

If you aren’t familiar with tidy data structures and want to know how they
can make your life easier, we highly recommend reading Hadley Wickham’s Tidy Data.
"
d_dials= "This package contains tools to create and manage values of tuning parameters and is designed to integrate well with the parsnip package.

The name reflects the idea that tuning predictive models can be like turning a set of dials on a complex machine under duress."

overviews_keys <- c("tidymodels","rsample","parsnip", "recipes","workflows","tune","yardstick","broom","dials")
overviews_descriptions <- c(d_tidymodels, d_rsample, d_parsnip, d_recipes, d_workflow, d_tune, d_yardstick, d_broom, d_dials)
names(overviews_descriptions) <- overviews_keys

# Define UI for application
ui <- fluidPage(
    navbarPage(
        # theme = "cerulean",  # <--- To use a theme, uncomment this
        #p("Tidymodels", style="color:firebrick"),
        a("Tidymodels", style="color:firebrick", href="https://www.tidymodels.org/"),
        tabPanel("Installation",
                 fluidRow(
                     column(5, img(src = "tidymodels.png", hight=300, width = 300)),
                     column(3,
                            h3("TIDYMODELS"), br(),
                            p("The tidymodels framework is a collection of packages for modeling 
                            and machine learning using", strong("tidyverse"),"principles."), 
                            p("Install tidymodels with:"),
                            br(),br(), code("install.packages(\"tidymodels\")"), br(),br(),
                            p("Run", em("library(tidymodels)"), "to load the core packages and make 
                              them available in your current R session"))
                 )
        ),
        tabPanel("Packages", 
                 h3("CORE TIDYMODELS"),
                 #img(src = "logo" , height=300, width = 300),
                 #imageOutput("logo", height=300, width = 300),
                 uiOutput(outputId = "logo"),
                 p("The core tidymodels packages work together to enable a wide variety of modeling approaches."),
                 selectInput("state", "Choose a tidymodel library:",
                             list(`package` = c("tidymodels","rsample","parsnip", "recipes","workflows","tune","yardstick","broom","dials")) # hacer una lista limpia con los paquetes principales
                 ),
                 verbatimTextOutput("result")
                 #htmlOutput("result")
                 
        ),
        tabPanel("Learn", "After you know what you need to get started with tidymodels, you can learn more and go further. Find articles here to help you solve specific problems using the tidymodels framework. 
                 Articles are organized into four categories:",
                 br(),
                 br(),
                 tabsetPanel(
                     tabPanel("Perform Statistical Analysis",
                              br(),
                              h4("Examples: "),
                              tabsetPanel(
                              tabPanel("Correlation and regression fundamentals with tidy data principles",
                                       #tableOutput("table"),
                                       br(),
                                       h5(em("Analyze the results of correlation tests and simple regression models for many data sets at once.")),                                       h3("INTRODUCTION"),
                                       p("While the tidymodels package", em("broom")," is useful for summarizing the result of a single analysis in a consistent format, it is really designed for high-throughput applications, where you must combine results from multiple analyses. These could be subgroups of data, analyses using different models, bootstrap replicates, permutations, and so on. In particular, it plays well with the", code("nest()/unnest()")," functions from tidyr and the",  code("map()"), "function in", em("purrr.")),
                                       h3("CORRELATION ANALYSIS"),
                                       p("Let’s demonstrate this with a simple data set, the built-in Orange. We start by coercing Orange to a tibble. This gives a nicer print method that will be especially useful later on when we start working with list-columns."),
                                       p("This contains 35 observations of three variables: Tree, age, and circumference. Tree is a factor with five levels describing five trees. As might be expected, age and circumference are correlated:"),
                                       verbatimTextOutput("cod_corr"),
                                       plotOutput("corr"),
                                       
                                       h3("REGRESSION MODELS"),
                                       p("This type of workflow becomes even more useful when applied to regressions"),
                                       p("When we tidy these results, we get multiple rows of output for each model."),
                                       p("We can handle multiple regressions at once using exactly the same workflow ."),
                                       verbatimTextOutput("cod_reg"),
                                       plotOutput("reg"),
                                       ),
                              
                              tabPanel("K-means clustering with tidy data principles",
                                       br(),
                                       h5(em("Summarize clustering characteristics and estimate the best number of clusters for a data set.")),
                                       h3("INTRODUCTION"),
                                       p("To use the code in this article, you will need to install the following packages: tidymodels and tidyr.
                                       K-means clustering serves as a useful example of applying tidy data principles to statistical analysis, and especially the distinction between the three tidying functions:"),
                                       tags$div(
                                           tags$ul(
                                               tags$li(em("tidy()")),
                                               tags$li(em("augment()")),
                                               tags$li(em("glance()"))
                                           )
                                       ),
                                       h3("CLUSTERING IN R"),
                                       p("We’ll use the built-in kmeans() function, which accepts a data frame with all numeric columns as it’s primary argument."),
                                       
                                       h3("EXPLORATORY CLUSTERING")
                                       ),
                              tabPanel("Bootstrap resampling and tidy regression models",
                                       br(),
                                       h5(em("Apply bootstrap resampling to estimate uncertainty in model parameters.")),
                                       h3("INTRODUCTION"),
                                       p("Bootstrapping consists of randomly sampling a data set with replacement, then performing the analysis individually on each bootstrapped replicate. The variation in the resulting estimate is then a reasonable approximation of the variance in our estimate."),
                                       
                                       h3("BOOTSTRAPPING MODELS"),
                                       p("We’ll use the built-in kmeans() function, which accepts a data frame with all numeric columns as it’s primary argument."),
                                       
                                       h3("POSSIBLE MODEL FITS")
                              ),
                              tabPanel("Hypothesis testing using resampling and tidy data",
                                       br(),
                                       h5(em("Perform common hypothesis tests for statistical inference using flexible functions.")),
                                       h3("INTRODUCTION"),
                                       p("Regardless of which hypothesis test we’re using, we’re still asking the same kind of question:"),
                                       p(em("Is the effect or difference in our observed data real, or due to chance?")),
                                       p("To answer this question, we start by assuming that the observed data came from some world where “nothing is going on” (i.e. the observed effect was simply due to random chance), and call this assumption our null hypothesis. 
                                         We then calculate a test statistic from our data that describes the observed effect. We can use this test statistic to calculate a p-value, giving the probability that our observed data could come about if the null hypothesis was true.
                                          If this probability is below some pre-defined significance level α, then we can reject our null hypothesis."),
                                       h3("BOOTSTRAPPING MODELS"),
                                       p("We’ll use the built-in kmeans() function, which accepts a data frame with all numeric columns as it’s primary argument."),
                                       
                                       h3("POSSIBLE MODEL FITS")
                              ),
                              tabPanel(" Statistical analysis of contingency tables",
                                       br(),
                                       h5(em("Use tests of independence and goodness of fit to analyze tables of counts.")),
                                       h3("INTRODUCTION"),
                                       p("Bootstrapping consists of randomly sampling a data set with replacement, then performing the analysis individually on each bootstrapped replicate. The variation in the resulting estimate is then a reasonable approximation of the variance in our estimate."),
                                       
                                       h3("BOOTSTRAPPING MODELS"),
                                       p("We’ll use the built-in kmeans() function, which accepts a data frame with all numeric columns as it’s primary argument."),
                                       
                                       h3("POSSIBLE MODEL FITS")
                              )
                              )
                              
                     ),
                     tabPanel("Create Robust Models", "This panel is intentionally left blank"),
                     tabPanel("Tune, compare and work with your models", "This panel is intentionally left blank"),
                     tabPanel("Develop Custom Modeling Tools", "This panel is intentionally left blank")
                 )
                 
        )
    )
    
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    output$txtout <- renderText({
        paste(input$txt, input$slider, format(input$date), sep = ", ")
    })
    output$table <- renderTable({
        head(cars, 4)
    })
    output$result <- renderText({
        paste(overviews_descriptions[input$state])
        #HTML(overviews_descriptions[input$state])
    })
    output$logo <- renderUI({
        #img(src = paste0(input$state, ".png") , height=300, width = 300)
        #img(src = "tidymodels.png", hight=300, width = 300)
        #list(src = "tidymodels",
        #     contentType = 'image/png',
        #     width = 400,
        #     height = 300)
        tags$img(src = paste0( input$state, ".png") , height=300, width = 300)
    }) 
    output$cod_corr <- renderText({
        "library(tidymodels)
        data(Orange)
        Orange <- as_tibble(Orange)
        Orange
        #> # A tibble: 35 x 3
        #>    Tree    age circumference
        #>    <ord> <dbl>         <dbl>
        #>  1 1       118            30
        #>  2 1       484            58
        #>  3 1       664            87
        #>  4 1      1004           115
        #>  5 1      1231           120
        #>  6 1      1372           142
        #>  7 1      1582           145
        #>  8 2       118            33
        #>  9 2       484            69
        #> 10 2       664           111
        #> # … with 25 more rows
        
        cor(Orange$age, Orange$circumference)
        #> [1] 0.914"
    })
    output$corr <- renderPlot({
        ggplot(Orange, aes(age, circumference, color = Tree)) +
            geom_line()+
            ggtitle("Plot of cincurference \n by age")
    })
    output$cod_reg <- renderText({
        "lm_fit <- lm(age ~ circumference, data = Orange)
summary(lm_fit)
#> 
#> Call:
#> lm(formula = age ~ circumference, data = Orange)
#> 
#> Residuals:
#>    Min     1Q Median     3Q    Max 
#> -317.9 -140.9  -17.2   96.5  471.2 
#> 
#> Coefficients:
#>               Estimate Std. Error t value Pr(>|t|)    
#> (Intercept)     16.604     78.141    0.21     0.83    
#> circumference    7.816      0.606   12.90  1.9e-14 ***
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#> Residual standard error: 203 on 33 degrees of freedom
#> Multiple R-squared:  0.835,	Adjusted R-squared:  0.83 
#> F-statistic:  166 on 1 and 33 DF,  p-value: 1.93e-14
        
        tidy(lm_fit)
#> # A tibble: 2 x 5
#>   term          estimate std.error statistic  p.value
#>   <chr>            <dbl>     <dbl>     <dbl>    <dbl>
#> 1 (Intercept)      16.6     78.1       0.212 8.33e- 1
#> 2 circumference     7.82     0.606    12.9   1.93e-14
        
        Orange %>%
  nest(data = c(-Tree)) %>% 
  mutate(
    fit = map(data, ~ lm(age ~ circumference, data = .x)),
    tidied = map(fit, tidy)
  ) %>% 
  unnest(tidied) %>% 
  select(-data, -fit)
#> # A tibble: 10 x 6
#>    Tree  term          estimate std.error statistic   p.value
#>    <ord> <chr>            <dbl>     <dbl>     <dbl>     <dbl>
#>  1 1     (Intercept)    -265.      98.6      -2.68  0.0436   
#>  2 1     circumference    11.9      0.919    13.0   0.0000485
#>  3 2     (Intercept)    -132.      83.1      -1.59  0.172    
#>  4 2     circumference     7.80     0.560    13.9   0.0000343
#>  5 3     (Intercept)    -210.      85.3      -2.46  0.0574   
#>  6 3     circumference    12.0      0.835    14.4   0.0000290
#>  7 4     (Intercept)     -76.5     88.3      -0.867 0.426    
#>  8 4     circumference     7.17     0.572    12.5   0.0000573
#>  9 5     (Intercept)     -54.5     76.9      -0.709 0.510    
#> 10 5     circumference     8.79     0.621    14.1   0.0000318"
    })
    
}

# Run the application 
shinyApp(ui, server)
