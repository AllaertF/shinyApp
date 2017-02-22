library(shiny)

shinyUI(fluidPage(
  
  # Application title
  titlePanel("Linking 2016 US Election Results To Population Statistics"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
       selectInput("Yvar", "Please select the outcome variable:", choices=c("Democrat vote percentage", 
                                                   "Republican vote percentage"),
                   selected="Democrat vote percentage"),
       selectInput("Xvar", "Please select the predictor variable:", choices=c("High school graduate percentage", 
                                                  "Bachelor's degree percentage",
                                                  "Advanced degree percentage",
                                                  "Guns per capita",
                                                  "Unemployment rate",
                                                  "Non-caucasian percentage"
                                                  ),
                   selected="Advanced degree percentage")
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
        tabsetPanel(type="tabs",
                    tabPanel("App",
                             plotOutput("plot1", brush = brushOpts(id="brush1")),
                             h5("Slope of the correlation:"),
                             textOutput("slop"),
                             h5("Significance of the correlation (p-value):"),
                             textOutput("pval")),
                    tabPanel("How to use",
                             h3("How to use this app"),
                             htmlOutput("docs")))
    )
  )
))
