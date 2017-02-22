library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output,session) {
    
    df <- read.csv("DataFile.csv")
    dem <- read.csv("demography.csv")[-1,c(1,2)]
    names(dem) <- c("State.Name","White")
    dem$White <- 1 - dem$White
    df <- merge(df,dem,sort=FALSE)
    
    vars <- list("Democrat vote percentage"="Dem.Vote", "Republican vote percentage"="Rep.Vote",
                 "High school graduate percentage"="High.school.degree", 
                 "Bachelor's degree percentage"="Bachelors.degree",
                 "Advanced degree percentage"="Advanced.degree",
                 "Guns per capita"="Gun.possession",
                 "Unemployment rate"="Unemployment",
                 "Non-caucasian percentage"="White")
    df$col <- ifelse(df$Dem.Vote>df$Rep.Vote,"blue","red")
    
    yv <- reactive({vars[[input$Yvar]]})
    xv <- reactive({input$Xvar})
    xv_nm <- reactive({vars[[input$Xvar]]})
    xv_dt <- reactive({df[[vars[[input$Xvar]]]]})
    yv <- reactive({input$Yvar})
    yv_nm <- reactive({vars[[input$Yvar]]})
    yv_dt <- reactive({df[[vars[[input$Yvar]]]]})
    
    model <- reactive({
        br_points <- brushedPoints(df, input$brush1, xvar=xv_nm(), yvar=yv_nm())
        #lm(yv_dt() ~ xv_dt(), data=df)
        if (nrow(br_points) < 2){
            return(lm(yv_dt() ~ xv_dt(), data=df))
        }
        lm(br_points[[vars[[input$Yvar]]]] ~ br_points[[vars[[input$Xvar]]]])
    })
    
    slope <- reactive({
        mod <- model()
        summ <- summary(mod)
        summ$coefficients[2,1]
    })
    
    pvalue <- reactive({
        mod <- model()
        summ <- summary(mod)
        summ$coefficients[[2,4]]
    })
    
    output$slop <- renderText({slope()})
    output$pval <- renderText({pvalue()})
    
    output$plot1 <- renderPlot({
        plot(xv_dt(), yv_dt(), xlab=xv(), ylab=yv(),
                         pch=16, cex=1.2, col=df$col)
        abline(model(), lwd=2)
        par(xpd=TRUE)
        legend('topleft', inset=c(0.22,-0.13),legend=c("Democratic State","Republican State"), 
               col=c("blue","red"), pch=c(16,16), cex=1.2, horiz=TRUE, bty="n")
        })
    
    output$docs <- renderUI({
        HTML("This application analyses the correlations between the voting
        behaviour in the US (for the 2016 elections) and several
        other population statistics per state.<br/><br/>
        The predictor variables include the number of
        registered guns per capita, the unemployment rate, the percentage of the
        population that is not caucasian and several
        measures of the educational attainment: the percentage of the
        population (aged 25 years and older) with a high school diploma,
        a bachelor's degree or an advanced degree.<br/>
        For the outcome variable the user can choose between the
        percentage of the population (per state) that voted democrat or the
        percentage that voted republican.<br/><br/>
        Both the predictor and the outcome variables can be chosen from a dropdown
        menu in the sidepanel on the left. The application shows a plot of these
        variables in the main panel, where each datapoint represents a state.
        A linear model correlating the chosen outcome and predictor variables
        is also fitted. This model is shown in the plot and the slope and p-value 
        of the correlation are given in the main panel.<br/><br/>
        The user can also choose to include only a subset of the data in the
        model by selecting a certain region in the plot. This can for example be
        useful to remove outliers.")})
  
})
