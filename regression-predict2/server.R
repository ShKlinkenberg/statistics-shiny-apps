library(shiny)
library(ggplot2)


shinyServer(function(input, output) {
  #Load styling for plots
  source("styling.R", local = TRUE)
  #Set constant.
  contact_mean = 4.5
  
  #CREATE PREDICTOR
  n <- 600 #number of data points
  set.seed(4932)
  exposure <- runif(n) * 10
  # contact.
  set.seed(4321)
  contact <- rnorm(n, mean = contact_mean, sd = 2)
  #smoker
  smoker <- sample(c(0,1),size = n, replace = TRUE)
  # Create outcome.
  set.seed(391)
  attitude <-
    0.65 - 0.6 * exposure + 0.5 * smoker + 0.15 * contact + rnorm(n, mean = 0, sd = 0.9)

  attfun <- function(exposure, contact, smoker) {
    0.65 - 0.6 * exposure + 0.5 * smoker + 0.15 * contact
  }

  output$mainplot <- renderPlot({
    
    
    #Create data frames for plotting
    df <- data.frame(x = c(0, 6)) #Limits for line
    scatter <- data.frame(attitude = attitude, exposure = exposure, smoker = smoker, contact = contact)
    scatter$plane <- dnorm(contact, input$contactvalueslider, sd = .5)
    #Plot
    ggplot(scatter,aes(x = exposure, y = attitude)) +
      geom_point(data = scatter,
                 shape = 21,
                 size = 3,
                 aes(x = exposure,
                     y = attitude)) +
    stat_function(data= df,inherit.aes = FALSE,
        fun = attfun,
        args = list(contact = input$contactvalueslider, smoker =  as.numeric(input$smokeselector)),
        n = 500,
        alpha = 1,
        size = 1, colour = brewercolors[5]) + 
      coord_cartesian(xlim = c(0, 10), ylim = c(-5, 5)) +
      ylab("Attitude") +
      xlab("Exposure") +
      theme_general()
    
  })
  
  ##FORMULA OUTPUT##
  output$formulamult <- renderUI({
    withMathJax(
      helpText(
        paste("$$\\color{red}{attitude = 0.65 - 0.6 * exposure + 0.5 * }\\color{red}{Smoker(",
              as.numeric(input$smokeselector),
              ")}\\color{red}{ + 0.15 * contact(",input$contactvalueslider,")}$$")
      )
    )
  })
  output$formulasimple <- renderUI({
    withMathJax(
      helpText(
        paste("$$\\color{blue}{attitude = ", 
              0.65 + 0.5 * as.numeric(input$smokeselector) + 0.15 * input$contactvalueslider, 
              "- 0.6 * exposure}$$")
      )
    )
  })
})