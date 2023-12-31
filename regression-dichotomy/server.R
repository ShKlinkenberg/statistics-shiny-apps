library(shiny)
library(ggplot2)


shinyServer(function(input, output) {
  
  # Load styling for plots.
  
  source("styling.R", local = TRUE)
  
  sample <- reactive({
    
    x <- c(rep(0, 20), rep(1, 20))
    
    # Generate random error per group.
    
    set.seed(1272)
    e <- rnorm(20, mean = 0, sd = 1)
    e <- sign(e) * sqrt(abs(e*abs(e) - mean(e*abs(e)))) # center in squares
    e <- c(e, e)
    
    # Set group means.
    
    nonsmoker <- input$nonsmokeraveragesli
    smoker    <- input$smokeraveragesli
    
    # Generate outcome scaled by unstandardized regression coefficient.
    
    y <- nonsmoker + (smoker - nonsmoker)*x + e
    category <- c(rep("Non-Smoker (0)", 20), rep("Smoker (1)", 20))
    
    data.frame(y = y, x = x,category)
  })
  
  # Make a function that corrects for the offset when plotting groups in ggplot.
  
  regfunc <-
    function(x, int=0, bet1 = 0)
    {
    (int + bet1*(x-1))
    }
  
  # Render the equation.
  
  output$equationui <- renderUI({
    withMathJax(
      helpText(
        paste("$$\\color{black}{attitude = }\\color{#2B83BA}{constant}\\color{black}{ + }\\color{#D7191C}{b }\\color{black}{* status}$$ \n $$\\color{black}{attitude = }\\color{#2B83BA}{",
              input$nonsmokeraveragesli,
              "}\\color{black}{ + }\\color{#D7191C}{",
              (input$smokeraveragesli - input$nonsmokeraveragesli),
              "}\\color{black}{* status}$$")
      )
    )
  })
  
  # Mainplot ---- 
  
  output$scatterplot <- renderPlot({
    
    df <- sample()
    
    # Extract model coefficients.
    
    model_coefficients <- coef(lm(y ~ x, df))
    intercept <- model_coefficients[1]
    beta1 <- model_coefficients[2]
    
    # Store means for making correct segments.
    
    meannonsmoke <- mean(df$y[df$x == 0])
    meansmoke <-  mean(df$y[df$x == 1])
   
    # Make a data-frame for the segments running parallel to x and y.
    
    segments <- 
      data.frame(x = c(1,2,0,0),
                       xend = c(1,2,1,2),
                       y = c(-6,-6,meannonsmoke,meansmoke),
                       yend = c(meannonsmoke,meansmoke,meannonsmoke,meansmoke))
    
    
    ggplot(df) + 
      geom_jitter(aes(x = category, y = y, colour = category), width = .1) + 
      stat_function(inherit.aes = FALSE,
                    data = data.frame(x = c(0,3)),
                    aes(x = x),
                    fun = regfunc,
                    args = list(int = intercept, bet1 = beta1)) + 
      geom_segment(inherit.aes = FALSE,
                   data = segments,
                   aes(x = x, xend = xend,y = y, yend= yend),
                   linetype = "dashed") +
      geom_vline(aes(xintercept = 1), color = "grey") +
      geom_hline(aes(yintercept = 0), color = "grey") +
      scale_colour_manual(name = "Smoking status", values = c("Non-Smoker (0)" = unname(brewercolors["Blue"]),
                                     "Smoker (1)" = unname(brewercolors["Red"]))) + 
      scale_y_continuous(name = "Attitude",
                         breaks = c(0, -5, round(meansmoke, digits=1), round(meannonsmoke, digits=1), 5)) +
      xlab("Smoking status") +
      coord_cartesian(ylim = c(-5,5)) + 
      theme_general() + 
      theme(legend.position = "none")
   
  })
  
})
