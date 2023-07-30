library(ggplot2)
library(shiny)

shinyServer(function(input, output) {
  source("styling.R", local = TRUE)
  
  se     <- 1.25  #sd of sampling distribution
  df     <- 30 #df of t distribution
  sampsd <- 0.8 #sd of sample
  mean   <- 5.5 #hypothesized population mean
  tc     <- qt(0.025, df, lower.tail = FALSE) #criticial quantiles 2.5%
  tc5    <- qt(0.05, df, lower.tail = FALSE) #criticial quantiles 5%
  under  <- -.10 #margin below sampling distribution
  
  #Calculating the right and left threshold
  right  <- mean + se * tc  #2.5%
  left   <- mean - se * tc  #2.5%
  right5 <- mean + se * tc5 #5%
  left5  <- mean - se * tc5 #5%
  
  #Function for scaling and shifting the t-distribution
  dtshift <- function(x,mean,sd,df) dt(x = (x - mean)/sd, df = df)
  
  #Function for stratified randomization of sample mean.
  newsample <- function() {
    stratum <- round(runif(1, 0.51, 5.49))
    ifelse(stratum == 1, runif(1, 2, left),
      ifelse(stratum == 2, runif(1, left, left5),
        ifelse(stratum == 3, runif(1, left5, right5),
          ifelse(stratum == 4, runif(1, right5, right),
                 runif(1, right, 9)
          )            
        )            
      )
    )
  }
  
  #Reactive containers for changing values
  reactive <- 
    reactiveValues(
      sampmean = newsample() #sample mean
    )
  react <- 
    reactiveValues(
      sample = data.frame(x = rnorm(df + 1, mean = isolate(reactive$sampmean), sd = sampsd), y = 0.9 * under) #sample
    )
  
  #Reset
  observeEvent(input$resetButton, {
    reactive$sampmean <- newsample() #sample mean
    react$sample <- data.frame(x = rnorm(df + 1, mean = reactive$sampmean, sd = sampsd), y = 0.9 * under) #sample
  })
  
  ##MAIN PLOT##
  output$mainplot <- renderPlot({
    
    #PLOT#
    ggplot(data.frame(x = c(0,8)), aes(x = x)) + 
      #Alternative ha T distribution function
      stat_function(fun  = dtshift,
                    args = list(mean = reactive$sampmean, sd = se, df = df),
                    n    = 1000) +
      # Observed effect size hline
      geom_segment(aes(x        = mean, 
                       xend     = reactive$sampmean,
                       y        = .2, yend = .2),
                       colour   = brewercolors["Blue"],
                       linetype = 2) +
      # Observed sample mean vline
      geom_segment(aes(x        = reactive$sampmean, xend = reactive$sampmean,
                       y        = under, yend = dtshift(reactive$sampmean, reactive$sampmean, se, df)),
                       colour   = brewercolors["Blue"],
                       linetype = 1) +
      # Observed effect size
      geom_text(label = "Observed effect size",
                aes(x = mean + mean(reactive$sampmean - mean) / 2,
                    y = 0.22),
                    hjust = .5,
                    size  = 3) +
      # Post Hoc Power Right
      stat_function(fun    = dtshift,
                    xlim   = c(right5,10),
                    geom   = "area",
                    colour = "black",
                    fill   = brewercolors["Green"],
                    alpha  = 0.3,
                    args   = list(mean = reactive$sampmean, 
                                  sd   = se, 
                                  df   = df),
                    n      = 1000) +
      #Right area 0 curve"5%
      stat_function(fun    = dtshift,
                    xlim   = c(right5,10),
                    geom   = "area",
                    colour = "black",
                    fill   = brewercolors["Blue"],
                    alpha  = 1,
                    args   = list(mean = mean, sd = se, df = df),
                    n      = 1000) +
      #5% label right
      geom_text(label = "5%",
                aes(x = right5 * 1.02 ,
                    y =  dtshift(right5, mean, se, df) + 0.01),
                colour = "White",
                hjust = .15,
                vjust = 3.5,
                size = 5) +
      #T distribution function
      stat_function(fun  = dtshift,
                    args = list(mean = mean, sd = se, df = df),
                    n    = 1000) +
      #critical value label right
      # geom_text(label = format(round(right, 1),nsmall = 1),
      #           aes(x = right,
      #               y =  -0.02),
      #           hjust = 0.5,
      #           size = 3) +
      # critical value 5% label right
      geom_text(label = format(round(right5, 1),nsmall = 1),
                aes(x = right5,
                    y =  -0.02),
                hjust = 0.5,
                size = 3) +
      # Horizontal axis for sampling distribution
      geom_hline(aes(yintercept = 0)) +
      # Hypothesized population mean line
      geom_segment(aes(x = mean, xend = mean, 
                       y = 0, yend = 0.4)) +
      #sample scores
      geom_point(data = react$sample[react$sample$x >= 1 & react$sample$x <= 10,], aes(x = x, y = y), 
                 colour = brewercolors["Red"]) +
      #Sample average vline
      geom_segment(aes(x = reactive$sampmean, xend = reactive$sampmean,
                       y = under, yend = dtshift(reactive$sampmean, mean, se, df)),
                   colour = brewercolors["Red"]) +
      # Sample average p value (left)
      # geom_text(label = paste0(format(round(pt((reactive$sampmean - mean)/se, df, 
      #                            lower.tail = TRUE), 
      #                         digits = 3), nsmall = 3)),
      #           aes(x = reactive$sampmean ,
      #               y =  0.01),
      #           colour = brewercolors["Red"],
      #           hjust = 1.1,
      #           vjust = 0,
      #           size = 5) +
      # #Sample average p value (right)
      # geom_text(label = paste0(format(round(pt((reactive$sampmean - mean)/se, df, 
      #                                          lower.tail = FALSE), 
      #                                       digits = 3), nsmall = 3)),
      #           aes(x = reactive$sampmean ,
      #               y =  0.01),
      #           colour = brewercolors["Red"],
      #           hjust = -0.1,
      #           vjust = 0,
      #           size = 5) +
      #Scaling and double axis definitions
      scale_x_continuous(breaks   = c(1, reactive$sampmean, 10),
                         limits   = c(1, 10),
                         labels   = c(1, paste0("Mean = ",round(reactive$sampmean, digits = 2)), 10),
                         name     = "Sample media literacy scores",
                         sec.axis = sec_axis(~ .,
                         breaks   = c(1, mean, 10),
                         labels   = c(1, format(round(mean,1), nsmall=1), 10),
                         name     = "Hypothesized population mean media literacy score"),
                         expand   = c(.02, .02)) +
      scale_y_continuous(breaks = NULL, 
                         limits = c(under, 0.4),
                         name   = "",
                         expand = c(0, 0)) + 
      #Theme                                       
      theme_general() +
      theme(panel.border = element_rect(colour = NA), 
            axis.line.y  = element_blank(),
            plot.margin  = margin(10,0,10,0))
  })
})
