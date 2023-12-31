library(shiny)
library(ggplot2)
library(RColorBrewer)
library(scales)

#Load general plot theme and colors for color brewer
source("styling.R",local = TRUE)

#Function that returns coordinates of dots, so that they stack nicely
#on top of each other. 
dotcoordinates <- function(numberofdots, dotsinrow = 5, xscale = 1, yscale = 1){
  xtemp <- numeric()
  ytemp <- numeric()
  
  if(numberofdots == 0) {
    xtemp <- numeric(0)
    ytemp <- numeric(0)
  }
  if (numberofdots <= dotsinrow & numberofdots > 0) {
    xtemp <- seq(1, numberofdots)
    ytemp <- rep(1, numberofdots)
  }
  if (numberofdots > dotsinrow) {
    xtemp <- rep(c(1:dotsinrow), floor(( numberofdots / dotsinrow)))
    if((numberofdots %% dotsinrow) != 0){
      xtemp <- c(xtemp, 1:(numberofdots %% dotsinrow))
    }
    ytemp <- rep(1:floor(numberofdots / dotsinrow), each = dotsinrow)
    ytemp <-
      c(ytemp, rep(ceiling(numberofdots / dotsinrow), numberofdots %% dotsinrow))
  }
  xtemp <- xtemp * xscale
  ytemp <- ytemp * yscale
  return(cbind(xtemp,ytemp))
}

shinyServer(function(input, output) {

  N <- 50 #size of a single sample
  reps <- 1000 #number of of repetitions for large bootstrap

 # Reactive container for changing values
  samples <- 
    reactiveValues(
      firstsample = rep(1:5,each = 10),
      hist = factor(),
      lastsample = factor()
    )
  
  #When initial sample is taken, take sample, clear history.
  observeEvent(input$firstsampleaction,{
    samples$firstsample <- sample(1:5, size = 50, replace = TRUE)
    samples$hist <- numeric()
    samples$lastsample <- numeric()
  })
  #When single sample is taken, take sample, append to history
  observeEvent(input$bootstrapsmallaction,
                                   {
                                     newsample <-
                                       replicate(sample(x = samples$firstsample,
                                              size = N,
                                              replace = TRUE), n = 1)
                                     samples$lastsample <<- newsample
                                     newprop <-
                                       prop.table(table(newsample))["5"]
                                     newprop[is.na(newprop)] <- 0
                                     samples$hist <<- c(samples$hist, newprop)
                                     
                                     # Limit size of samples to 3 Mb
                                     if(object.size(samples) > 3e+06) {
                                       samples$firstsample <<- sample(1:5, size = 50, replace = TRUE)
                                       samples$hist <<- numeric()
                                       samples$lastsample <<- numeric()
                                     }
                                     
                                   })
  #When big sample is taken, take sample, append to history, store last sample
  observeEvent(input$bootstraplargeaction,
                                   {
                                    newsample <-
                                     replicate(sample(x = samples$firstsample,
                                            size = N,
                                            replace = TRUE),
                                            n = reps)
                                    samples$lastsample <<- 
                                      newsample[ , reps]
                                    newprop <-
                                      apply(X = newsample,
                                            MARGIN = 2,
                                            function(x) prop.table(table(x))["5"])
                                    newprop[is.na(newprop)] <- 0
                                    samples$hist <<- c(samples$hist, newprop)
                                    
                                    # Limit size of samples to 3 Mb
                                    if(object.size(samples) > 3e+06) {
                                      samples$firstsample <<- sample(1:5, size = 50, replace = TRUE)
                                      samples$hist <<- numeric()
                                      samples$lastsample <<- numeric()
                                    }
                                    
                                   })
  
  # Render Plot of last sample
  output$sampleplot <- renderPlot({
    #Store sample and make factor
    sample <- samples$firstsample
    sample <- factor(sample,
              levels = c(1:5),
              labels = sort(c("Red",
                             "Orange",
                             "Yellow",
                             "Green",
                             "Blue")))
    #Sort before passing to generate dotcoordinates
    sample <- sort(sample)
    
    #Make coordinates for all five categories
    tempcoord <- numeric()
    coordinates <- numeric()
    for (i in 1:length(levels(sample))) {
      data <- sample[sample == levels(sample)[i]]
      tempcoord <- dotcoordinates(length(data),yscale = 2)
      tempcoord[, 1] <- tempcoord[, 1] + (((i - 1) * 6))
      coordinates <- rbind(coordinates, tempcoord)
    }
    
    # Store dotcoordinates in data frame
    df <- data.frame(sample,coordinates)
    
    # Generate plot
    ggplot(data = df, aes(x = xtemp, y = ytemp, fill = sample)) +
      geom_point(shape = 21, size = 4, color = "black") +
      scale_fill_manual(values = brewercolors) +
      scale_x_continuous(
        name = "",
        breaks = c(2.5, 8.5, 15.5, 21.5, 28.5),
        limits = c(0, 30),
        labels = sort(names(brewercolors))
      ) +
      scale_y_continuous(name = "",
                         labels = c(),
                         limits = c(0, 15)) +
      ggtitle("Initial Sample") + 
      theme_general() + 
      theme(line = element_blank(),
            legend.position  = "none")
  })
  
  # Render bootstrapped examples
  output$bootstrappedplot <- renderPlot({
    # Store last sampleS  
    sample <- samples$lastsample
    sample <- factor(sample,
                     levels = c(1:5),
                     labels = sort(c("Red",
                                     "Orange",
                                     "Yellow",
                                     "Green",
                                     "Blue")))
    #Sort before passing to generate dotcoordinates
    sample <- sort(sample)
    #Make coordinates for all five categories
    tempcoord <- numeric()
    coordinates <- numeric()
    #Generate dotcoordinates for each category
    for (i in 1:length(levels(sample))) {
      data <- sample[sample == levels(sample)[i]]
      tempcoord <- dotcoordinates(length(data), yscale = 2)
      tempcoord[, 1] <- tempcoord[, 1] + (((i - 1) * 6))
      coordinates <- rbind(coordinates, tempcoord)
    }
      
      df <- data.frame(sample,coordinates)
      #Generate plot
      ggplot(data = df, aes(x = xtemp, y = ytemp, fill = sample)) +
        geom_point(shape = 21,
                   size = 4,
                   color = "black") +
        scale_fill_manual(values = brewercolors) +
        scale_x_continuous(
          name = "",
          breaks = c(2.5, 8.5, 15.5, 21.5, 28.5),
          limits = c(0, 30),
          labels = sort(names(brewercolors))
        ) +
        scale_y_continuous(name = "",
                           labels = c(),
                           limits = c(0, 15)) +
        ggtitle("Last drawn bootstrap sample") + 
        theme_general() +
        theme(line = element_blank(),
              legend.position  = "none")
  })
  # Render plot of distributions
  output$sampdistplot <- renderPlot({
      df <- data.frame(prop = samples$hist)
      ggplot(df, aes(x = prop)) + 
        geom_col(data = data.frame(x = (0:20)/50, y = dbinom(0:20, 50, 0.2)),
                 aes(x, y), 
                 fill = "Grey",
                 color = "Black",
                 alpha = .4,
                 width = .02) +
        geom_histogram(fill = brewercolors["Yellow"],
                     color = "Grey",
                     alpha = .6,
                     binwidth = .02,
                     aes(y = ..count../sum(..count..))) + 
        ggtitle("Proportions of yellow candies in all bootstrap samples") +
          coord_cartesian(xlim = c(0, 0.4)) +
          labs(x = "Proportion of yellow candies", y = "Probability") +
        theme_general()
  })

})
