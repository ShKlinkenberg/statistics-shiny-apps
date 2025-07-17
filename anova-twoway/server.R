library(shiny)
library(ggplot2)

shinyServer(function(input, output) {
  
  source("styling.R", local = TRUE)
  
  n <- 4 # Sample size per category.
  categories <- 3 # no. of categories.
  
  # Function needed to space dots nicely in the plot
  
  spacemaker <-  function(categories, samples, spacing) {
    x <- numeric()
    for (i in 1:categories) {
      temp <-
        seq(i - categories / 2 * spacing, i + categories / 2 * spacing, by = spacing)
      x <- c(x, temp)
    }
    return(x)
  }
  
  samples <-
    reactive({
      # Samples is dependent on the new sample button.
      
      if ( input$newsampbut == 0 ) {
        #start data set with larger differences for endorser than for sex
        df <- data.frame(
          y = c(
            6 + rnorm(1, -0.4, sd = 0.6),
            6 + rnorm(1, -0.4, sd = 0.6),
            6 + rnorm(1, 0.4, sd = 0.6),
            6 + rnorm(1, 0.4, sd = 0.6),
            7 + rnorm(1, -0.4, sd = 0.6),
            7 + rnorm(1, -0.4, sd = 0.6),
            7 + rnorm(1, 0.4, sd = 0.6),
            7 + rnorm(1, 0.4, sd = 0.6),
            3 + rnorm(1, -0.4, sd = 0.6),
            3 + rnorm(1, -0.4, sd = 0.6),
            3 + rnorm(1, 0.4, sd = 0.6),
            3 + rnorm(1, 0.4, sd = 0.6)),
          x = spacemaker(3, n, .15),
          cat1 = rep(c("Autonomy","Control","Neutral"),each = n),
          cat2 = rep(c("Low","High"),each = 2))
      }
      
      else {
        
        # Define the means for each category.
        
        mCloon <- runif(1, min = 3, max = 7)
        mJolie <- runif(1, min = 3, max = 7)
        mNoEnd <- runif(1, min = 3, max = 7)
        mMen <-   runif(1, min = -1, max = 1)
        mWom <-   runif(1, min = -1, max = 1)
        
        # Generate the data frame needed for plotting the positions of each dot
        # variable y is the mean of each endorser category mixed with a random 
        # bit for the sex category with sd of 1. 
        
        df <-
          data.frame(
            y = c(
              mAutonomy + rnorm(1, mLow, sd = 0.6),
              mAutonomy + rnorm(1, mLow, sd = 0.6),
              mAutonomy + rnorm(1, mHigh, sd = 0.6),
              mAutonomy + rnorm(1, mHigh, sd = 0.6),
              mControl + rnorm(1, mLow, sd = 0.6),
              mControl + rnorm(1, mLow, sd = 0.6),
              mControl + rnorm(1, mHigh, sd = 0.6),
              mControl + rnorm(1, mHigh, sd = 0.6),
              mNeutral + rnorm(1, mLow, sd = 0.6),
              mNeutral + rnorm(1, mLow, sd = 0.6),
              mNeutral + rnorm(1, mHigh, sd = 0.6),
              mNeutral + rnorm(1, mHigh, sd = 0.6)),
            x = spacemaker(3, n, .15),
            cat1 = rep(c("Autonomy","Control","Neutral"),each = n),
            cat2 = rep(c("Low","High"),each = 2))
      }  
      return(df)
    })
  
  # MAIN PLOT ----
  
  output$mainplot <- renderPlot({
    
    df <- samples()
    
    # Plot With No Selection ----
    p <-
      ggplot(df, aes(x = x, y = y)) +
      
      geom_line(y = mean(df$y),
                aes(linetype = "Grand Mean"), colour = "black",
                size = 1.3) +
      geom_point(size = 5, aes(fill = cat1, shape = cat2)) +
      scale_colour_manual(values = 
                            c("Autonomy" = unname(brewercolors["Orange"]),
                              "Control" = unname(brewercolors["Blue"]),
                              "Neutral" = unname(brewercolors["Green"]))) +
      scale_fill_manual(values = 
                          c("Autonomy" = unname(brewercolors["Orange"]),
                            "Control" = unname(brewercolors["Blue"]),
                            "Neutral" = unname(brewercolors["Green"]))) +
      scale_shape_manual(values = c("Low" = 21, "High" = 22)) +
      scale_linetype_manual(values = c("Grand Mean" = "solid")) +
      guides(colour = "none", # guide_legend(title = "Means:"),
             linetype = guide_legend(title = "Means:"),
             fill = "none",
             shape = guide_legend(title = "Health Literacy:")) +
      scale_x_continuous(breaks = 1:3,
                         labels = c("Autonomy", "Control", "Neutral")) +
      coord_cartesian(ylim = c(min(df$y) - 1, max(df$y) + 1)) +
      xlab("") +
      ylab("Vaccine Acceptance") +
      theme_general() +
      theme(legend.position = "top")
    
    # Plot With only Endorser ----
    
    if ("1" %in% input$groupselect & length(input$groupselect) == 1)
    {
      
      # Create data frame for arrows between grand mean and group means.
      
      dfarrows <- data.frame(
        x.grpgr    = df$x ,
        xend.grpgr = df$x,
        y.grpgr    = rep(mean(df$y), length(df$y)),
        yend.grpgr = 
          rep(as.vector(by(df$y, df$cat1, mean, simplify = TRUE)), each =  4))
      
      p <-
        p +
        geom_segment( x    = min(subset(df, cat1 == "Autonomy")$x - .09),
                      xend = max(subset(df, cat1 == "Autonomy")$x + .09),
                      y    = mean(subset(df, cat1 == "Autonomy")$y),
                      yend = mean(subset(df, cat1 == "Autonomy")$y),
                      size = 1.2,
                      aes(colour = "Autonomy",linetype = "Grand Mean")) +
        geom_segment( x    = min(subset(df, cat1 == "Control")$x - .09),
                      xend = max(subset(df, cat1 == "Control")$x + .09),
                      y    = mean(subset(df, cat1 == "Control")$y),
                      yend = mean(subset(df, cat1 == "Control")$y),
                      size = 1.2,
                      aes(colour = "Control",linetype = "Grand Mean")) +
        geom_segment( x    = min(subset(df, cat1 == "Neutral")$x - .09),
                      xend = max(subset(df, cat1 == "Neutral")$x + .09),
                      y    = mean(subset(df, cat1 == "Neutral")$y),
                      yend = mean(subset(df, cat1 == "Neutral")$y),
                      size = 1.2,
                      aes(colour = "Neutral",linetype = "Grand Mean"))  +
        geom_segment( x        = dfarrows$x.grpgr,
                      xend     = dfarrows$xend.grpgr,
                      y        = dfarrows$y.grpgr,
                      yend     = dfarrows$yend.grpgr,
                      linetype = "solid",
                      colour   = "grey",
                      arrow    = 
                        arrow(length = unit(2, "mm"),
                              ends   = "both",
                              type   = "closed")) + 
        guides(colour = guide_legend(title = ""),
               linetype = guide_legend(title = "Means:"),
               fill = "none",
               shape = "none") + 
        scale_linetype_manual( values = c("Grand Mean"  = "solid"))  
    }
    
    #Plot With Only Health Literacy Selected ----
    
    if("2" %in% input$groupselect & length(input$groupselect) == 1)
    {
      
      # Data frame for arrows between grand and group means.
      
      dfarrows <-
        data.frame( x.grpgr    = df$x ,
                    xend.grpgr = df$x,
                    y.grpgr    = rep(mean(df$y), length(df$y)),
                    yend.grpgr = 
                      rep(as.vector(by(df$y, df$cat2, mean, simplify = TRUE)),3, each = 2)
        )
      
      p <-
        p +
        geom_segment( x    = min(df$x),
                      xend = max(df$x),
                      y    = mean(subset(df, cat2 == "Low")$y),
                      yend = mean(subset(df, cat2 == "Low")$y),
                      aes(linetype = "Low")) +
        geom_segment( x    = min(df$x),
                      xend = max(df$x),
                      y    = mean(subset(df, cat2 == "High")$y),
                      yend = mean(subset(df, cat2 == "High")$y),
                      aes(linetype = "High")) +
        scale_linetype_manual(values = c("Grand Mean" = "solid",
                                         "Low"        = "dotted",
                                         "High"      = "dashed")) +
        guides( colour       = "none",
                linetype     = guide_legend(title = "Means:",
                                            order = 2,
                                            override.aes = list(
                                              linetype = 
                                                c("solid", "dotted", "dashed"),
                                              colour = 
                                                c("black", "black", "black"),
                                              size = 0.5)),
                fill         = "none",
                shape        = guide_legend(title = "Health Literacy:", order = 1)) +
        geom_segment( x        = dfarrows$x.grpgr,
                      xend     = dfarrows$xend.grpgr,
                      y        = dfarrows$y.grpgr,
                      yend     = dfarrows$yend.grpgr,
                      linetype = "solid",
                      colour   = "grey",
                      arrow    = arrow( length = unit(2, "mm"),
                                        ends   = "both",
                                        type   = "closed"))
    }
    
    # Plot With Language Condition and Health Literacy Selected ----
    
    if(length(input$groupselect) == 2)
    {
      
      meandf <-
        data.frame(
          x    = as.vector(by(df$x,INDICES = list(df$cat1,df$cat2),min) - 0.1), 
          xend = as.vector(by(df$x,INDICES = list(df$cat1,df$cat2),max) + 0.1),
          y    = as.vector(by(df$y,INDICES = list(df$cat1,df$cat2),mean)),
          cat1 = unique(df$cat1),
          cat2 = rep(unique(df$cat2),1,each = 3))
      
      meandf <- meandf[order(meandf$cat1,meandf$cat2),]
      
      dfarrows <-
        data.frame(x.grpgr    = df$x ,
                   xend.grpgr = df$x,
                   y.grpgr    = rep(mean(df$y), length(df$y)),
                   yend.grpgr = rep(meandf$y, 1, each = 2)) 
      
      p <-
        p +
        geom_segment(data = meandf,
                     aes(x        = x,
                         xend     = xend,
                         y        = y,
                         yend     = y,
                         colour   = cat1,
                         linetype =  cat2),
                     size = 1) +
        scale_linetype_manual(values = c("Grand Mean" = "solid",
                                         "Men" = "dotted",
                                         "Women" = "dashed")) +
        guides(colour = guide_legend(title = "Means:"),
               linetype = guide_legend(title = ""),
               fill = "none",
               shape = "none") + 
        geom_segment(x = dfarrows$x.grpgr,
                     xend = dfarrows$xend.grpgr,
                     y = dfarrows$y.grpgr,
                     yend = dfarrows$yend.grpgr,
                     linetype = "solid",
                     colour   = "grey",
                     arrow = arrow(
                       length = unit(2, "mm"),
                       ends = "both",
                       type = "closed"))
    }
    
    # Print the plot 
    p
    
  })
})