library(shiny)
library(ggplot2)

shinyServer(function(input, output) {
  
  source("styling.R", local = TRUE)
  
  n <- 4 #sample size per category
  start = 0.5 #minimum value of x
  end = 4 #maximum value of x

  #Function to space dots in plot
  spacemaker <-  function(categories, samples, spacing) {
    x <- numeric()
    for (i in 1:categories) {
      temp <-
        seq(i - categories / 2 * spacing, i + categories / 2 * spacing, by = spacing)
      x <- c(x, temp)
    }
    return(x)
  }
  output$mainplot <- renderPlot({
    
    #Dependance on new sample button
    input$newsampbut
    
    #Data frame containing samples of observations
    df <- isolate(
            data.frame(
              y = c(
                    input$autonomynumin - 0.7, input$autonomynumin + 1.3, 
                    input$autonomynumin - 1.1, input$autonomynumin + 0.5,
                    input$controlnumin + 0.8, input$controlnumin + 0.6, 
                    input$controlnumin - 1.2, input$controlnumin - 0.2,
                    input$neutralnumin - 0.9, input$neutralnumin + 2.1, 
                    input$neutralnumin - 0.5, input$neutralnumin - 0.7
                    ),
                    x = spacemaker(3,n,.15),
                    cat = rep(
                            c(
                              "Autonomy",
                              "Control",
                              "Neutral"
                              ),
                            each = n
                            )
            )
          )

    etasqrd <- summary.lm(aov(data = df, y  ~ cat))$r.squared
    ggplot(df,aes(x = x, y = y)) +
      #Mean of group Autonomy
      geom_segment(x = start + .09,
                   xend = end - .09,
                   y = mean(subset(df, cat == "Autonomy")$y),
                   yend = mean(subset(df, cat == "Autonomy")$y),
                   size = 1.2,
                   aes(colour = "Autonomy")) + 
      #Mean of Group Control
      geom_segment(x = start + .09,
                   xend = end - .09,
                   y = mean(subset(df, cat == "Control")$y),
                   yend = mean(subset(df, cat == "Control")$y),
                   size = 1.2,
                   aes(colour = "Control")) + 
      #Mean of Group Neutral
      geom_segment(x = start + .09,
                   xend = end - .09,
                   y = mean(subset(df, cat == "Neutral")$y),
                   yend = mean(subset(df, cat == "Neutral")$y),
                   size = 1.2,
                   aes(colour = "Neutral")) +  
      #Arrows between group means
      geom_segment(x = end - 0.6,
                   xend = end - 0.6,
                   y = mean(subset(df, cat == "Autonomy")$y),
                   yend = mean(subset(df, cat == "Control")$y),
                   linetype = "solid",
                   color = "red",
                   arrow = arrow(length = unit(2,"mm"),
                                 ends = "both",
                                 type = "closed")) + 
      geom_segment(x = end - 0.4,
                   xend = end - 0.4,
                   y = mean(subset(df, cat == "Neutral")$y),
                   yend = mean(subset(df, cat == "Control")$y),
                   linetype = "solid",
                   color = "red",
                   arrow = arrow(length = unit(2,"mm"),
                                 ends = "both",
                                 type = "closed")) + 
      geom_segment(x = end - 0.2,
                   xend = end - 0.2,
                   y = mean(subset(df, cat == "Autonomy")$y),
                   yend = mean(subset(df, cat == "Neutral")$y),
                   linetype = "solid",
                   color = "red",
                   arrow = arrow(length = unit(2,"mm"),
                                 ends = "both",
                                 type = "closed")) + 
      #Points representing observations
      geom_point(shape = 21,
                 size = 5,
                 aes(fill = cat)) +
      #Manual color definitions
      scale_colour_manual(values = c(
                                     "Autonomy" = unname(brewercolors["Orange"]),
                                     "Control" = unname(brewercolors["Blue"]),
                                     "Neutral" = unname(brewercolors["Green"])
                                      )
                          ) + 
      #Manual fill definitions
      scale_fill_manual(values = c(
                                  "Autonomy" = unname(brewercolors["Orange"]),
                                  "Control" = unname(brewercolors["Blue"]),
                                  "Neutral" = unname(brewercolors["Green"])
                                  )
                         ) + 
      #Eta squared text label
      geom_text(label = paste0("'eta'^2 == ", format(round(etasqrd,2), nsmall = 2)),
                x = end - 0.4,
                y = 9.5,
                hjust = 0.5,
                parse = TRUE) +
      #Legend definitions
      guides(colour = guide_legend(title = "Group Means:"),
             linetype = guide_legend(title = "Grand Mean:",label = FALSE),
             fill = FALSE) + 
      #X label definitions
      scale_x_continuous(breaks = 1:4,labels = c("Autonomy", "Control", "Neutral", ""), limits = c(0.5,4)) +
      scale_y_continuous(breaks = 1:10, limits = c(1,10)) +
      #coord_cartesian(ylim = c(min(df$y) - 1,max(df$y)+ 1)) +
      xlab("") + 
      ylab("Vaccine Acceptance") + 
      theme_general() + 
      theme(legend.position = "top") 
      
  })

  
})
