library(shiny)
library(ggplot2)

shinyServer(function(input, output) {
  
#  source("../plottheme/styling.R", local = TRUE)
  source("styling.R", local = TRUE)
  
  n <- 4 #sample size per category
  categories <- 3 #categories
  
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
    #Dataframe containing arrow positions 
    #tot = arrows between observation + grand mean
    #grpgr = grand mean to group mean
    #grpob = group mean to observation
    dfarrows <- data.frame(x.tot = df$x,
                           xend.tot = df$x,
                           y.tot = rep(mean(df$y),length(df$y)),
                           yend.tot = df$y,
                           x.grpgr = df$x + 0.05,
                           xend.grpgr = df$x + 0.05,
                           y.grpgr = rep(mean(df$y), length(df$y)),
                           yend.grpgr = rep(as.vector(by(df$y, df$cat, mean, simplify = TRUE)),
                                            each = 4),
                           
                           x.grpob = df$x - 0.05,
                           xend.grpob = df$x - 0.05,
                           y.grpob = rep(as.vector(by(df$y, df$cat, mean, simplify = TRUE)),
                                         each = 4),
                           yend.grpob = df$y
                           )
                           

    etasqrd <- summary.lm(aov(data = df, y  ~ cat))$r.squared
    ggplot(df,aes(x = x, y = y)) +
      #Grand mean line
      geom_line(y = mean(df$y),
                aes(linetype = "Grand Mean"),
                size = 1.3) + 
      #Mean of group Autonomy
      geom_segment(x = min(subset(df, cat == "Autonomy")$x - .09),
                   xend = max(subset(df, cat == "Autonomy")$x + .09),
                   y = mean(subset(df, cat == "Autonomy")$y),
                   yend = mean(subset(df, cat == "Autonomy")$y),
                   size = 1.2,
                   aes(colour = "Autonomy")) + 
      #Mean of Group Control
      geom_segment(x = min(subset(df, cat == "Control")$x - .09),
                   xend = max(subset(df, cat == "Control")$x + .09),
                   y = mean(subset(df, cat == "Control")$y),
                   yend = mean(subset(df, cat == "Control")$y),
                   size = 1.2,
                   aes(colour = "Control")) + 
      #Mean of Group Neutral
      geom_segment(x = min(subset(df, cat == "Neutral")$x - .09),
                   xend = max(subset(df, cat == "Neutral")$x + .09),
                   y = mean(subset(df, cat == "Neutral")$y),
                   yend = mean(subset(df, cat == "Neutral")$y),
                   size = 1.2,
                   aes(colour = "Neutral")) +  
      #Arrows from grand mean to observation
      geom_segment(x = dfarrows$x.tot,
                   xend = dfarrows$xend.tot,
                   y = dfarrows$y.tot,
                   yend = dfarrows$yend.tot,
                   linetype = "dotted",
                   arrow = arrow(length = unit(2,"mm"),
                                 ends = "both",
                                 type = "closed")
                   ) +
      #Arrows from grand mean to group mean
      geom_segment(x = dfarrows$x.grpgr,
                   xend = dfarrows$xend.grpgr,
                   y = dfarrows$y.grpgr,
                   yend = dfarrows$yend.grpgr,
                   linetype = "solid",
                   arrow = arrow(length = unit(2,"mm"),
                                 ends = "both",
                                 type = "closed")) + 
      #Arrows from group to observation
      geom_segment(x = dfarrows$x.grpob,
                   xend = dfarrows$xend.grpob,
                   y = dfarrows$y.grpob,
                   yend = dfarrows$yend.grpob,
                   linetype = "solid",
                   color = "red",
                   arrow = arrow(length = unit(2,"mm"),
                                 ends = "both",
                                 type = "closed")) + 
      #Points representing observations
      geom_point(shape = 21,
                 size = 3,
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
      geom_text(label = paste("'eta'^2 ==", round(etasqrd,2)),
                x = 3,
                y = 9.5,
                parse = TRUE) +
      #Legend definitions
      guides(colour = guide_legend(title = "Group Means"),
             linetype = guide_legend(title = "Grand Mean",label = FALSE),
             fill = FALSE) + 
      #X label definitions
      scale_x_continuous(name = "", breaks = 1:3,labels = c("Autonomy", "Control", "Neutral")) +
      scale_y_continuous(name = "Vaccine Acceptance", limits = c(1, 10), breaks = 1:10) +
      theme_general() + 
      theme(legend.position = "top") 
      
  })

  
})
