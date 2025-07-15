library(shiny)
library(ggplot2)
library(RColorBrewer)
library(tidyverse)

shinyServer(function(input, output) {
  #Load styling files for plots
  source("styling.R", local = TRUE)
  source("simulate_vaccine.R", local = TRUE)
  
  #Reactive data frame for plotting the means
  sim_data <- reactive({
    req(
      input$autonomyhigh, input$autonomylow,
      input$controlhigh, input$controllow,
      input$neutralhigh, input$neutrallow
    )
    
    simulate_data(AutonomyHigh_avg=input$autonomyhigh,AutonomyLow_avg=input$autonomylow,
                  ControlHigh_avg=input$controlhigh,ControlLow_avg=input$controllow,
                  NeutralHigh_avg=input$neutralhigh,NeutralLow_avg=input$neutrallow, 
                  Autonomy_sd=1.66, Control_sd=1.66, Neutral_sd=1.66) 
  })

  #Calculations for p value, f value and eta^2
  stats <- reactive({df <- sim_data()
                    
                    # Execute ANOVA (via lm)
                    anova_out <- anova(lm(vacc_acceptance ~ lang_cond * health_literacy, data = df,
                                 contrasts = list(lang_cond = contr.sum, health_literacy = contr.sum)))
                    # Calculate eta2
                    ss_total <- sum(anova_out$`Sum Sq`)
                    # Return results as extended anova data.frame
                    cbind(anova_out, eta2 = c(anova_out[,2]/ss_total))
                    })
  
  #Output of results (p,f,eta) in HTML format
  output$fvaltext <- renderUI({
      helpText(
        div(HTML(paste0(
            "<b>Main effect of Language Condition:</b> F(2, 144) = ",
              rprint(stats()[1,4]), ", ",
              pprint(stats()[1,5]),
              ", eta<sup>2</sup> = ",
              rprint(stats()[1,6]),
            "<br><b>Main effect of Health Literacy:</b> F(1, 144) = ",
              rprint(stats()[2,4]), ", ",
              pprint(stats()[2,5]),
              ", eta<sup>2</sup> = ",
              rprint(stats()[2,6]),
            "<br><b>Interaction effect:</b> F(2, 144) = ",
              rprint(stats()[3,4]), ", ",
              pprint(stats()[3,5]),
              ", eta<sup>2</sup> = ",
              rprint(stats()[3,6])
        )
        )
        )
      )
  })
  
  ##MAIN PLOT##
  output$mainplot <- renderPlot({
    #Load data
    df <- sim_data()
   
     #Validation block to check if input within range of 0 and 10
    validate(
      need(!(input$neutralhigh < 0 || input$neutralhigh > 10),
            "Please use values between 0 and 10"),
      need(!(input$autonomyhigh < 0 || input$autonomyhigh > 10),
           "Please use values between 0 and 10"),
      need(!(input$controlhigh < 0 || input$controlhigh > 10),
           "Please use values between 0 and 10"),
      need(!(input$neutrallow < 0 || input$neutrallow > 10),
           "Please use values between 0 and 10"),
      need(!(input$autonomylow < 0 || input$autonomylow > 10),
           "Please use values between 0 and 10"),
      need(!(input$controllow < 0 || input$controllow > 10),
           "Please use values between 0 and 10")
      )
    
    df_av=df %>%
      group_by(lang_cond,health_literacy) %>%
      summarise(
        accept_av = mean(vacc_acceptance),
        se = sd(vacc_acceptance) / sqrt(n()),
        lower = accept_av - qt(0.975, df = n()-1) * se,
        upper = accept_av + qt(0.975, df = n()-1) * se,
        .groups = "drop"
      ) %>%
      mutate(
        const = "line_group",  # for line connection
        lang_cond=factor(lang_cond, levels=c('Autonomy','Neutral','Control'))
      ) %>% arrange(health_literacy,lang_cond)
    
    
    #Dataframe containing vertical arrow positions 
    dfarrows <- data.frame(y = df_av$accept_av[1:3],
                           yend  = df_av$accept_av[4:6])
    #Add space at the right side.
    dfarrows$y <- ifelse(dfarrows$y > dfarrows$yend,
                         dfarrows$y - 0.2,
                         dfarrows$y + 0.2)
    dfarrows$yend <- ifelse(dfarrows$y > dfarrows$yend,
                         dfarrows$yend + 0.2,
                         dfarrows$yend - 0.2)

    #Plot output
    ggplot(df_av, aes(x = lang_cond,
                  y = accept_av,
                  group = health_literacy,
                  colour = health_literacy)) +
      #Plot the means by endorsers
      geom_point(aes(shape = health_literacy),
                 size = 3) + 
      #Connect the means with lines
      geom_line(aes(linetype = health_literacy),
                size = .7) + 
      #Add vertical arcs between subgroup means
      geom_segment(x = 0.8,
                   xend = 0.8,
                   y = dfarrows$y[1],
                   yend = dfarrows$yend[1],
                   linetype = "solid",
                   color = brewercolors["Green"],
                   arrow = arrow(length = unit(2,"mm"),
                                 ends = "last",
                                 type = "closed")) + 
      geom_segment(x = 1.8,
                   xend = 1.8,
                   y = dfarrows$y[2],
                   yend = dfarrows$yend[2],
                   linetype = "solid",
                   color = brewercolors["Green"],
                   arrow = arrow(length = unit(2,"mm"),
                                 ends = "last",
                                 type = "closed")) + 
      geom_segment(x = 3.2,
                   xend = 3.2,
                   y = dfarrows$y[3],
                   yend = dfarrows$yend[3],
                   linetype = "solid",
                   color = brewercolors["Green"],
                   arrow = arrow(length = unit(2,"mm"),
                                 ends = "last",
                                 type = "closed")) + 
      # Horizontal lines for arrows
      geom_segment(aes(x = 0.7, xend = 1.3, y = accept_av[[1]], yend = accept_av[[1]]),
                   linetype = "dashed", color = brewercolors["Green"]) +
      geom_segment(aes(x = 0.7, xend = 1.3, y = accept_av[[4]], yend = accept_av[[4]]),
                   linetype = "dashed", color = brewercolors["Green"]) +
      geom_segment(aes(x = 1.7, xend = 2.3, y = accept_av[[2]], yend = accept_av[[2]]),
                   linetype = "dashed", color = brewercolors["Green"]) +
      geom_segment(aes(x = 1.7, xend = 2.3, y = accept_av[[5]], yend = accept_av[[5]]),
                   linetype = "dashed", color = brewercolors["Green"]) +
      geom_segment(aes(x = 2.7, xend = 3.3, y = accept_av[[3]], yend = accept_av[[3]]),
                   linetype = "dashed", color = brewercolors["Green"]) +
      geom_segment(aes(x = 2.7, xend = 3.3, y = accept_av[[6]], yend = accept_av[[6]]),
                   linetype = "dashed", color = brewercolors["Green"]) +
      #Set limits of plot
      scale_y_continuous(limits = c(0,10),
                         breaks = seq(1,10)) +
      #Set colours per condition
      scale_color_manual(values = c(unname(brewercolors["Red"]),
                                    unname(brewercolors["Blue"]))) + 
      #Labels and title
      labs(x='Language Condition', y='Average Vaccine Acceptance',shape = "Health Literacy", color = "Health Literacy", linetype = "Health Literacy") + 
      ggtitle("Average vaccine acceptance by condition") + 
      #Theme settings
      theme_general() + 
      theme(legend.position = "bottom", text = element_text(size = 16))+
      # Error bars
      geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.1) 
  })
  
  #Output for total of Neutral
  output$totneuttext <- renderText(
    as.character(
      round(
        mean(
          c(input$neutralhigh, input$neutrallow)
        ),
        digits = 2
      )
    )
  )
  #Output for total autonomy
  output$totauttext <- renderText(
    as.character(
      round(
        mean(
          c(input$autonomyhigh, input$autonomylow)
        ),
        digits = 2
      )
    )
  )
  #Output for total control
  output$totconttext <- renderText(
    as.character(
      round(
        mean(
          c(
            input$controlhigh, input$controllow)
        ),
        digits = 2
      )
    )
  )
  #Output for total high literacy
  output$tothightext <- renderText(
    as.character(
      round(
        mean(c(input$neutralhigh,
               input$autonomyhigh,
               input$controlhigh)
        ),
        digits = 2
      )
    )
  )
  #Output for total low literacy
  output$totlowtext <- renderText(
    as.character(
      round(
        mean(
          c(input$neutrallow,
            input$autonomylow,
            input$controllow)
        ),
        digits = 2
        
      )  
    )
  )
  #Output for total of all cells
  output$tottext <- renderText(
    as.character(
      round(
        mean(
          c(input$neutrallow,
            input$autonomylow,
            input$controllow,
            input$neutralhigh,
            input$autonomyhigh,
            input$controlhigh)
        ),
        digits = 2
      )
    )
  )
})
