library(shiny)
library(ggplot2)


# Define the server logic
shinyServer(function(input, output) {
  source("styling.R", local = TRUE)
  # Reactive expression to generate sample means and their confidence intervals
  sim_data <- reactive({
    set.seed(123)  # for reproducibility
    #mu <- input$mean  # mean from input
    mu = 3
    n <- input$sample_size  # sample size
    num_sim <- input$num_simulations  # number of simulations
    
    means <- numeric(num_sim)
    lower_ci <- numeric(num_sim)
    upper_ci <- numeric(num_sim)
    
    for (i in 1:num_sim) {
      samples <- rnorm(n, mean = mu, sd = 1)
      samp_mean <- mean(samples)
      samp_se <- sd(samples) / sqrt(n)
      means[i] <- samp_mean
      lower_ci[i] <- samp_mean - qt(0.975, df = n-1) * samp_se
      upper_ci[i] <- samp_mean + qt(0.975, df = n-1) * samp_se
    }
    
    data.frame(
      Simulation = 1:num_sim,
      Mean = means,
      LowerCI = lower_ci,
      UpperCI = upper_ci,
      InsideCI = (lower_ci <= mu & upper_ci >= mu)
    )
  })
  
  # Output the histogram of sample means
  output$histPlot <- renderPlot({
    data <- sim_data()
    ggplot(data, aes(x = Mean)) +
      #geom_histogram(bins = 30, fill = "grey", color = "black") +
      geom_histogram(aes(y = ..density..) ,binwidth = 0.02, color = "grey") + 
      #geom_vline(aes(xintercept = input$mean), color = "red", linetype = "dashed", size = 1) +
      #labs(title = "Distribution of Sample Means", x = "Sample Means", y = "Frequency")  }) +
    labs(title = "Distribution of sample mean from each data collection", x = "sample mean", y = "Density") + 
   # xlab("") +
   # ylab("") + 
    xlim(1, 5) + 
    ylim(0,10) + 
    geom_vline(xintercept = 3, color = "blue", linetype = "dashed",size = 1) +
    #stat_function(fun = dnorm, args = list(mean = input$mean, sd = 1),
    #              aes(color = "Population distribution"), size = 1) +
    stat_function(fun = dnorm, args = list(mean = 3, sd = 1/ sqrt(input$sample_size)),
                  aes(color = "Theoretical sampling distribution of sample mean"), size = 1) +
    #make the distribution green color
    scale_color_manual(values = c("Theoretical sampling distribution of sample mean" = "green")) + 
    theme_minimal() + 
    theme(
      panel.grid.major = element_blank(),  # Remove major grid lines
      panel.grid.minor = element_blank(),  # Remove minor grid lines
      #axis.text.y = element_blank(),       # Remove y-axis text/labels
      #axis.ticks.y = element_blank(), 
      legend.position = "bottom",
      legend.box = "vertical",
      legend.title = element_blank()
    ) 
  })
  
  # Output the means with CIs plot
  output$ciPlot <- renderPlot({
    data <- sim_data()
   # mu <- input$mean
    mu <- 3
    
    #total_sims <- nrow(data)
    # This formula adjusts the height so that it's inversely proportional to the number of simulations
    #bar_height <- 0.05 / sqrt(total_sims)
    
    ggplot(data, aes(y = Simulation)) +
      geom_segment(aes(x = LowerCI, xend = UpperCI, yend = Simulation, color = InsideCI), size = 1) +
      scale_color_manual(values = c("TRUE" = "green", "FALSE" = "red")) +
      geom_point(aes(x = Mean), shape = 19, size = 3, color = "black") +  # Sample means as black dots
      #geom_errorbarh(aes(xmin = LowerCI, xmax = UpperCI, height = bar_height), color = "black") +  # Short vertical bars at CI edges
      geom_vline(aes(xintercept = mu), color = "blue", linetype = "dashed", size = 1) +
      xlim(1, 5) + 
      labs(title = "95% Confidence Interval from each data collection", x = "Population mean", y = "Times of data collection") + 
      theme_minimal() +
      theme(legend.position = "none",
            panel.grid.major = element_blank(),  # Remove major grid lines
            panel.grid.minor = element_blank())  # Remove minor grid lines)
  })
  
  summary_stats <- reactive({
    data <- sim_data()  # This uses the existing sim_data reactive from your setup
    total_catches <- sum(data$InsideCI)
    total_sims <- nrow(data)
    percentage_catches <- (total_catches / total_sims) * 100
    
    list(total_catches = total_catches, total_sims = total_sims, percentage_catches = percentage_catches)
  })
  
  # Output for summary statistics
  output$summaryStats <- renderText({
    stats <- summary_stats()
    paste("Number of 95% CIs that catch the population mean:", stats$total_catches,
          "\nNumber of times of data collection:", stats$total_sims,
          "\nPercentage of 95% CIs that catch the population mean:", sprintf("%.2f%%", stats$percentage_catches))
  })
  
  # Output the population distribution plot
  output$popDistPlot <- renderPlot({
    mu = 3
    #mu <- input$mean  # mean from input
    x_vals <- seq(mu - 4, mu + 4, length.out = 100)  # generate x values around the mean
    y_vals <- dnorm(x_vals, mean = mu, sd = 1)  # calculate the density of normal distribution
    
    ggplot() +
      geom_line(aes(x = x_vals, y = y_vals), color = "red") +
      geom_vline(aes(xintercept = mu), color = "blue", linetype = "dashed", size = 1) +
      labs(title = "Population distribution", x = "Value", y = "Density") +
      xlim(1, 5) + 
      ylim(0,0.5) + 
      theme_minimal() + 
      theme(legend.position = "none",
            panel.grid.major = element_blank(),  # Remove major grid lines
            panel.grid.minor = element_blank())
  })
  
  
  # Output for specific simulation descriptions using existing sim_data
  output$simulationDetails <- renderText({
    data <- sim_data()  # Use the existing sim_data reactive
    num_sim <- nrow(data)  # Total number of simulations
    
    # Manually fetch data for first, second, and last simulation
    first_sim <- data[1, ]
    second_sim <- data[2, ]
    last_sim <- data[num_sim, ]
    
    # Generate random samples for descriptive text
    mu <- 3
    first_samples <- sample(rnorm(input$sample_size, mean = mu, sd = 1), 3)
    second_samples <- sample(rnorm(input$sample_size, mean = mu, sd = 1), 3)
    last_samples <- sample(rnorm(input$sample_size, mean = mu, sd = 1), 3)
    
    # Create descriptions manually
    first_description <- sprintf(
      "1st data collection: (%.2f, %.2f, %.2f, ...), we can calculate M = %.2f; 95%%CI = [%.2f, %.2f]",
      first_samples[1], first_samples[2], first_samples[3], first_sim$Mean, first_sim$LowerCI, first_sim$UpperCI
    )
    second_description <- sprintf(
      "2nd data collection: (%.2f, %.2f, %.2f, ...), we can calculate M = %.2f; 95%%CI = [%.2f, %.2f]",
      second_samples[1], second_samples[2], second_samples[3], second_sim$Mean, second_sim$LowerCI, second_sim$UpperCI
    )
    third_description <- sprintf(
      "3rd data collection: (%.2f, %.2f, %.2f, ...), we can calculate M = %.2f; 95%%CI = [%.2f, %.2f]",
      last_samples[1], last_samples[2], last_samples[3], last_sim$Mean, last_sim$LowerCI, last_sim$UpperCI
    )
    
    last_description <- sprintf(
      "%sth data collection: (%.2f, %.2f, %.2f, ...), we can calculate M = %.2f; 95CI = [%.2f, %.2f]",
      num_sim, last_samples[1], last_samples[2], last_samples[3], last_sim$Mean, last_sim$LowerCI, last_sim$UpperCI
    )
    
    # Combine descriptions
    if (num_sim == 1) {
      paste("Collect data from population distribution:", first_description, sep = "\n")
    } else if (num_sim == 2) {
      paste("Collect data from population distribution:", first_description, second_description, sep = "\n")
    } else if (num_sim == 3) {
      paste("Collect data from population distribution:", first_description, second_description, third_description, sep = "\n")
    } else {
      paste("Collect data from population distribution:", first_description, second_description, "...", last_description, sep = "\n")
    }
  })
  
  
  
  
})


