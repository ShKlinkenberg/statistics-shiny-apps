library(shiny)
# Define the UI
fig.width = 400


 
shinyUI(
  fluidPage(
    # tags$head(
    #   tags$style(HTML("
    #       .no-space {
    #         margin-bottom: -20px;
    #       }
    #     "))
    # ),

  
  #titlePanel("Sampling Distribution and Confidence Interval Simulator"),
  sidebarLayout(
    
      sidebarPanel(
        # sliderInput("mean", "Mean of the population distribution:", 
        #             min = 1, max = 5, value = 3),
        sliderInput("num_simulations", "Number of times of data collection (replication):", 
                    min = 1, max = 100, value = 1),
        sliderInput("sample_size", "Sample size for each data collection:", 
                    min = 30, max = 300, value = 30)
      ),
    
      mainPanel(
        
          plotOutput("popDistPlot",width = fig.width, height = 150), 
          
          #verbatimTextOutput("simulationDetails"),
      
    
          uiOutput("dynamicCiPlot",width = fig.width),
          
      
      
          #verbatimTextOutput("summaryStats"),        
          plotOutput("histPlot",width = fig.width, height = 180) 
      

    
    ),
    
    
    
  )
)
)

