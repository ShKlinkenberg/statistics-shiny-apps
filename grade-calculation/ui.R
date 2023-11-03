library(shiny)

ui <- fluidPage(
  # titlePanel("Calculate Tentamen Cijfer"),
  withMathJax(),
  div(style = "display: flex; justify-content: space-between;",
      div(style = "width: 50%;",
          sidebarLayout(
            sidebarPanel(
              numericInput("MC_score_def", "Enter MC Score:", value = NA, min = 0, max = 7),
              numericInput("Open_score_def", "Enter Open Score:", value = NA, min = 0, max = 15),
              actionButton("calculate", "Calculate")
            ),
            mainPanel()
          )
      ),
      div(style = "width: 50%;", class = "col-sm-4",
          htmlOutput("result")
      )
  )
)
