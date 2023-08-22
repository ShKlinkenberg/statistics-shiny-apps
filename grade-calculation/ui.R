library(shiny)

ui <- fluidPage(
  # titlePanel("Calculate Tentamen Cijfer"),
  withMathJax(),
  div(style = "display: flex; justify-content: space-between;",
      div(style = "width: 50%;",
          sidebarLayout(
            sidebarPanel(
              numericInput("MC_score_def", "Enter MC Score Def:", value = 7, min = 0, max = 10),
              numericInput("Open_score_def", "Enter Open Score Def:", value = 11, min = 0, max = 15),
              numericInput("MC_gues", "Enter MC Guess:", value = 2.38, min = 0, max = 10),
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
