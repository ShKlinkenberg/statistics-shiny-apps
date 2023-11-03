server <- function(input, output) {
  observeEvent(input$calculate, {
    MC_max <- 7
    Open_max <- 15
    MC_gues <- 1.33
    
    tentamen.cijfer <- ((input$MC_score_def - MC_gues) / (MC_max - MC_gues) * MC_max + input$Open_score_def) / (MC_max + Open_max) * 10
    
    output$result <- renderUI({
      HTML(paste(
        "<div>",
        "MC Score:", input$MC_score_def, "<br>",
        "Open Score:", input$Open_score_def, "<br>",
        "MC Guess:", MC_gues, "<br>",
        "MC Max:", MC_max, "<br>",
        "Open Max:", Open_max, "<br>",
        "Max:", MC_max+Open_max, "<br>",
        withMathJax(paste0("$$\\left[  \\frac{",
                           input$MC_score_def,
                           " - ",
                           MC_gues,
                           "}{",
                           MC_max,
                           " - ",
                           MC_gues,
                           "} \\times",
                           MC_max,
                           " + ",
                           input$Open_score_def,
                           "\\right] \\div",
                           MC_max+Open_max,
                           " \\times 10 = ",
                           round(tentamen.cijfer, 3),
                           "$$")),
        "<h3>Exam grade:", round(tentamen.cijfer, 3), "</h3>",
        "</div>")
      )
    })
  })
}
