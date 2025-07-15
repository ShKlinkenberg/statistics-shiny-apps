library(shiny)
fig.width = 360
fig.height = 300
shinyUI(
  fluidPage(
    #CSS Styling for table vjust to be in middle
    tags$style(
      HTML(
        ".table > tbody > tr > td {
          vertical-align: middle;
        }
        .table > tbody > tr > th {
          text-align: center; 
        }
        " 
      )
    ),
    
    verticalLayout(
      fluidRow(
        column(6,
        align = "center",
        plotOutput("mainplot",
                   width = fig.width,
                   height = fig.height)
      ),
      column(6,
        align = "center",
        
        tags$table(
          style="width: auto;",
          class="table: table-condensed",
          tags$tr(
            tags$th(""),
            tags$th("Autonomy"),
            tags$th("Control"),
            tags$th("Neutral"),
            tags$th("Total:")
          ),
          tags$tr(
            tags$td(
              "High Literacy"
            ),
            tags$td(
              numericInput("autonomyhigh",
                           label = "",
                           value = 6.5,
                           min = 0,
                           max = 10,
                           width = 60,
                           step = .5
              )            
            ),
            tags$td(
              numericInput("controlhigh",
                           label = "",
                           value = 8.5,
                           min = 0,
                           max = 10,
                           width = 60,
                           step = .5
              )            
            ),
            tags$td(
              numericInput("neutralhigh",
                           label = "",
                           value = 4.5,
                           min = 0,
                           max = 10,
                           width = 60,
                           step = .5
              )
            ),
            tags$td(
              textOutput("tothightext")  
            )
          ),
          tags$tr(
            tags$td(
              "Low Literacy"
            ),
            tags$td(
              numericInput("autonomylow",
                           label = "",
                           value = 5,
                           min = 0,
                           max = 10,
                           width = 60,
                           step = .5
              )
            ),
            tags$td(
              numericInput("controllow",
                           label = "",
                           value = 7,
                           min = 0,
                           max = 10,
                           width = 60,
                           step = .5
              )
            ),
            tags$td(
              numericInput("neutrallow",
                           label = "",
                           value = 3,
                           min = 0,
                           max = 10,
                           width = 60,
                           step = .5
              )             
            ),
            tags$td(
              textOutput("totlowtext") 
            )
          ),
          tags$tr(
            tags$td(
              strong("Total:")
            ),
            tags$td(
              textOutput("totauttext")
            ),
            tags$td(
              textOutput("totconttext")
            ),
            tags$td(
              textOutput("totneuttext")
            ),
            tags$td(textOutput("tottext"))
          ) 
        )
      )
    ),
    fluidRow(align = "center",
             uiOutput("fvaltext")
    )
  )
)
)
