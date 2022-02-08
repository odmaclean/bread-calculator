#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
  # tags$head(
  #   tags$style(
  #     HTML('
  #       #uploadfile{height: 70px}
  #       #rat{height: 60px}
  #       #ratio{height: 60px}
  #       #mytext{width: 50px}
  #   ')
  #   )
  # ),

    # Application title
    titlePanel("Sourdough Calculator"),

    # Key parameters: flour weight and hydration percentage
    fluidRow(
      column(4, numericInput(inputId = "total_flour",
                 label = "Total flour weight (g)",
                 value = 500)),
      column(4, numericInput(inputId = "hydration",
                 label = "Hydration percentage",
                 value = 75)),
      column(4, numericInput(inputId = "levain_hydro",
                 label = "Levain hydration percentage",
                 value = 100))
    ),
    helpText("Percentages of flours 2 or 3 set percentage of flour 1."),
    helpText("Note: levain percentage defined by total weight / flour weight"),
    fluidRow(
      column(4, h4(style="strong", "Ingredient")),
      column(4, h4("Percent")),
      column(4, h4("Weight (g)"))
    ),
    fluidRow(
      column(4, div(style="height: 30px;", "Flour 1 (e.g. white)")),
      column(4, textOutput("pct_flour_1")),
      column(4, div(textOutput("wt_flour_1")))
    ),
    fluidRow(
      column(4, div(style="margin-top: 9px;", "Flour 2 (e.g. whole wheat)")),
      column(4, numericInput(inputId = "pct_flour_2",
                             label = NULL,
                             value = 20)),
      column(4, div(style="margin-top: 9px;",textOutput("wt_flour_2")))
    ),
    fluidRow(
      column(4, div(style="margin-top: 9px;", "Flour 3 (e.g. rye)")),
      column(4, numericInput(inputId = "pct_flour_3",
                             label = NULL,
                             value = 5)),
      column(4, div(style="margin-top: 9px;",textOutput("wt_flour_3")))
    ),
    fluidRow(
      column(4, div(style="height: 30px;", "Water")),
      column(4),
      column(4, div(textOutput("wt_water")))
    ),
    fluidRow(
      column(4, div(style="margin-top: 9px;", "Levain")),
      column(4, numericInput(inputId = "pct_levain",
                             label = NULL,
                             value = 25, min = 0, max = 100)),
      column(4, div(style="margin-top: 9px;",textOutput("wt_levain")))
    ),
    fluidRow(
      column(4, div(style="margin-top: 9px;", "Salt")),
      column(4, numericInput(inputId = "pct_salt",
                             label = NULL,
                             value = 2.0, step = 0.1)),
      column(4, div(style="margin-top: 9px;",textOutput("wt_salt")))
    ),
    fluidRow(
      column(4, div(style="margin-top: 9px;", "Yeast")),
      column(4, numericInput(inputId = "pct_yeast",
                             label = NULL,
                             value = 0, step = 0.1, min = 0)),
      column(4, div(style="margin-top: 9px;",textOutput("wt_yeast")))
    ),
    br(),
    fluidRow(
      column(4, div(style="height: 30px;", "Levain Flour 1")),
      column(4, textOutput("pct_lf_1"))
    ),
    fluidRow(
      column(4, div(style="margin-top: 9px;", "Levain Flour 2")),
      column(4, numericInput(inputId = "pct_lf_2",
                             label = NULL,
                             value = 0))
    ),
    fluidRow(
      column(4, div(style="margin-top: 9px;", "Levain Flour 3")),
      column(4, numericInput(inputId = "pct_lf_3",
                             label = NULL,
                             value = 0))
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
    pct_flour_calc <- reactive(
      { 100 - input$pct_flour_2 - input$pct_flour_3 }
      )
    output$pct_flour_1 <- renderText({ pct_flour_calc() })
    output$wt_flour_1 <- renderText({ 
      round(input$total_flour * pct_flour_calc() / 100 - lf_1())
      })
    output$wt_flour_2 <- renderText({ 
      round(input$total_flour * input$pct_flour_2 / 100 - lf_2()) })
    output$wt_flour_3 <- renderText({ 
      round(input$total_flour * input$pct_flour_3 / 100 - lf_3()) })
    
    ## Levain math
    # Levain weight and output
    wt_levain_calc <- reactive(
      { input$total_flour * input$pct_levain / 100 }
    )
    output$wt_levain <- renderText( { wt_levain_calc() } )
    
    # Levain flour and water weights
    levain_flour_tot <- reactive(
      { wt_levain_calc() / ( 1 + input$levain_hydro / 100 )}
    )
    levain_water <- reactive( { wt_levain_calc() - levain_flour_tot() } )
    pct_lf_1_calc <- reactive({ 100 - input$pct_lf_2 - input$pct_lf_3 })
    lf_1 <- reactive({ levain_flour_tot() * pct_lf_1_calc() / 100 })
    lf_2 <- reactive({ levain_flour_tot() * input$pct_lf_2 / 100 })
    lf_3 <- reactive({ levain_flour_tot() * input$pct_lf_3 / 100 })
    output$pct_lf_1 <- renderText({ pct_lf_1_calc() })
    
    wt_water_calc <- reactive(
      { round(input$total_flour * input$hydration / 100 - levain_water()) }
    )
    output$wt_water <- renderText({ wt_water_calc() })
    
    output$wt_salt <- renderText({ 
      round(input$total_flour * input$pct_salt / 100, 1) })

    output$wt_yeast <- renderText({ 
      round(input$total_flour * input$pct_yeast / 100, 1) })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
