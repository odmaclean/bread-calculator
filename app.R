#
# This is a Shiny web application to calculate ingredient weights in 
# sourdough recipes.
# 
# Author: Oliver MacLean
# Feb. 2022

# Possible to-do list
# - Add help tab
# - Add total weight tab, or even better, way to update inputs
# - Comment code
# - Add extra ingredients

library(shiny)

ui <- function(request) {
  fluidPage(
  tags$head(
    tags$style(
      HTML('
        .shiny-text-output {
          margin-left: 12px; margin-bottom: 6px;
        }
    ')
    )
  ),
  
  # Application title
  titlePanel("Sourdough Calculator"),
  
  # Key parameters: flour weight and hydration percentage
  splitLayout(
    numericInput(inputId = "total_flour",
                 label = "Total flour (g)", width = '100%',
                 value = 500),
    numericInput(inputId = "hydration",
                 label = "Hydration percent.", width = '100%',
                 value = 75),
  ),
  splitLayout(
    div(tags$label(class="control-label;", "Total dough weight (g)"),
        textOutput("wt_total")),
    bookmarkButton(),
  ),
  helpText("Percentages of flours 2 and 3 set percentage of flour 1."),
  helpText("Note: levain percentage defined by total weight / flour weight"),
  splitLayout(
    h4("Percent"),
    h4("Weight (g)")
  ),
  tags$label(class="control-label;", "Flour 1 (e.g. white)"),
  splitLayout(
    textOutput("pct_flour_1"),
    textOutput("wt_flour_1")
  ),
  splitLayout(
    numericInput(inputId = "pct_flour_2",
                 label = "Flour 2 (e.g. whole)", 
                 width = '100%', value = 20),
    div(style="margin-top: 30px;",textOutput("wt_flour_2"))
  ),
  splitLayout(
    numericInput(inputId = "pct_flour_3",
                 label = "Flour 3 (e.g rye)", width = '100%',
                 value = 5),
    div(style="margin-top: 30px;",textOutput("wt_flour_3"))
  ),
  tags$label(class="control-label;", "Water"),
  splitLayout(
    div(textOutput("pct_water")),
    div(textOutput("wt_water"))
  ),
  splitLayout(
    numericInput(inputId = "pct_levain",
                 label = "Levain", width = '100%',
                 value = 25, min = 0, max = 100),
    div(style="margin-top: 30px;",textOutput("wt_levain"))
  ),
  splitLayout(
    numericInput(inputId = "pct_salt",
                 label = "Salt", width = '100%',
                 value = 2.0, step = 0.1),
    div(style="margin-top: 30px;",textOutput("wt_salt"))
  ),
  splitLayout(
    numericInput(inputId = "pct_yeast",
                 label = "Yeast", width = '100%',
                 value = 0, step = 0.1, min = 0),
    div(style="margin-top: 34px;",textOutput("wt_yeast"))
  ),
  br(),
  h4("Levain Parameters:"),
  splitLayout(
    numericInput(inputId = "levain_hydro",
                 label = "Levain hydration", width = '100%',
                 value = 100),
    div(tags$label(class="control-label;", "Levain Flour 1"),
        div(style="margin-top: 6px;", textOutput("pct_lf_1")))
  ),
  splitLayout(
    numericInput(inputId = "pct_lf_2",
                 label = "Levain Flour 2", width = '100%',
                 value = 0),
    numericInput(inputId = "pct_lf_3",
                 label = "Levain Flour 3", width = '100%',
                 value = 0)
  )
)
}

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
  output$pct_water <- renderText({ input$hydration })
  
  output$wt_salt <- renderText({ 
    round(input$total_flour * input$pct_salt / 100, 1) })
  
  output$wt_yeast <- renderText({ 
    round(input$total_flour * input$pct_yeast / 100, 1) })
  
  output$wt_total <- renderText({ 
    round(input$total_flour * (100 + input$hydration + input $pct_salt + 
                                 input$pct_yeast) / 100, 1) })
}

# Run the application 
shinyApp(ui = ui, server = server, enableBookmarking = "url")
