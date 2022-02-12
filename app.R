#
# This is a Shiny web application to calculate ingredient weights in 
# sourdough recipes.
# 
# Author: Oliver MacLean
# Feb. 2022

# Possible to-do list
# - Make certain inputs/outputs mutually dependent.
# - Add extra ingredient option

library(shiny)

ui <- function(request) {
  navbarPage(title = "Sourdough calculator",
             doughCalcPanel,
             ddtPanel,
             helpPanel)
}

doughCalcPanel <- tabPanel(
  title = "Recipe",
  
  ## Content:
  # Specify CSS for output text
  tags$head(
    tags$style(
      HTML('
        .shiny-text-output {
          margin-left: 12px; margin-bottom: 6px;
        }
    ')
    )
  ),
  
  # Key parameters: flour weight and hydration percentage
  splitLayout(
    numericInput(inputId = "total_flour",
                 label = "Total flour (g)", width = '100%',
                 value = 500),
    numericInput(inputId = "hydration",
                 label = "Hydration percent.", width = '100%',
                 value = 75),
  ),
  
  # Output dough weight, plus bookmarking button
  splitLayout(
    div(tags$label(class="control-label;", "Total dough weight (g)"),
        textOutput("wt_total")),
    bookmarkButton(),
  ),
  
  # Big table with ingredient percents and weights for dough
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
  
  # Smaller table to define levain composition
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

# Next tab: Desired Dough Temperature Calculator
ddtPanel <- tabPanel(
  title = "DDT",
  
  # Specify CSS for output text, again
  tags$head(
    tags$style(
      HTML('
        .shiny-text-output {
          margin-left: 12px; margin-bottom: 6px;
        }
    ')
    )
  ),
  # All the inputs for temperature and recipe parameters:
  splitLayout(
    numericInput(inputId = "ideal_T",
                 label = "Ideal temperature", width = '100%',
                 value = 78),
    numericInput(inputId = "room_T",
                 label = "Room temperature", width = '100%',
                 value = 72)
  ),
  splitLayout(
    numericInput(inputId = "flour_T",
                 label = "Flour temperature", width = '100%',
                 value = 72),
    numericInput(inputId = "fric_fac",
                 label = "Friction factor", width = '100%',
                 value = 5, step = 0.5, min = 0, max = 50)
  ),
  splitLayout(
    numericInput(inputId = "levain_T",
                 label = "Preferment temperature (°F)",
                 width = '100%',
                 value = 72),
    numericInput(inputId = "levain_pct_ddt",
                 label = "Preferment percent (%)",
                 width = '100%',
                 value = 20, min = 0)
  ),
  
  # Output the water temperature to use
  div(tags$label(class="control-label;", "Final water temperature"),
      textOutput("water_T"))
)

helpPanel <- tabPanel(
  title = "Help",
  
  # A bunch of explanatory text
  helpText("Recipe:"),
  p("To build a recipe, choose the total flour weight and hydration, then the 
    percentage of additional flours you would like, and the baker's percent of 
    levain, salt, and yeast (if any, or use this for additional ingredient)."),
  p("The  flours 2 and 3 set percentage of flour 1, and the levain percent 
    is defined in baker's percent (total weight / total flour weight)."),
  p("The levain hydration and flour composition can be set by the fields at the 
    bottom. The composition could be significant if you feed your levain a 
    different flour than the main one in your dough (flour 1 here is the same as
    the above, and so on.)"),
  p("For a yeast-leavened recipe, just set the levain percentage to zero."),
  p("The weight of water in the recipe is set by the hydration and the levain 
    percent and its hydration. To adjust the water weight, adjust the hydration 
    value."),
  p("The total dough weight is determined mainly by the flour weight and 
    hydration, so to target a different total weight, adjust the flour. (In 
    future I may try to make it possible to set either.)"),
  helpText("Desired dough temperature (DDT)"),
  p("This calculator allows you to determine the water temperature needed to 
    have your dough at the ideal temperature after mixing. It's an implementation
    of the formula given by Wordloaf: ", 
    tags$a(href="https://wordloaf.substack.com/p/class-time-desired-dough-temperature",
           "click here for more information.")),
  p("The ideal temperature is 78 °F (25.5 °C) for sourdough and 75 °F (24 °C) for
    yeast doughs."),
  p("The Friction Factor depends on your mixing method and is about 5 °F for 
  hand mixing, 25 °F for a stand mixer, and 30 °F for a food processor (or 3, 14, 
    and 17 °C, respectively). The initial values are for °F, but I think it 
    should work in Celsius with the right friction factor."),
  helpText("About: "),
  p("This calculator was built by Oliver MacLean. For feedback or suggestions, 
    contact me at oliverdmaclean@gmail.com")
)

server <- function(input, output) {
  
  ## Recipe math and outputs
  # Calculate and output main flour percent, minus minor ones
  pct_flour_calc <- reactive(
    { 100 - input$pct_flour_2 - input$pct_flour_3 }
  )
  output$pct_flour_1 <- renderText({ pct_flour_calc() })
  
  # Output flour weights after accounting for flour in levain
  output$wt_flour_1 <- renderText({ 
    round(input$total_flour * pct_flour_calc() / 100 - lf_1())
  })
  output$wt_flour_2 <- renderText({ 
    round(input$total_flour * input$pct_flour_2 / 100 - lf_2()) })
  output$wt_flour_3 <- renderText({ 
    round(input$total_flour * input$pct_flour_3 / 100 - lf_3()) })
  
  ## Levain math
  # Calculate and output levain weight
  wt_levain_calc <- reactive(
    { input$total_flour * input$pct_levain / 100 }
  )
  output$wt_levain <- renderText( { wt_levain_calc() } )
  
  # Levain total flour and water weights
  levain_flour_tot <- reactive(
    { wt_levain_calc() / ( 1 + input$levain_hydro / 100 )}
  )
  levain_water <- reactive( { wt_levain_calc() - levain_flour_tot() } )
  
  # Levain component weights
  pct_lf_1_calc <- reactive({ 100 - input$pct_lf_2 - input$pct_lf_3 })
  lf_1 <- reactive({ levain_flour_tot() * pct_lf_1_calc() / 100 })
  lf_2 <- reactive({ levain_flour_tot() * input$pct_lf_2 / 100 })
  lf_3 <- reactive({ levain_flour_tot() * input$pct_lf_3 / 100 })
  output$pct_lf_1 <- renderText({ pct_lf_1_calc() })
  
  # Now calculate water weight from hydration and levain percent
  # Also output the hydration again in table, to fill spot
  wt_water_calc <- reactive(
    { round(input$total_flour * input$hydration / 100 - levain_water()) }
  )
  output$wt_water <- renderText({ wt_water_calc() })
  output$pct_water <- renderText({ input$hydration })
  
  # Output salt, yeast, and total dough weight (at top)
  output$wt_salt <- renderText({ 
    round(input$total_flour * input$pct_salt / 100, 1) })
  
  output$wt_yeast <- renderText({ 
    round(input$total_flour * input$pct_yeast / 100, 1) })
  
  output$wt_total <- renderText({ 
    round(input$total_flour * (100 + input$hydration + input$pct_salt + 
                                 input$pct_yeast) / 100, 1) })
  
  ## DDT math and output
  output$water_T <- renderText({
    # Each line corresponds to A/B/C/D value on Wordloaf guide (linked in Help)
    round(
      input$ideal_T - input$flour_T 
      + input$ideal_T - input$room_T
      + input$ideal_T - input$fric_fac
      + (input$ideal_T - input$levain_T) * input$levain_pct_ddt / 100,
      1
    )
  })
}

# Run the application 
shinyApp(ui = ui, server = server, enableBookmarking = "url")
