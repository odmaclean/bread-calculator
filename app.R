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

    # Application title
    titlePanel("Sourdough Calculator"),

    # Key parameters: flour weight and hydration percentage
    numericInput(inputId = "total_flour",
                 label = "Total flour weight (g)",
                 value = 500),
    numericInput(inputId = "hydration",
                 label = "Hydration percentage",
                 value = 75),
    br(),
    helpText("Set the percentages of flours 2 or 3 below",
             "and the weights will calculate automatically,",
             "with flour 1 as the remainder:"),
    fluidRow(
      column(4,
             h5("Flour:"),
             h4("1 (e.g. white)"),
             h4("2 (e.g. whole wheat)"),
             h4("3 (e.g. rye)"),
      ),
      column(4,
             h5("Percent:"),
             textOutput("pct_flour_1", container = div),
             numericInput(inputId = "pct_flour_2",
                          label = NULL,
                          value = 20),
             numericInput(inputId = "pct_flour_3",
                          label = NULL,
                          value = 0),
      ),
      column(4,
             h5("Weight (g):"),
             textOutput("wt_flour_1", container = div),
             textOutput("wt_flour_2", container = div),
             textOutput("wt_flour_3", container = div),
      )
    ),
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
    pct_flour_1_int <- reactive({ 100 - input$pct_flour_2 - input$pct_flour_3 })
    output$pct_flour_1 <- renderText({ pct_flour_1_int() })
    output$wt_flour_1 <- renderText({ input$total_flour * pct_flour_1_int() / 100 })
    output$wt_flour_2 <- renderText({ input$total_flour * input$pct_flour_2 / 100 })
    output$wt_flour_3 <- renderText({ input$total_flour * input$pct_flour_3 / 100 })
    
    # output$distPlot <- renderPlot({
    #     # generate bins based on input$bins from ui.R
    #     x    <- faithful[, 2]
    #     bins <- seq(min(x), max(x), length.out = input$bins + 1)
    # 
    #     # draw the histogram with the specified number of bins
    #     hist(x, breaks = bins, col = 'darkgray', border = 'white')
    # })
}

# Run the application 
shinyApp(ui = ui, server = server)
