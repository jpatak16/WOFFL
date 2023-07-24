library(shiny)






ui = navbarPage("WOFFL Portal", fluid = TRUE,
                tabPanel("Simulate the Season!",
                         fluidRow(column(9, h1(span("WOFFL Portal", style = 'color:#580515;')), 
                                         h1(span("Simulate the Season!", style = 'font-size: 60px; font-weight: bold;'))),
                                  column(3, img(src="MaroonPNG.png", height = 180, width = 280))),
                         fluidRow(column(12, actionButton("StS", "Simulate The Season!")))
                         ) #end of StS! tabPanel
                ) #end of navbarPage

# Define server logic required to draw a histogram
server = function(input, output) {

}

shinyApp(ui, server)
