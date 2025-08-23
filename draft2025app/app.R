library(shiny)
library(shinyjs)
library(reactable)
library(dotenv)
library(googlesheets4)

# Load environment variables
load_dot_env()
PASSWORD <- Sys.getenv("PASSWORD")

# Google Sheets setup
gs4_auth(cache = ".secrets", email = Sys.getenv("GSHEET_EMAIL"))
sheet_id <- Sys.getenv("GSHEET_ID")

ui <- fluidPage(
  
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
  ),
  
  #header
  tags$div(
    class = "header-bar",
    tags$img(src = "WhitePNG.png", class = "header-logo"),
    tags$div("2025 Auction Draft", class = "header-subtitle"),
    actionButton(
      "toggle_page", 
      label = NULL, 
      icon = icon("exchange-alt"),
      class = "exchange-button"
    )
  ),
  
  uiOutput("page_ui")
  
)

server <- function(input, output, session) {
  
  current_page <- reactiveVal("public")
  
  observeEvent(input$toggle_page, {
    if(current_page() == "public") current_page("private") else current_page("public")
  })
  
  output$page_ui <- renderUI({
    if(current_page() == "public") {
      h3("Public content here")
    } else {
      h3("Private content here")
    }
  })
  
  
}

shinyApp(ui, server)
