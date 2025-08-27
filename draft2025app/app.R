library(shiny)
library(shinyjs)
library(reactable)
library(dotenv)
library(googlesheets4)
library(dplyr)

# Load environment variables
load_dot_env()
PASSWORD <- Sys.getenv("PASSWORD")

# Google Sheets setup
gs4_auth(cache = ".secrets", email = Sys.getenv("GSHEET_EMAIL"))
sheet_id <- Sys.getenv("GSHEET_ID")
df <- read_sheet(sheet_id)

# Team names
teams <- c(
  "Jeremy Patak", 
  "Austin Iske",
  "Brody Morgan",
  "Landry Sheridan",
  "Dax Davis",
  "Nick McFarland",
  "Nike Simmons",
  "Cade Palmer",
  "Stone Palmer",
  "Daniel Potichko",
  "Seth Lassiter"
  )

# Create public table
public_df <- df |>
  arrange(NomOrder) |>
  select(NomOrder, Player, Pos, NFLteam, WOFFLteam, value) |>
  filter(!is.na(WOFFLteam))

# Available Players
available_players = df |>
  filter(
    is.na(NomOrder),
    Rank != "NOM"
  ) |>
  pull(Player)

available_players = c("", available_players)

# Create team summary table
team_summary <- public_df |>
  summarise(
    players_rostered = n(),
    budget_spent = sum(value),
    .by = WOFFLteam
  ) |>
  mutate(
    budget_remaining = 200 - budget_spent,
    max_bid = budget_remaining - (15 - players_rostered - 1)
  )

team_summary <- tibble(WOFFLteam = teams) |>
  left_join(team_summary, by = "WOFFLteam") |>
  mutate(
    players_rostered = coalesce(players_rostered, 0),
    budget_remaining = coalesce(budget_remaining, 200),
    budget_spent = coalesce(budget_spent, 0),
    max_bid = coalesce(max_bid, 186)
  ) |>
  arrange(desc(players_rostered), budget_remaining)

current_nom <- df |>
  filter(Rank == "NOM") |>
  mutate(
    Player = ifelse(is.na(Player), "TBD", Player)
  ) |>
  pull(Player)

current_nom_pos <- df |>
  filter(Rank == "NOM") |>
  mutate(
    Player = ifelse(is.na(Player), "TBD", Player)
  ) |>
  pull(Pos)

current_nom_tm <- df |>
  filter(Rank == "NOM") |>
  mutate(
    Player = ifelse(is.na(Player), "TBD", Player)
  ) |>
  pull(NFLteam)

# UI for each page we will switch between
page_public <- fluidPage(
  fluidRow(
    column(
      width = 8,
      class = "col-md-8",
      reactableOutput("public_table")
    ),
    column(
      width = 4,
      class = "col-md-4",
      reactableOutput("team_summary")
    )
  )
)

page_private <- fluidPage(
  div(
    uiOutput("password_ui"),
    style = "display: flex; justify-content: center; margin-top: 50px;"
  ),
  column(
    width = 6,
    class = "col-md-6",
    uiOutput("left_protected_ui")
  ),
  column(
    width = 6,
    class = "col-md-6",
    uiOutput("right_protected_ui")
  ),
)

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
  
  #Current Nom bar
  tags$div(
    class = "nom-bar",
    HTML(
      paste0(
        "Current Nomination:<br>", 
        current_nom, 
        "<br>", current_nom_pos, ", ", current_nom_tm
      )
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
      page_public
    } else {
      page_private
    }
  })
  
  output$public_table <- renderReactable({
    reactable(
      public_df,
      defaultColDef = colDef(
        align = "center",
        headerStyle = list(background = "#ceced4")
      ),
      columns = list(
        Player = colDef(name = "Player", sortable = FALSE),
        Pos = colDef(
          name = "Position",
          style = function(value) {
            if (value == "QB") {
              color <- "#c65f8a"
            } else if (value == "RB") {
              color <- "#7cc3a5"
            } else if (value == "WR") {
              color <- "#4badd0"
            } else if (value == "TE") {
              color <- "#d58e50"
            } else if (value == "D/ST") {
              color <- "#765e3f"
            } else {
              color <- "#777"
            }
            list(color = color, fontWeight = "bold")
          }
          ),
        NomOrder = colDef(name = "Nomination Order", filterable = FALSE),
        NFLteam = colDef(
          name = "NFL Team",
          cell = function(value) {
            logos <- list(
              ARI = "https://a.espncdn.com/i/teamlogos/nfl/500/ari.png",
              ATL = "https://a.espncdn.com/i/teamlogos/nfl/500/atl.png",
              BAL = "https://a.espncdn.com/i/teamlogos/nfl/500/bal.png",
              BUF = "https://a.espncdn.com/i/teamlogos/nfl/500/buf.png",
              CAR = "https://a.espncdn.com/i/teamlogos/nfl/500/car.png",
              CHI = "https://a.espncdn.com/i/teamlogos/nfl/500/chi.png",
              CIN = "https://a.espncdn.com/i/teamlogos/nfl/500/cin.png",
              CLE = "https://a.espncdn.com/i/teamlogos/nfl/500/cle.png",
              DAL = "https://a.espncdn.com/i/teamlogos/nfl/500/dal.png",
              DEN = "https://a.espncdn.com/i/teamlogos/nfl/500/den.png",
              DET = "https://a.espncdn.com/i/teamlogos/nfl/500/det.png",
              GB  = "https://a.espncdn.com/i/teamlogos/nfl/500/gb.png",
              HOU = "https://a.espncdn.com/i/teamlogos/nfl/500/hou.png",
              IND = "https://a.espncdn.com/i/teamlogos/nfl/500/ind.png",
              JAX = "https://a.espncdn.com/i/teamlogos/nfl/500/jax.png",
              KC  = "https://a.espncdn.com/i/teamlogos/nfl/500/kc.png",
              LV  = "https://a.espncdn.com/i/teamlogos/nfl/500/lv.png",
              LAC = "https://a.espncdn.com/i/teamlogos/nfl/500/lac.png",
              LAR = "https://a.espncdn.com/i/teamlogos/nfl/500/lar.png",
              MIA = "https://a.espncdn.com/i/teamlogos/nfl/500/mia.png",
              MIN = "https://a.espncdn.com/i/teamlogos/nfl/500/min.png",
              NE  = "https://a.espncdn.com/i/teamlogos/nfl/500/ne.png",
              NO  = "https://a.espncdn.com/i/teamlogos/nfl/500/no.png",
              NYG = "https://a.espncdn.com/i/teamlogos/nfl/500/nyg.png",
              NYJ = "https://a.espncdn.com/i/teamlogos/nfl/500/nyj.png",
              PHI = "https://a.espncdn.com/i/teamlogos/nfl/500/phi.png",
              PIT = "https://a.espncdn.com/i/teamlogos/nfl/500/pit.png",
              SF  = "https://a.espncdn.com/i/teamlogos/nfl/500/sf.png",
              SEA = "https://a.espncdn.com/i/teamlogos/nfl/500/sea.png",
              TB  = "https://a.espncdn.com/i/teamlogos/nfl/500/tb.png",
              TEN = "https://a.espncdn.com/i/teamlogos/nfl/500/ten.png",
              WAS = "https://a.espncdn.com/i/teamlogos/nfl/500/wsh.png"
            )
            
            if (!is.null(logos[[value]])) {
              htmltools::div(
                style = "display: flex; align-items: center; justify-content: center; height: 100%;",
                htmltools::img(src = logos[[value]], height = "30px")
              )
            } else {
              value
            }
          }),
        value = colDef(name = "Winning Bid", filterable = FALSE, style = list(fontWeight="bold")),
        WOFFLteam = colDef(
          name = "Winning Bidder",
          style = function(value) {
            if (value == "Jeremy Patak") {
              color <- "#264fe5"
            } else if (value == "Brody Morgan") {
              color <- "#aee526"
            } else if (value == "Austin Iske") {
              color <- "#e5d926"
            } else if (value == "Landry Sheridan") {
              color <- "#4fe526"
            } else if (value == "Dax Davis") {
              color <- "#26e56a"
            } else if (value == "Nick McFarland") {
              color <- "#26e5d9"
            } else if (value == "Nike Simmons") {
              color <- "#6a26e5"
            } else if (value == "Cade Palmer") {
              color <- "#d926e5"
            } else if (value == "Stone Palmer") {
              color <- "#e526ae"
            } else if (value == "Seth Lassiter") {
              color <- "#e5264f"
            } else if (value == "Daniel Potichko") {
              color <- "#26aee5"
            } else {
              color <- "#777"
            }
            list(background = color, fontWeight = "bold", color = "white")
          }
          )
      ),
      outlined = TRUE,
      highlight = TRUE,
      filterable = TRUE,
      showPageSizeOptions = TRUE,
      pageSizeOptions = c(10, 20, 50),
      defaultPageSize = 20,
      showPagination = TRUE
    )
  })
  
  output$team_summary <- renderReactable({
    reactable(
      team_summary,
      defaultColDef = colDef(
        align = "center",
        headerStyle = list(background = "black",
                           color = "white"),
        style = list(background = "grey", color = "white", fontWeight = "bold")
      ),
      columns = list(
        WOFFLteam = colDef(name = "Team", sortable = FALSE),
        players_rostered = colDef(name = "Players Rostered"),
        budget_spent = colDef(name = "Budget Spent", show = FALSE),
        budget_remaining = colDef(name = "Budget Remaining"),
        max_bid = colDef(name = "Max Possible Bid")
      ),
      showPageSizeOptions = FALSE,
      defaultPageSize = 11,
      showPagination = FALSE
    )
  })
    
  #password entry for private page
  output$password_ui <- renderUI({
    if (is.null(input$password_box) || input$password_box != PASSWORD) {
      passwordInput("password_box", "Enter password:")
    }
  })
  
  # Show main UI only after correct password
  output$left_protected_ui <- renderUI({
    req(input$password_box)
    if (input$password_box == PASSWORD) {
      fluidRow(
        column(
          6,
          selectInput(
            "nominatedPlayer", 
            "Nominated Player", 
            choices = available_players
          )
        ),
        column(
          6,
          actionButton("submitNom", "Nominate Player") 
        )
      )
    }
  })
  
  output$right_protected_ui <- renderUI({
    req(input$password_box)
    if (input$password_box == PASSWORD) {
      reactableOutput("team_summary")
    }
  })
  
  observeEvent(input$submitNom, {
    
    # Get the nominated player from the input
    nominated_player <- input$nominatedPlayer
    
    # Info to write into NOM row
    player_info <- df |> 
      filter(Player == nominated_player) |>
      select(-Rank)
    
    # Determine which row to overwrite
    row_index <- which(df$Rank == "NOM")
    
    # Write Nominated player into the sheet
    googlesheets4::range_write(
      ss = sheet_id,
      data = player_info,
      range = paste0("B", row_index),
      col_names = TRUE
    )
    
  })

  
}

shinyApp(ui, server)
