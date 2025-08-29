library(shiny)
library(shinyjs)
library(reactable)
library(dotenv)
library(googlesheets4)
library(dplyr)

# Load environment variables
load_dot_env()

# Google Sheets setup
gs4_auth(cache = ".secrets", email = Sys.getenv("GSHEET_EMAIL"))
sheet_id <- Sys.getenv("GSHEET_ID")

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

ui <- fluidPage(
  
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
  ),
  
  #header
  tags$div(
    class = "header-bar",
    tags$img(src = "WhitePNG.png", class = "header-logo"),
    tags$div("2025 Auction Draft", class = "header-subtitle")
  ),
  
  uiOutput("currentNomInfo"),
  
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

server <- function(input, output, session) {
  
  df <- reactive({read_sheet(sheet_id)})
  
  # Create public table
  public_df <- reactive({
    df() |>
      arrange(desc(NomOrder)) |>
      select(NomOrder, Player, Pos, NFLteam, WOFFLteam, value) |>
      filter(!is.na(WOFFLteam))
  })
  
  # Create team summary table
  team_summary_prep <- reactive({
    public_df() |>
      summarise(
        players_rostered = n(),
        budget_spent = sum(value),
        .by = WOFFLteam
      ) |>
      mutate(
        budget_remaining = 200 - budget_spent,
        max_bid = budget_remaining - (15 - players_rostered - 1)
      )
  })
    
  team_summary <- reactive({
    tibble(WOFFLteam = teams) |>
      left_join(team_summary_prep(), by = "WOFFLteam") |>
      mutate(
        players_rostered = coalesce(players_rostered, 0),
        budget_remaining = coalesce(budget_remaining, 200),
        budget_spent = coalesce(budget_spent, 0),
        max_bid = coalesce(max_bid, 186)
      ) |>
      arrange(desc(players_rostered), budget_remaining)
  })
  
  current_nom <- reactive({
    df() |>
      filter(Rank == "NOM") |>
      mutate(
        Player = ifelse(is.na(Player), "TBD", Player)
      ) |>
      pull(Player)
  })
  
  current_nom_pos <- reactive({
    df() |>
      filter(Rank == "NOM") |>
      mutate(
        Player = ifelse(is.na(Player), "TBD", Player)
      ) |>
      pull(Pos)
  })
  
  current_nom_tm <- reactive({
    df() |>
      filter(Rank == "NOM") |>
      mutate(
        Player = ifelse(is.na(Player), "TBD", Player)
      ) |>
      pull(NFLteam)
  })
  
  output$currentNomInfo <- renderUI({
    tags$div(
      class = "nom-bar",
      HTML(
        paste0(
          "Current Nomination:<br>", 
          current_nom(), 
          "<br>", current_nom_pos(), ", ", current_nom_tm()
        )
      )
    )
  })
  
  output$public_table <- renderReactable({
    reactable(
      public_df(),
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
              Cardinals = "https://a.espncdn.com/i/teamlogos/nfl/500/ari.png",
              Falcons = "https://a.espncdn.com/i/teamlogos/nfl/500/atl.png",
              Ravens = "https://a.espncdn.com/i/teamlogos/nfl/500/bal.png",
              Bills = "https://a.espncdn.com/i/teamlogos/nfl/500/buf.png",
              Panthers = "https://a.espncdn.com/i/teamlogos/nfl/500/car.png",
              Bears = "https://a.espncdn.com/i/teamlogos/nfl/500/chi.png",
              Bengals = "https://a.espncdn.com/i/teamlogos/nfl/500/cin.png",
              Browns = "https://a.espncdn.com/i/teamlogos/nfl/500/cle.png",
              Cowboys = "https://a.espncdn.com/i/teamlogos/nfl/500/dal.png",
              Broncos = "https://a.espncdn.com/i/teamlogos/nfl/500/den.png",
              Lions = "https://a.espncdn.com/i/teamlogos/nfl/500/det.png",
              Packers  = "https://a.espncdn.com/i/teamlogos/nfl/500/gb.png",
              Texans = "https://a.espncdn.com/i/teamlogos/nfl/500/hou.png",
              Colts = "https://a.espncdn.com/i/teamlogos/nfl/500/ind.png",
              Jaguars = "https://a.espncdn.com/i/teamlogos/nfl/500/jax.png",
              Chiefs  = "https://a.espncdn.com/i/teamlogos/nfl/500/kc.png",
              Raiders  = "https://a.espncdn.com/i/teamlogos/nfl/500/lv.png",
              Chargers = "https://a.espncdn.com/i/teamlogos/nfl/500/lac.png",
              Rams = "https://a.espncdn.com/i/teamlogos/nfl/500/lar.png",
              Dolphins = "https://a.espncdn.com/i/teamlogos/nfl/500/mia.png",
              Vikings = "https://a.espncdn.com/i/teamlogos/nfl/500/min.png",
              Patriots  = "https://a.espncdn.com/i/teamlogos/nfl/500/ne.png",
              Saints  = "https://a.espncdn.com/i/teamlogos/nfl/500/no.png",
              Giants = "https://a.espncdn.com/i/teamlogos/nfl/500/nyg.png",
              Jets = "https://a.espncdn.com/i/teamlogos/nfl/500/nyj.png",
              Eagles = "https://a.espncdn.com/i/teamlogos/nfl/500/phi.png",
              Steelers = "https://a.espncdn.com/i/teamlogos/nfl/500/pit.png",
              '49ers'  = "https://a.espncdn.com/i/teamlogos/nfl/500/sf.png",
              Seahawks = "https://a.espncdn.com/i/teamlogos/nfl/500/sea.png",
              Buccaneers  = "https://a.espncdn.com/i/teamlogos/nfl/500/tb.png",
              Titans = "https://a.espncdn.com/i/teamlogos/nfl/500/ten.png",
              Commanders = "https://a.espncdn.com/i/teamlogos/nfl/500/wsh.png"
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
      team_summary(),
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

  
}

shinyApp(ui, server)
