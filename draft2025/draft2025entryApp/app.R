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
  
  # content
  fluidRow(
    column(
      width = 5,
      class = "col-md-6",
      uiOutput("left_protected_ui")
    ),
    column(
      width = 7,
      class = "col-md-6",
      uiOutput("right_protected_ui")
    ),
  )
  
)

server <- function(input, output, session) {
  
  df <- reactive({read_sheet(sheet_id)})
  
  # Create a df to reference that doesn't have NOM row in it
  NOMless_df <- reactive({
    df() |>
      filter(Rank != "NOM")
  }) 
  
  # Create public table
  public_df <- reactive({
    NOMless_df() |>
      filter(!is.na(WOFFLteam)) |>
      arrange(desc(NomOrder)) |>
      select(NomOrder, Player, Pos, NFLteam, WOFFLteam, value)
  })
  
  # Available Players
  available_players = reactive({
    df() |>
      filter(
        NomOrder == "NULL",
        Rank != "NOM"
      ) |>
      pull(Player)
  })
  
  available_players2 = reactive({
    c("", available_players())
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
    
  
  maxNomOrder <- reactive({
    public_df() |>
      mutate(NomOrder = as.numeric(NomOrder)) |>
      summarize(
        NomOrder = max(NomOrder)
      ) |>
      pull(NomOrder)
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
  
  # Show main UI only after correct password
  output$left_protected_ui <- renderUI({
    div(
      fluidRow(
        class = "private-fluid-ui",
        column(
          1
          ),
        column(
          6,
          selectInput(
            "nominatedPlayer", 
            "Nominated Player", 
            choices = available_players2()
          )
        ),
        column(
          5,
          actionButton("submitNom", "Nominate Player") 
        )
      ),
      fluidRow(
        class = "private-fluid-ui",
        column(
          1
        ),
        column(
          7,
          selectInput(
            "winningTeam",
            "Winning Team",
            choices = teams
          )
        ),
        column(
          4,
          numericInput(
            "winningBid",
            "Winning Bid",
            value = 1, min = 1, max = 200
          )
        )
      ),
      fluidRow(
        column(8),
        column(4, actionButton("submitBid", "Submit"))
      )
    )
  })
  
  output$right_protected_ui <- renderUI({
      reactableOutput("team_summary")
  })
  
  observeEvent(input$submitNom, {
    
    # Get the nominated player from the input
    nominated_player <- input$nominatedPlayer
    
    # Info to write into NOM row
    player_info <- df() |> 
      filter(Player == nominated_player) |>
      select(-Rank)
    
    # Determine which row to overwrite
    nom_player_row <- which(df()$Rank == "NOM")
    
    # Write Nominated player into the sheet
    googlesheets4::range_write(
      ss = sheet_id,
      data = player_info,
      range = paste0("B", nom_player_row),
      col_names = TRUE
    )
    
    # Change last sheet update time
    googlesheets4::range_write(
      ss = sheet_id,
      data = data.frame(Sys.time()),
      range = paste0("G", 2),
      col_names = FALSE
    )
    
    session$reload()
    
  })
  
  observeEvent(input$submitBid, {
    
    # Determine which row to overwrite
    sold_player_row <- which(NOMless_df()$Player == current_nom()) + 1
    
    # Info to write
    sold_player_info <- NOMless_df() |> 
      filter(Player == current_nom()) |>
      mutate(
        WOFFLteam = input$winningTeam,
        value = input$winningBid,
        NomOrder = maxNomOrder() + 1
      )
    
    # Write Nominated player into the sheet
    googlesheets4::range_write(
      ss = sheet_id,
      data = sold_player_info,
      range = paste0("A", sold_player_row + 1),
      col_names = FALSE
    )
    
    # Clear the nom row
    nom_player_row <- which(df()$Rank == "NOM")
    empty_nom <- data.frame(
      Rank = "NOM",
      Player = "",
      Pos = "",
      NFLteam = ""
    )
    googlesheets4::range_write(
      ss = sheet_id,
      data = empty_nom,
      range = paste0("A", nom_player_row),
      col_names = TRUE
    )
    
    # Change last sheet update time
    googlesheets4::range_write(
      ss = sheet_id,
      data = data.frame(Sys.time()),
      range = paste0("G", 2),
      col_names = FALSE
    )
    
    session$reload()
    
  })

  
}

shinyApp(ui, server)
