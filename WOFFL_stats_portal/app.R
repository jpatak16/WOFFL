library(shiny)
library(pacman)
p_load(dplyr, readxl, magrittr, janitor, httr, jsonlite, gt, gtExtras, tidyr)

AllGames = read.csv("AllGames.csv") %>%
  clean_names() %>%
  mutate(result = ifelse(score > opponent_score, 1, 0))

woffl_id = 313259
dat <- ffl_api(leagueId = 313259, view = c("mScoreboard", "mRoster"), seasonId = 2023)

CS = unique(AllGames$season) %>% max()
CW = dat$scoringPeriodId

week_selector_options = c("Week 1" = 1,
                          "Week 2" = 2,
                          "Week 3" = 3,
                          "Week 4" = 4,
                          "Week 5" = 5,
                          "Week 6" = 6,
                          "Week 7" = 7,
                          "Week 8" = 8,
                          "Week 9" = 9,
                          "Week 10" = 10,
                          "Week 11" = 11,
                          "Week 12" = 12,
                          "Week 13" = 13,
                          "Postseason Week 1" = 14,
                          "Postseason Week 2" = 15,
                          "Postseason Week 3" = 16)


pos_ids <- tibble::tribble(
  ~name, ~abbrev, ~slot, ~position,
  "Quarterback",                 "QB",      0,     1,
  "Team Quarterback",            "TQB",     1,     NA,
  "Running Back",                "RB",      2,     2,
  "Running Back/Wide Receiver",  "RB/WR",   3,     NA,
  "Wide Receiver",               "WR",      4,     3,
  "Wide Receiver/Tight End",     "WR/TE",   5,     NA,
  "Tight End",                   "TE",      6,     4,
  "Flex",                        "FLEX",    23,    NA,
  "Offensive Player Utility",    "OP",      7,    NA,
  "Defensive Tackle",            "DT",      8,     9,
  "Defensive End",               "DE",      9,     10,
  "Linebacker",                  "LB",      10,    11,
  "Defensive Line",              "DL",      11,    NA,
  "Cornerback",                  "CB",      12,    12,
  "Safety",                      "S",       13,    13,
  "Defensive Back",              "DB",      14,    NA,
  "Defensive Player Utility",    "DP",      15,    NA,
  "Team Defense/Special Teams",  "D/ST",    16,    16,
  "Place Kicker",                "K",       17,    5,
  "Punter",                      "P",       18,    7,
  "Head Coach",                  "HC",      19,    14,
  "Bench",                       "BE",      20,    NA ,
  "Injured Reserve",             "IR",      21,    NA
)

round2 = function(x, digits) {
  posneg = sign(x)
  z = abs(x)*10^digits
  z = z + 0.5 + sqrt(.Machine$double.eps)
  z = trunc(z)
  z = z/10^digits
  z*posneg
}
gt_merge_img_circle<-function(gt_object, col1, col2, col3, palette = c("black", "grey"), 
                              ..., small_cap = TRUE, font_size = c("14px", "10px"), font_weight = c("bold", "bold"), 
                              height = 25, border_color = "black", border_weight = 1.5) {
  
  colors <- scales::col2hcl(palette, ...)
  col1_bare <- rlang::enexpr(col1) %>% rlang::as_string()
  row_name_var <- gt_object[["_boxhead"]][["var"]][which(gt_object[["_boxhead"]][["type"]] == "stub")]
  data_in2 <- gtExtras::gt_index(gt_object, column = {{col2}})
  data_in3 <- gtExtras::gt_index(gt_object, column = {{col3}})
  
  gt_object %>% text_transform(locations = 
                                 if (isTRUE(row_name_var == col1_bare)) {cells_stub(rows = gt::everything())
    
                                   } else {cells_body(columns = {{col1}})}, 
                               fn = function(x) {
                                 if (small_cap) {font_variant <- "small-caps"
                                 } else {font_variant <- "normal"}
    
                                 glue::glue("<div style='line-height:{font_size[1]}'><div style='background-image: url({x});background-size:cover;background-position:center;background-color:white;border: {border_weight}px solid {border_color};border-radius: 50%;height:{height}px;width:100%;'></div></div>\n        
                                            <div style='line-height:{font_size[2]}'><span style ='float:left;font-weight:{font_weight[2]};color:{colors[2]};font-size:{font_size[2]}'>{data_in2}</span>
                                            <span style ='float:right;font-weight:{font_weight[2]};color:{colors[2]};font-size:{font_size[2]}'>{data_in3}</span></div>")}) %>% 
    cols_hide(columns = c({{col2}}, {{col3}}))}

ffl_api <- function(leagueId = ffl_id(), view = NULL, leagueHistory = FALSE,
                    seasonId = 2022, scoringPeriodId = NULL, ...) {
  dots <- list(..., scoringPeriodId = scoringPeriodId)
  age_path <- ifelse(
    test = isTRUE(leagueHistory),
    yes = "leagueHistory",
    no = sprintf("seasons/%i/segments/0/leagues", seasonId)
  )
  view <- as.list(view)
  names(view) <- rep("view", length(view))
  if (!is.null(names(dots))) {
    view <- c(view, dots)
  }
  try_json(
    url = "https://fantasy.espn.com",
    path = paste("apis/v3/games/ffl", age_path, leagueId, sep = "/"),
    query = view,
    leagueHistory = leagueHistory
  )
}
try_json <- function(url, path = "", query = NULL, leagueHistory = NULL) {
  resp <- httr::RETRY(
    verb = "GET",
    url = ifelse(
      test = is.null(path) | !nzchar(path),
      yes = url,
      paste(url, paste(path, collapse = "/"), sep = "/")
    ),
    query = query,
    httr::accept_json(),
    httr::user_agent("https://github.com/kiernann/fflr/"),
    terminate_on = c(400:417)
  )
  if (httr::http_type(resp) != "application/json") {
    stop("API did not return JSON", call. = FALSE)
  }
  raw <- httr::content(resp, as = "text", encoding = "UTF-8")
  parsed <- jsonlite::fromJSON(raw)
  if (httr::http_error(resp) && any(grepl("message", names(parsed)))) {
    if (!is.null(leagueHistory) && isTRUE(leagueHistory)) {
      parsed$message <- paste(parsed$message, "(No League History?)")
    }
    stop(
      sprintf(
        "ESPN Fantasy API request failed [%s]\n%s",
        httr::status_code(resp), parsed$message
      ),
      call. = FALSE
    )
  } else {
    return(parsed)
  }
}
live_scoring <- function(leagueId = ffl_id(), yetToPlay = FALSE,
                         bonusWin = FALSE, ...) {
  dat <- ffl_api(
    leagueId = 313259,
    view = c("mScoreboard", "mRoster"),
    seasonId = 2023
  )
  s <- tibble::tibble(
    matchupPeriodId = c(
      dat$schedule$matchupPeriodId,
      dat$schedule$matchupPeriodId
    ),
    matchupId = c(
      dat$schedule$id,
      dat$schedule$id
    ),
    teamId = c(
      dat$schedule$home$teamId,
      dat$schedule$away$teamId
    ),
    totalPointsLive = c(
      dat$schedule$home$totalPointsLive,
      dat$schedule$away$totalPointsLive
    ),
    totalProjectedPointsLive = c(
      dat$schedule$home$totalProjectedPointsLive,
      dat$schedule$away$totalProjectedPointsLive
    )
  ) %>% arrange(matchupPeriodId, matchupId)
  
  
  
  return(s)
}
yet_to_play <- function(){
  blank = data.frame()
  for(i in 1:11){
    finished = dat$teams$roster$entries[[i]]$playerPoolEntry$rosterLocked
    pos = dat$teams$roster$entries[[1]]$lineupSlotId
    df = data.frame(team_id = i, finished_game = finished, position_id = pos)
    blank = rbind(blank, df)
  }
  return(blank)
}

schedule = live_scoring() %>% mutate(totalPointsLive = ifelse(is.na(totalPointsLive), 0, totalPointsLive),
                                     totalProjectedPointsLive = round(totalProjectedPointsLive, digits = 2)) %>%
  left_join(rbind(data.frame(teamId = dat$schedule$away$teamId, score = dat$schedule$away$totalPoints, matchupPeriodId = rep(1:(length(dat$schedule$away$totalPoints)*6), each=6)), 
            data.frame(teamId = dat$schedule$home$teamId, score = dat$schedule$home$totalPoints, matchupPeriodId = rep(1:(length(dat$schedule$home$totalPoints)*6), each=6)))) %>%
  mutate(totalPointsLive = ifelse(is.na(totalProjectedPointsLive), score, totalPointsLive)) %>% select(-score)
team = dat$teams %>% mutate(owner = case_when(id==1 ~ "Jeremy Patak",
                                              id==2 ~ "Austin Iske",
                                              id==3 ~ "Brody Morgan",
                                              id==4 ~ "Dax Davis",
                                              id==5 ~ "Landry Sheridan",
                                              id==6 ~ "Stone Palmer",
                                              id==7 ~ "Seth Lassiter",
                                              id==8 ~ "Nick McFarland",
                                              id==9 ~ "Nike Simmons",
                                              id==10 ~ "Daniel Potichko",
                                              id==11 ~ "Cade Palmer",
                                              id==12 ~ ""))

still_playing = yet_to_play() %>% filter(position_id != 20, finished_game==F) %>% group_by(team_id) %>% summarise(still_playing = n()) %>%
  right_join(team %>% select(id), by = c("team_id" = "id")) %>% mutate(still_playing = ifelse(is.na(still_playing), 0, still_playing))

ui = navbarPage("WOFFL Portal", fluid = TRUE,
                tabPanel("Weekly Scoreboard",
                         fluidRow(column(9, h1(span("White Oak Fantasy Football League Portal", style = 'color:#8A8A8A; text-shadow: black 0.0em 0.1em 0.2em')), 
                                         h1(span("Weekly Scoreboard", style = 'font-size: 60px; font-weight: bold; color:#FFFFFF; text-shadow: black 0.0em 0.18em 0.2em'))),
                                  column(3, img(src="3d.jpg", height = 150, width = 210)),
                                  style = 'margin-top:-20px; padding-top:10px; padding-bottom:10px; background-color:#580515'),
                         fluidRow(column(12, align='center', selectInput("week", "Week Selector", week_selector_options)), 
                                  style = 'padding-top:10px;'),
                         fluidRow(column(12, align='center', uiOutput("playoffs"))),
                         fluidRow(column(6, gt_output('wk_matchup_1')),
                                  column(6, gt_output('wk_matchup_2'))),
                         fluidRow(column(6, gt_output('wk_matchup_3')),
                                  column(6, gt_output('wk_matchup_4'))),
                         fluidRow(column(6, gt_output('wk_matchup_5')),
                                  column(6, gt_output('wk_matchup_6')))
                         ) #end of WS tabPanel
                ) #end of navbarPage


server = function(input, output) {
  
  output$playoffs = renderUI(if(as.numeric(input$week) > 13){img(src="playoffs.gif", height = 300)}
                             else{img(src="playoffs.gif", height = 0)})
  
  team_record = reactive(AllGames %>% 
                           filter(season == CS, week < as.numeric(input$week)) %>%
                           mutate(wins = ifelse(score>opponent_score, 1, 0),
                                  losses = ifelse(opponent_score>score, 1, 0),
                                  ties = ifelse(opponent_score == score & score != 0 , 1, 0)) %>%
                           select(team, wins, losses, ties) %>%
                           group_by(team) %>%
                           summarise(wins = sum(wins), losses = sum(losses), ties = sum(ties)) %>%
                           right_join(team %>% select(owner, id), by = c('team'='owner')) %>%
                           mutate(across(c(wins:ties), ~ ifelse(is.na(.), 0, .))) %>%
                           mutate(id = ifelse(team == "Ghost of Dakota Frantum", 12, id)) %>%
                           mutate(record = ifelse(ties==0, paste0("(", wins, "-", losses, ")"), paste0("(", wins, "-", losses, "-", ties, ")"))) %>%
                           select(id, record))
  
  team_ov_record = reactive(AllGames %>% 
                              filter(season == CS,
                                     team != "Ghost of Dakota Frantum",
                                     week < as.numeric(5)) %>%
                              group_by(ov_wk) %>%
                              mutate(ov_wins = order(order(score, decreasing = FALSE)) - 1,
                                     ov_losses = order(order(score, decreasing = TRUE)) - 1,
                                     ov_wins = ifelse(is.na(score) | score == 0, 0, ov_wins),
                                     ov_losses = ifelse(is.na(score) | score == 0, 0, ov_losses)) %>%
                              group_by(team) %>%
                              summarise(ov_wins = sum(ov_wins), ov_losses = sum(ov_losses)) %>%
                              right_join(team %>% select(owner, id), by = c('team'='owner')) %>%
                              mutate(ov_record = paste0("(", ov_wins, "-", ov_losses, ")"),
                                     ov_record = ifelse(is.na(ov_wins), "", ov_record)) %>%
                              select(id, ov_record))
  
  ghost_matchupId = reactive(schedule %>% filter(matchupPeriodId == as.numeric(input$week),
                                                 teamId == 12) %>%
                               select(matchupId) %>% as.numeric())
  
  ghost_score = reactive(schedule %>% filter(matchupPeriodId == as.numeric(input$week),
                                             matchupId != ghost_matchupId()) %>%
                           select(totalPointsLive) %>% as.vector() %>% unlist() %>% mean() %>% round2(digits = 2))
  
  ghost_proj = reactive(schedule %>% filter(matchupPeriodId == as.numeric(input$week),
                                             matchupId != ghost_matchupId()) %>%
                           select(totalProjectedPointsLive) %>% as.vector() %>% unlist() %>% mean() %>% round2(digits = 2))
  
  m1_away = reactive(data.frame(week = as.numeric(input$week), 
                                matchup_id = seq(1, 91, by=6)[as.numeric(input$week)], 
                                h_or_a = 'Away', 
                                team_id = schedule %>% filter(matchupId == seq(1, 91, by=6)[as.numeric(input$week)]) %>% filter(row_number() == 2) %>% select(teamId) %>% as.numeric(),
                                score = schedule %>% filter(matchupId == seq(1, 91, by=6)[as.numeric(input$week)]) %>% filter(row_number() == 2) %>% select(totalPointsLive) %>% as.numeric(),
                                projection = ifelse(
                                  is.na(schedule %>% filter(matchupId == seq(1, 91, by=6)[as.numeric(input$week)]) %>% filter(row_number() == 2) %>% select(totalProjectedPointsLive) %>% as.numeric()),
                                  "",
                                  schedule %>% filter(matchupId == seq(1, 91, by=6)[as.numeric(input$week)]) %>% filter(row_number() == 2) %>% select(totalProjectedPointsLive) %>% as.numeric())))
  
  m1_home = reactive(data.frame(week = input$week, 
                                matchup_id = seq(1, 91, by=6)[input$week], 
                                h_or_a = 'Home', 
                                team_id = schedule %>% filter(matchupId == seq(1, 91, by=6)[as.numeric(input$week)]) %>% filter(row_number() == 1) %>% select(teamId) %>% as.numeric(),
                                score = schedule %>% filter(matchupId == seq(1, 91, by=6)[as.numeric(input$week)]) %>% filter(row_number() == 1) %>% select(totalPointsLive) %>% as.numeric(),
                                projection = ifelse(
                                  is.na(schedule %>% filter(matchupId == seq(1, 91, by=6)[as.numeric(input$week)]) %>% filter(row_number() == 1) %>% select(totalProjectedPointsLive) %>% as.numeric()),
                                  "",
                                  schedule %>% filter(matchupId == seq(1, 91, by=6)[as.numeric(input$week)]) %>% filter(row_number() == 1) %>% select(totalProjectedPointsLive) %>% as.numeric())))
  
  m1_table = reactive(rbind(m1_away(), m1_home()) %>%
                        mutate(score = ifelse(team_id == 12, ghost_score(), score),
                               projection = ifelse(team_id == 12 & !is.na(ghost_proj()), ghost_proj(), projection)))
  
  output$wk_matchup_1 = render_gt(if(as.numeric(input$week) > 13){}
                                  else{m1_table() %>% 
                                      left_join(team %>% mutate(id = as.double(id)) %>% 
                                                  select(id, logo, name, owner), 
                                                by = c('team_id' = 'id')) %>%
                                      left_join(still_playing, by = "team_id") %>%
                                      left_join(team_record(), by = c("team_id" = "id")) %>%
                                      left_join(team_ov_record(), by = c("team_id" = "id")) %>%
                                      mutate(still_playing = case_when(as.numeric(input$week) == CW ~ still_playing,
                                                                       as.numeric(input$week) < CW ~ 0,
                                                                       as.numeric(input$week) > CW ~ 15)) %>%
                                      mutate(projection = ifelse(still_playing == 0, "", projection)) %>% 
                                      select(logo, name, score, projection, owner, still_playing, record, ov_record) %>%
                                      mutate(win = ifelse(sum(still_playing) == 0 & score>mean(score), 1, 0)) %>%
                                      gt() %>% 
                                      gt_highlight_rows(columns = gt::everything(), rows = win > 0,
                                                        fill = "#ead89e", alpha = 0.8, font_weight = "normal") %>%
                                      gt_merge_stack(col1 = score, col2 = projection,
                                                     font_size = c('25px', '15px'),
                                                     font_weight = c('bold', 'normal')) %>%
                                      gt_merge_stack(col1 = name, col2 = owner,
                                                     font_size = c("25px", "15px"),
                                                     font_weight = c("bold", "normal")) %>%
                                      gt_merge_img_circle(col1 = logo, col2 = record, col3 = ov_record, height = 100) %>%
                                      cols_width(logo ~ px(100),
                                                 name ~ px(300),
                                                 score ~ px(120)) %>%
                                      cols_hide(columns = c(still_playing, win)) %>%
                                      tab_style(style = list(cell_text(weight = 'bold', size = 'x-large', align = 'left')),
                                                locations = cells_body(columns = name)) %>%
                                      tab_style(style = list(cell_text(weight = 'bold', size = 'xx-large', align = 'right')),
                                                locations = cells_body(columns = score)) %>%
                                      tab_style(style = "padding-left:40px",
                                                locations = cells_body(columns = name)) %>%
                                      tab_style(style = "padding-right:20px",
                                                locations = cells_body(columns = score)) %>%
                                      tab_options(table_body.hlines.width = 0,
                                                  table.border.top.color = 'black',
                                                  table.border.top.width = 4,
                                                  table.border.bottom.color = 'black',
                                                  table.border.bottom.width = 4,
                                                  table.border.left.style = 'solid',
                                                  table.border.left.color = 'black',
                                                  table.border.left.width = 4,
                                                  table.border.right.style = 'solid',
                                                  table.border.right.color = 'black',
                                                  table.border.right.width = 4,
                                                  column_labels.hidden = TRUE) %>%
                                      fmt_number(columns = c(score, projection),
                                                 drop_trailing_zeros = TRUE)
                                    })
  
  m2_away = reactive(data.frame(week = as.numeric(input$week), 
                                matchup_id = seq(2, 92, by=6)[as.numeric(input$week)], 
                                h_or_a = 'Away', 
                                team_id = schedule %>% filter(matchupId == seq(2, 92, by=6)[as.numeric(input$week)]) %>% filter(row_number() == 2) %>% select(teamId) %>% as.numeric(),
                                score = schedule %>% filter(matchupId == seq(2, 92, by=6)[as.numeric(input$week)]) %>% filter(row_number() == 2) %>% select(totalPointsLive) %>% as.numeric(),
                                projection = ifelse(
                                  is.na(schedule %>% filter(matchupId == seq(2, 92, by=6)[as.numeric(input$week)]) %>% filter(row_number() == 2) %>% select(totalProjectedPointsLive) %>% as.numeric()),
                                  "",
                                  schedule %>% filter(matchupId == seq(2, 92, by=6)[as.numeric(input$week)]) %>% filter(row_number() == 2) %>% select(totalProjectedPointsLive) %>% as.numeric())))
  
  m2_home = reactive(data.frame(week = as.numeric(input$week), 
                                matchup_id = seq(2, 92, by=6)[as.numeric(input$week)], 
                                h_or_a = 'Home', 
                                team_id = schedule %>% filter(matchupId == seq(2, 92, by=6)[as.numeric(input$week)]) %>% filter(row_number() == 1) %>% select(teamId) %>% as.numeric(),
                                score = schedule %>% filter(matchupId == seq(2, 92, by=6)[as.numeric(input$week)]) %>% filter(row_number() == 1) %>% select(totalPointsLive) %>% as.numeric(),
                                projection = ifelse(
                                  is.na(schedule %>% filter(matchupId == seq(2, 92, by=6)[as.numeric(input$week)]) %>% filter(row_number() == 1) %>% select(totalProjectedPointsLive) %>% as.numeric()),
                                  "",
                                  schedule %>% filter(matchupId == seq(2, 92, by=6)[as.numeric(input$week)]) %>% filter(row_number() == 1) %>% select(totalProjectedPointsLive) %>% as.numeric())))
  
  m2_table = reactive(rbind(m2_away(), m2_home()) %>%
                        mutate(score = ifelse(team_id == 12, ghost_score(), score),
                               projection = ifelse(team_id == 12 & !is.na(ghost_proj()), ghost_proj(), projection)))
  
  output$wk_matchup_2 = render_gt(if(as.numeric(input$week) > 13){}
                                  else{m2_table() %>% 
                                      left_join(team %>% mutate(id = as.double(id)) %>% 
                                                  select(id, logo, name, owner), 
                                                by = c('team_id' = 'id')) %>%
                                      left_join(still_playing, by="team_id") %>%
                                      left_join(team_record(), by = c("team_id" = "id")) %>%
                                      left_join(team_ov_record(), by = c("team_id" = "id")) %>%
                                      mutate(still_playing = case_when(as.numeric(input$week) == CW ~ still_playing,
                                                                       as.numeric(input$week) < CW ~ 0,
                                                                       as.numeric(input$week) > CW ~ 15)) %>%
                                      mutate(projection = ifelse(still_playing == 0, "", projection)) %>%
                                      select(logo, name, score, projection, owner, still_playing, record, ov_record) %>%
                                      mutate(win = ifelse(sum(still_playing) == 0 & score>mean(score), 1, 0)) %>%
                                      gt() %>%
                                      gt_highlight_rows(columns = gt::everything(), rows = win > 0,
                                                        fill = "#ead89e", alpha = 0.8) %>%
                                      gt_merge_stack(col1 = score, col2 = projection,
                                                     font_size = c('25px', '15px'),
                                                     font_weight = c('bold', 'normal')) %>%
                                      gt_merge_stack(col1 = name, col2 = owner,
                                                     font_size = c("25px", "15px"),
                                                     font_weight = c("bold", "normal")) %>%
                                      gt_merge_img_circle(col1 = logo, col2 = record, col3 = ov_record, height = 100) %>%
                                      cols_width(logo ~ px(100),
                                                 name ~ px(300),
                                                 score ~ px(120)) %>%
                                      cols_hide(columns = c(still_playing, win)) %>%
                                      tab_style(style = list(cell_text(weight = 'bold', size = 'x-large', align = 'left')),
                                                locations = cells_body(columns = name)) %>%
                                      tab_style(style = list(cell_text(weight = 'bold', size = 'xx-large', align = 'right')),
                                                locations = cells_body(columns = score)) %>%
                                      tab_style(style = "padding-left:40px",
                                                locations = cells_body(columns = name)) %>%
                                      tab_style(style = "padding-right:20px",
                                                locations = cells_body(columns = score)) %>%
                                      tab_options(table_body.hlines.width = 0,
                                                  table.border.top.color = 'black',
                                                  table.border.top.width = 4,
                                                  table.border.bottom.color = 'black',
                                                  table.border.bottom.width = 4,
                                                  table.border.left.style = 'solid',
                                                  table.border.left.color = 'black',
                                                  table.border.left.width = 4,
                                                  table.border.right.style = 'solid',
                                                  table.border.right.color = 'black',
                                                  table.border.right.width = 4,
                                                  column_labels.hidden = TRUE) %>%
                                      fmt_number(columns = c(score, projection),
                                                 drop_trailing_zeros = TRUE)
                                  })
  
  m3_away = reactive(data.frame(week = as.numeric(input$week), 
                                matchup_id = seq(3, 93, by=6)[as.numeric(input$week)], 
                                h_or_a = 'Away', 
                                team_id = schedule %>% filter(matchupId == seq(3, 93, by=6)[as.numeric(input$week)]) %>% filter(row_number() == 2) %>% select(teamId) %>% as.numeric(),
                                score = schedule %>% filter(matchupId == seq(3, 93, by=6)[as.numeric(input$week)]) %>% filter(row_number() == 2) %>% select(totalPointsLive) %>% as.numeric(),
                                projection = ifelse(
                                  is.na(schedule %>% filter(matchupId == seq(3, 93, by=6)[as.numeric(input$week)]) %>% filter(row_number() == 2) %>% select(totalProjectedPointsLive) %>% as.numeric()),
                                  "",
                                  schedule %>% filter(matchupId == seq(3, 93, by=6)[as.numeric(input$week)]) %>% filter(row_number() == 2) %>% select(totalProjectedPointsLive) %>% as.numeric())))
  
  m3_home = reactive(data.frame(week = as.numeric(input$week), 
                                matchup_id = seq(3, 93, by=6)[as.numeric(input$week)], 
                                h_or_a = 'Home', 
                                team_id = schedule %>% filter(matchupId == seq(3, 93, by=6)[as.numeric(input$week)]) %>% filter(row_number() == 1) %>% select(teamId) %>% as.numeric(),
                                score = schedule %>% filter(matchupId == seq(3, 93, by=6)[as.numeric(input$week)]) %>% filter(row_number() == 1) %>% select(totalPointsLive) %>% as.numeric(),
                                projection = ifelse(
                                  is.na(schedule %>% filter(matchupId == seq(3, 93, by=6)[as.numeric(input$week)]) %>% filter(row_number() == 1) %>% select(totalProjectedPointsLive) %>% as.numeric()),
                                  "",
                                  schedule %>% filter(matchupId == seq(3, 93, by=6)[as.numeric(input$week)]) %>% filter(row_number() == 1) %>% select(totalProjectedPointsLive) %>% as.numeric())))
  
  m3_table = reactive(rbind(m3_away(), m3_home()) %>%
                        mutate(score = ifelse(team_id == 12, ghost_score(), score),
                               projection = ifelse(team_id == 12 & !is.na(ghost_proj()), ghost_proj(), projection)))
  
  output$wk_matchup_3 = render_gt(if(as.numeric(input$week) > 13){}
                                  else{m3_table() %>% 
                                      left_join(team %>% mutate(id = as.double(id)) %>% 
                                                  select(id, logo, name, owner), 
                                                by = c('team_id' = 'id')) %>%
                                      left_join(still_playing, by="team_id") %>%
                                      left_join(team_record(), by = c("team_id" = "id")) %>%
                                      left_join(team_ov_record(), by = c("team_id" = "id")) %>%
                                      mutate(still_playing = case_when(as.numeric(input$week) == CW ~ still_playing,
                                                                       as.numeric(input$week) < CW ~ 0,
                                                                       as.numeric(input$week) > CW ~ 15)) %>%
                                      mutate(projection = ifelse(still_playing == 0, "", projection)) %>%
                                      select(logo, name, score, projection, owner, still_playing, record, ov_record) %>%
                                      mutate(win = ifelse(sum(still_playing) == 0 & score>mean(score), 1, 0)) %>%
                                      gt() %>%
                                      gt_highlight_rows(columns = gt::everything(), rows = win > 0,
                                                        fill = "#ead89e", alpha = 0.8) %>%
                                      gt_merge_stack(col1 = score, col2 = projection,
                                                     font_size = c('25px', '15px'),
                                                     font_weight = c('bold', 'normal')) %>%
                                      gt_merge_stack(col1 = name, col2 = owner,
                                                     font_size = c("25px", "15px"),
                                                     font_weight = c("bold", "normal")) %>%
                                      gt_merge_img_circle(col1 = logo, col2 = record, col3 = ov_record, height = 100) %>%
                                      cols_width(logo ~ px(100),
                                                 name ~ px(300),
                                                 score ~ px(120)) %>%
                                      cols_hide(columns = c(still_playing, win)) %>%
                                      tab_style(style = list(cell_text(weight = 'bold', size = 'x-large', align = 'left')),
                                                locations = cells_body(columns = name)) %>%
                                      tab_style(style = list(cell_text(weight = 'bold', size = 'xx-large', align = 'right')),
                                                locations = cells_body(columns = score)) %>%
                                      tab_style(style = "padding-left:40px",
                                                locations = cells_body(columns = name)) %>%
                                      tab_style(style = "padding-right:20px",
                                                locations = cells_body(columns = score)) %>%
                                      tab_options(table_body.hlines.width = 0,
                                                  table.border.top.color = 'black',
                                                  table.border.top.width = 4,
                                                  table.border.bottom.color = 'black',
                                                  table.border.bottom.width = 4,
                                                  table.border.left.style = 'solid',
                                                  table.border.left.color = 'black',
                                                  table.border.left.width = 4,
                                                  table.border.right.style = 'solid',
                                                  table.border.right.color = 'black',
                                                  table.border.right.width = 4,
                                                  column_labels.hidden = TRUE) %>%
                                      fmt_number(columns = c(score, projection),
                                                 drop_trailing_zeros = TRUE)
                                  })
  
  m4_away = reactive(data.frame(week = as.numeric(input$week), 
                                matchup_id = seq(4, 94, by=6)[as.numeric(input$week)], 
                                h_or_a = 'Away', 
                                team_id = schedule %>% filter(matchupId == seq(4, 94, by=6)[as.numeric(input$week)]) %>% filter(row_number() == 2) %>% select(teamId) %>% as.numeric(),
                                score = schedule %>% filter(matchupId == seq(4, 94, by=6)[as.numeric(input$week)]) %>% filter(row_number() == 2) %>% select(totalPointsLive) %>% as.numeric(),
                                projection = ifelse(
                                  is.na(schedule %>% filter(matchupId == seq(4, 94, by=6)[as.numeric(input$week)]) %>% filter(row_number() == 2) %>% select(totalProjectedPointsLive) %>% as.numeric()),
                                  "",
                                  schedule %>% filter(matchupId == seq(4, 94, by=6)[as.numeric(input$week)]) %>% filter(row_number() == 2) %>% select(totalProjectedPointsLive) %>% as.numeric())))
  
  m4_home = reactive(data.frame(week = as.numeric(input$week), 
                                matchup_id = seq(4, 94, by=6)[as.numeric(input$week)], 
                                h_or_a = 'Home', 
                                team_id = schedule %>% filter(matchupId == seq(4, 94, by=6)[as.numeric(input$week)]) %>% filter(row_number() == 1) %>% select(teamId) %>% as.numeric(),
                                score = schedule %>% filter(matchupId == seq(4, 94, by=6)[as.numeric(input$week)]) %>% filter(row_number() == 1) %>% select(totalPointsLive) %>% as.numeric(),
                                projection = ifelse(
                                  is.na(schedule %>% filter(matchupId == seq(4, 94, by=6)[as.numeric(input$week)]) %>% filter(row_number() == 1) %>% select(totalProjectedPointsLive) %>% as.numeric()),
                                  "",
                                  schedule %>% filter(matchupId == seq(4, 94, by=6)[as.numeric(input$week)]) %>% filter(row_number() == 1) %>% select(totalProjectedPointsLive) %>% as.numeric())))
  
  m4_table = reactive(rbind(m4_away(), m4_home()) %>%
                        mutate(score = ifelse(team_id == 12, ghost_score(), score),
                               projection = ifelse(team_id == 12 & !is.na(ghost_proj()), ghost_proj(), projection)))
  
  output$wk_matchup_4 = render_gt(if(as.numeric(input$week) > 13){}
                                  else{m4_table() %>% 
                                      left_join(team %>% mutate(id = as.double(id)) %>% 
                                                  select(id, logo, name, owner), 
                                                by = c('team_id' = 'id')) %>%
                                      left_join(still_playing, by="team_id") %>%
                                      left_join(team_record(), by = c("team_id" = "id")) %>%
                                      left_join(team_ov_record(), by = c("team_id" = "id")) %>%
                                      mutate(still_playing = case_when(as.numeric(input$week) == CW ~ still_playing,
                                                                       as.numeric(input$week) < CW ~ 0,
                                                                       as.numeric(input$week) > CW ~ 15)) %>%
                                      mutate(projection = ifelse(still_playing == 0, "", projection)) %>%
                                      select(logo, name, score, projection, owner, still_playing, record, ov_record) %>%
                                      mutate(win = ifelse(sum(still_playing) == 0 & score>mean(score), 1, 0)) %>%
                                      gt() %>%
                                      gt_highlight_rows(columns = gt::everything(), rows = win > 0,
                                                        fill = "#ead89e", alpha = 0.8) %>%
                                      gt_merge_stack(col1 = score, col2 = projection,
                                                     font_size = c('25px', '15px'),
                                                     font_weight = c('bold', 'normal')) %>%
                                      gt_merge_stack(col1 = name, col2 = owner,
                                                     font_size = c("25px", "15px"),
                                                     font_weight = c("bold", "normal")) %>%
                                      gt_merge_img_circle(col1 = logo, col2 = record, col3 = ov_record, height = 100) %>%
                                      cols_width(logo ~ px(100),
                                                 name ~ px(300),
                                                 score ~ px(120)) %>%
                                      cols_hide(columns = c(still_playing, win)) %>%
                                      tab_style(style = list(cell_text(weight = 'bold', size = 'x-large', align = 'left')),
                                                locations = cells_body(columns = name)) %>%
                                      tab_style(style = list(cell_text(weight = 'bold', size = 'xx-large', align = 'right')),
                                                locations = cells_body(columns = score)) %>%
                                      tab_style(style = "padding-left:40px",
                                                locations = cells_body(columns = name)) %>%
                                      tab_style(style = "padding-right:20px",
                                                locations = cells_body(columns = score)) %>%
                                      tab_options(table_body.hlines.width = 0,
                                                  table.border.top.color = 'black',
                                                  table.border.top.width = 4,
                                                  table.border.bottom.color = 'black',
                                                  table.border.bottom.width = 4,
                                                  table.border.left.style = 'solid',
                                                  table.border.left.color = 'black',
                                                  table.border.left.width = 4,
                                                  table.border.right.style = 'solid',
                                                  table.border.right.color = 'black',
                                                  table.border.right.width = 4,
                                                  column_labels.hidden = TRUE) %>%
                                      fmt_number(columns = c(score, projection),
                                                 drop_trailing_zeros = TRUE)
                                  })
  
  m5_away = reactive(data.frame(week = as.numeric(input$week), 
                                matchup_id = seq(5, 95, by=6)[as.numeric(input$week)], 
                                h_or_a = 'Away', 
                                team_id = schedule %>% filter(matchupId == seq(5, 95, by=6)[as.numeric(input$week)]) %>% filter(row_number() == 2) %>% select(teamId) %>% as.numeric(),
                                score = schedule %>% filter(matchupId == seq(5, 95, by=6)[as.numeric(input$week)]) %>% filter(row_number() == 2) %>% select(totalPointsLive) %>% as.numeric(),
                                projection = ifelse(
                                  is.na(schedule %>% filter(matchupId == seq(5, 95, by=6)[as.numeric(input$week)]) %>% filter(row_number() == 2) %>% select(totalProjectedPointsLive) %>% as.numeric()),
                                  "",
                                  schedule %>% filter(matchupId == seq(5, 95, by=6)[as.numeric(input$week)]) %>% filter(row_number() == 2) %>% select(totalProjectedPointsLive) %>% as.numeric())))
  
  m5_home = reactive(data.frame(week = as.numeric(input$week), 
                                matchup_id = seq(5, 95, by=6)[as.numeric(input$week)], 
                                h_or_a = 'Home', 
                                team_id = schedule %>% filter(matchupId == seq(5, 95, by=6)[as.numeric(input$week)]) %>% filter(row_number() == 1) %>% select(teamId) %>% as.numeric(),
                                score = schedule %>% filter(matchupId == seq(5, 95, by=6)[as.numeric(input$week)]) %>% filter(row_number() == 1) %>% select(totalPointsLive) %>% as.numeric(),
                                projection = ifelse(
                                  is.na(schedule %>% filter(matchupId == seq(5, 95, by=6)[as.numeric(input$week)]) %>% filter(row_number() == 1) %>% select(totalProjectedPointsLive) %>% as.numeric()),
                                  "",
                                  schedule %>% filter(matchupId == seq(5, 95, by=6)[as.numeric(input$week)]) %>% filter(row_number() == 1) %>% select(totalProjectedPointsLive) %>% as.numeric())))
  
  m5_table = reactive(rbind(m5_away(), m5_home()) %>%
                        mutate(score = ifelse(team_id == 12, ghost_score(), score),
                               projection = ifelse(team_id == 12 & !is.na(ghost_proj()), ghost_proj(), projection)))
  
  output$wk_matchup_5 = render_gt(if(as.numeric(input$week) > 13){}
                                  else{m5_table() %>% 
                                      left_join(team %>% mutate(id = as.double(id)) %>% 
                                                  select(id, logo, name, owner), 
                                                by = c('team_id' = 'id')) %>%
                                      left_join(still_playing, by="team_id") %>%
                                      left_join(team_record(), by = c("team_id" = "id")) %>%
                                      left_join(team_ov_record(), by = c("team_id" = "id")) %>%
                                      mutate(still_playing = case_when(as.numeric(input$week) == CW ~ still_playing,
                                                                       as.numeric(input$week) < CW ~ 0,
                                                                       as.numeric(input$week) > CW ~ 15)) %>%
                                      mutate(projection = ifelse(still_playing == 0, "", projection)) %>%
                                      select(logo, name, score, projection, owner, still_playing, record, ov_record) %>%
                                      mutate(win = ifelse(sum(still_playing) == 0 & score>mean(score), 1, 0)) %>%
                                      gt() %>%
                                      gt_highlight_rows(columns = gt::everything(), rows = win > 0,
                                                        fill = "#ead89e", alpha = 0.8) %>%
                                      gt_merge_stack(col1 = score, col2 = projection,
                                                     font_size = c('25px', '15px'),
                                                     font_weight = c('bold', 'normal')) %>%
                                      gt_merge_stack(col1 = name, col2 = owner,
                                                     font_size = c("25px", "15px"),
                                                     font_weight = c("bold", "normal")) %>%
                                      gt_merge_img_circle(col1 = logo, col2 = record, col3 = ov_record, height = 100) %>%
                                      cols_width(logo ~ px(100),
                                                 name ~ px(300),
                                                 score ~ px(120)) %>%
                                      cols_hide(columns = c(still_playing, win)) %>%
                                      tab_style(style = list(cell_text(weight = 'bold', size = 'x-large', align = 'left')),
                                                locations = cells_body(columns = name)) %>%
                                      tab_style(style = list(cell_text(weight = 'bold', size = 'xx-large', align = 'right')),
                                                locations = cells_body(columns = score)) %>%
                                      tab_style(style = "padding-left:40px",
                                                locations = cells_body(columns = name)) %>%
                                      tab_style(style = "padding-right:20px",
                                                locations = cells_body(columns = score)) %>%
                                      tab_options(table_body.hlines.width = 0,
                                                  table.border.top.color = 'black',
                                                  table.border.top.width = 4,
                                                  table.border.bottom.color = 'black',
                                                  table.border.bottom.width = 4,
                                                  table.border.left.style = 'solid',
                                                  table.border.left.color = 'black',
                                                  table.border.left.width = 4,
                                                  table.border.right.style = 'solid',
                                                  table.border.right.color = 'black',
                                                  table.border.right.width = 4,
                                                  column_labels.hidden = TRUE) %>%
                                      fmt_number(columns = c(score, projection),
                                                 drop_trailing_zeros = TRUE)
                                  })
  
  m6_away = reactive(data.frame(week = as.numeric(input$week), 
                                matchup_id = seq(6, 96, by=6)[as.numeric(input$week)], 
                                h_or_a = 'Away', 
                                team_id = schedule %>% filter(matchupId == seq(6, 96, by=6)[as.numeric(input$week)]) %>% filter(row_number() == 2) %>% select(teamId) %>% as.numeric(),
                                score = schedule %>% filter(matchupId == seq(6, 96, by=6)[as.numeric(input$week)]) %>% filter(row_number() == 2) %>% select(totalPointsLive) %>% as.numeric(),
                                projection = ifelse(
                                  is.na(schedule %>% filter(matchupId == seq(6, 96, by=6)[as.numeric(input$week)]) %>% filter(row_number() == 2) %>% select(totalProjectedPointsLive) %>% as.numeric()),
                                  "",
                                  schedule %>% filter(matchupId == seq(6, 96, by=6)[as.numeric(input$week)]) %>% filter(row_number() == 2) %>% select(totalProjectedPointsLive) %>% as.numeric())))
  
  m6_home = reactive(data.frame(week = as.numeric(input$week), 
                                matchup_id = seq(6, 96, by=6)[as.numeric(input$week)], 
                                h_or_a = 'Home', 
                                team_id = schedule %>% filter(matchupId == seq(6, 96, by=6)[as.numeric(input$week)]) %>% filter(row_number() == 1) %>% select(teamId) %>% as.numeric(),
                                score = schedule %>% filter(matchupId == seq(6, 96, by=6)[as.numeric(input$week)]) %>% filter(row_number() == 1) %>% select(totalPointsLive) %>% as.numeric(),
                                projection = ifelse(
                                  is.na(schedule %>% filter(matchupId == seq(6, 96, by=6)[as.numeric(input$week)]) %>% filter(row_number() == 1) %>% select(totalProjectedPointsLive) %>% as.numeric()),
                                  "",
                                  schedule %>% filter(matchupId == seq(6, 96, by=6)[as.numeric(input$week)]) %>% filter(row_number() == 1) %>% select(totalProjectedPointsLive) %>% as.numeric())))
  
  m6_table = reactive(rbind(m6_away(), m6_home()) %>%
                        mutate(score = ifelse(team_id == 12, ghost_score(), score),
                               projection = ifelse(team_id == 12 & !is.na(ghost_proj()), ghost_proj(), projection)))
  
  output$wk_matchup_6 = render_gt(if(as.numeric(input$week) > 13){}
                                  else{m6_table() %>% 
                                      left_join(team %>% mutate(id = as.double(id)) %>% 
                                                  select(id, logo, name, owner), 
                                                by = c('team_id' = 'id')) %>%
                                      left_join(still_playing, by="team_id") %>%
                                      left_join(team_record(), by = c("team_id" = "id")) %>%
                                      left_join(team_ov_record(), by = c("team_id" = "id")) %>%
                                      mutate(still_playing = case_when(as.numeric(input$week) == CW ~ still_playing,
                                                                       as.numeric(input$week) < CW ~ 0,
                                                                       as.numeric(input$week) > CW ~ 15)) %>%
                                      mutate(projection = ifelse(still_playing == 0, "", projection)) %>%
                                      select(logo, name, score, projection, owner, still_playing, record, ov_record) %>%
                                      mutate(win = ifelse(sum(still_playing) == 0 & score>mean(score), 1, 0)) %>%
                                      gt() %>%
                                      gt_highlight_rows(columns = gt::everything(), rows = win > 0,
                                                        fill = "#ead89e", alpha = 0.8) %>%
                                      gt_merge_stack(col1 = score, col2 = projection,
                                                     font_size = c('25px', '15px'),
                                                     font_weight = c('bold', 'normal')) %>%
                                      gt_merge_stack(col1 = name, col2 = owner,
                                                     font_size = c("25px", "15px"),
                                                     font_weight = c("bold", "normal")) %>%
                                      gt_merge_img_circle(col1 = logo, col2 = record, col3 = ov_record, height = 100) %>%
                                      cols_width(logo ~ px(100),
                                                 name ~ px(300),
                                                 score ~ px(120)) %>%
                                      cols_hide(columns = c(still_playing, win)) %>%
                                      tab_style(style = list(cell_text(weight = 'bold', size = 'x-large', align = 'left')),
                                                locations = cells_body(columns = name)) %>%
                                      tab_style(style = list(cell_text(weight = 'bold', size = 'xx-large', align = 'right')),
                                                locations = cells_body(columns = score)) %>%
                                      tab_style(style = "padding-left:40px",
                                                locations = cells_body(columns = name)) %>%
                                      tab_style(style = "padding-right:20px",
                                                locations = cells_body(columns = score)) %>%
                                      tab_options(table_body.hlines.width = 0,
                                                  table.border.top.color = 'black',
                                                  table.border.top.width = 4,
                                                  table.border.bottom.color = 'black',
                                                  table.border.bottom.width = 4,
                                                  table.border.left.style = 'solid',
                                                  table.border.left.color = 'black',
                                                  table.border.left.width = 4,
                                                  table.border.right.style = 'solid',
                                                  table.border.right.color = 'black',
                                                  table.border.right.width = 4,
                                                  column_labels.hidden = TRUE) %>%
                                      fmt_number(columns = c(score, projection),
                                                 drop_trailing_zeros = TRUE)
                                  })
  
}

shinyApp(ui, server)
