library(shiny)
library(pacman)
p_load(dplyr, readxl, magrittr, janitor, httr, jsonlite, gt, gtExtras, tidyr)

AllGames = read_excel("AllGames.xlsx", sheet = "All Games") %>%
  clean_names() %>%
  mutate(result = ifelse(score > opponent_score, 1, 0)) %>%
  group_by(ov_wk) %>%
  mutate(ov_wins = order(order(score, decreasing = FALSE)) - 1) %>%
  mutate(ov_losses = order(order(score, decreasing = TRUE)) - 1) %>%
  mutate(opp_ov_wins = order(order(opponent_score, decreasing = FALSE)) - 1) %>%
  mutate(opp_ov_losses = order(order(opponent_score, decreasing = TRUE)) - 1) %>%
  ungroup()

woffl_id = 313259

CS = unique(AllGames$season) %>% max()
CW = AllGames %>% filter(season==CS) %>% filter(!is.na(score)) %>% filter(score>0) %>% 
  arrange(desc(week)) %>% select(week) %>% unlist() %>% max()
CW = ifelse(CW == -Inf, 0, CW)

week_selector_options = AllGames %>% filter(season==CS) %>% select(week) %>% unlist() %>% unique() %>% paste("Week", ., sep = " ")
week_selector_options = case_when(week_selector_options=="Week 14" ~ "Postseason Week 1",
                                  week_selector_options=="Week 15" ~ "Postseason Week 2",
                                  week_selector_options=="Week 16" ~ "Postseason Week 3",
                                  .default = week_selector_options)

url_matchup = paste0('https://fantasy.espn.com/apis/v3/games/ffl/seasons/', CS, '/segments/0/leagues/', woffl_id, '?view=', 'mMatchup')

conn_matchup = GET(url_matchup)

schedule = content(conn_matchup, as = 'text') %>% fromJSON(flatten = TRUE) %>% .$schedule

url_team = paste0('https://fantasy.espn.com/apis/v3/games/ffl/seasons/', CS, '/segments/0/leagues/', woffl_id, '?view=', 'mTeam')

conn_team = GET(url_team)

team = content(conn_team, as = 'text') %>% fromJSON(flatten = TRUE) %>% .$teams

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
  
  output$playoffs = renderUI(if(sub("(\\w+).*", "\\1", input$week) == "Postseason"){img(src="playoffs.gif", height = 300)}
                             else{img(src="playoffs.gif", height = 0)})
  
  week_num = reactive(strsplit(input$week, "eek ")[[1]][2])
  
  ghost_score = reactive((((schedule %>% filter(matchupPeriodId == week_num()) %>% filter(away.teamId != 12) %>% select(away.totalPoints) %>% as.vector() %>% unlist() %>% sum()) + 
                             (schedule %>% filter(matchupPeriodId == week_num()) %>% filter(home.teamId != 12) %>% select(home.totalPoints) %>% as.vector() %>% unlist() %>% sum())) / 11))
  
  m1_away = reactive(data.frame(week = week_num(), 
                                matchup_id = 1, 
                                h_or_a = 'Away', 
                                team_id = schedule %>% filter(matchupPeriodId == week_num()) %>% filter(row_number() == 1) %>% select(away.teamId) %>% as.numeric(),
                                score = schedule %>% filter(matchupPeriodId == week_num()) %>% filter(row_number() == 1) %>% select(away.totalPoints) %>% as.numeric()))
  
  m1_home = reactive(data.frame(week = week_num(), 
                                matchup_id = 1, 
                                h_or_a = 'Home', 
                                team_id = schedule %>% filter(matchupPeriodId == week_num()) %>% filter(row_number() == 1) %>% select(home.teamId) %>% as.numeric(),
                                score = schedule %>% filter(matchupPeriodId == week_num()) %>% filter(row_number() == 1) %>% select(home.totalPoints) %>% as.numeric()))
  
  m1_table = reactive(rbind(m1_away(), m1_home()) %>%
                        mutate(score = ifelse(team_id == 12, ghost_score(), score)))
  
  output$wk_matchup_1 = render_gt(if(sub("(\\w+).*", "\\1", input$week) == "Postseason"){}
                                  else{m1_table() %>% 
                                      left_join(team %>% mutate(id = as.double(id)) %>% 
                                                  select(id, logo, name, record.overall.wins, record.overall.losses, record.overall.ties), 
                                                by = c('team_id' = 'id')) %>%
                                      select(logo, name, score) %>%
                                      gt() %>%
                                      gt_img_circle(logo, height = 100) %>%
                                      cols_width(logo ~ px(100),
                                                 name ~ px(300),
                                                 score ~ px(100)) %>%
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
                                                  column_labels.hidden = TRUE,
                                                  )
                                    })
  
  m2_away = reactive(data.frame(week = week_num(), 
                                matchup_id = 2, 
                                h_or_a = 'Away', 
                                team_id = schedule %>% filter(matchupPeriodId == week_num()) %>% filter(row_number() == 2) %>% select(away.teamId) %>% as.numeric(),
                                score = schedule %>% filter(matchupPeriodId == week_num()) %>% filter(row_number() == 2) %>% select(away.totalPoints) %>% as.numeric()))
  
  m2_home = reactive(data.frame(week = week_num(), 
                                matchup_id = 2, 
                                h_or_a = 'Home', 
                                team_id = schedule %>% filter(matchupPeriodId == week_num()) %>% filter(row_number() == 2) %>% select(home.teamId) %>% as.numeric(),
                                score = schedule %>% filter(matchupPeriodId == week_num()) %>% filter(row_number() == 2) %>% select(home.totalPoints) %>% as.numeric()))
  
  m2_table = reactive(rbind(m2_away(), m2_home()) %>%
                        mutate(score = ifelse(team_id == 12, ghost_score(), score)))
  
  output$wk_matchup_2 = render_gt(if(sub("(\\w+).*", "\\1", input$week) == "Postseason"){}
                                  else{m2_table() %>% 
                                      left_join(team %>% mutate(id = as.double(id)) %>% 
                                                  select(id, logo, name, record.overall.wins, record.overall.losses, record.overall.ties), 
                                                by = c('team_id' = 'id')) %>%
                                      select(logo, name, score) %>%
                                      gt() %>%
                                      gt_img_circle(logo, height = 100) %>%
                                      cols_width(logo ~ px(100),
                                                 name ~ px(300),
                                                 score ~ px(100)) %>%
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
                                                  column_labels.hidden = TRUE,
                                      )
                                  })
  
  m3_away = reactive(data.frame(week = week_num(), 
                                matchup_id = 3, 
                                h_or_a = 'Away', 
                                team_id = schedule %>% filter(matchupPeriodId == week_num()) %>% filter(row_number() == 3) %>% select(away.teamId) %>% as.numeric(),
                                score = schedule %>% filter(matchupPeriodId == week_num()) %>% filter(row_number() == 3) %>% select(away.totalPoints) %>% as.numeric()))
  
  m3_home = reactive(data.frame(week = week_num(), 
                                matchup_id = 3, 
                                h_or_a = 'Home', 
                                team_id = schedule %>% filter(matchupPeriodId == week_num()) %>% filter(row_number() == 3) %>% select(home.teamId) %>% as.numeric(),
                                score = schedule %>% filter(matchupPeriodId == week_num()) %>% filter(row_number() == 3) %>% select(home.totalPoints) %>% as.numeric()))
  
  m3_table = reactive(rbind(m3_away(), m3_home()) %>%
                        mutate(score = ifelse(team_id == 12, ghost_score(), score)))
  
  output$wk_matchup_3 = render_gt(if(sub("(\\w+).*", "\\1", input$week) == "Postseason"){}
                                  else{m3_table() %>% 
                                      left_join(team %>% mutate(id = as.double(id)) %>% 
                                                  select(id, logo, name, record.overall.wins, record.overall.losses, record.overall.ties), 
                                                by = c('team_id' = 'id')) %>%
                                      select(logo, name, score) %>%
                                      gt() %>%
                                      gt_img_circle(logo, height = 100) %>%
                                      cols_width(logo ~ px(100),
                                                 name ~ px(300),
                                                 score ~ px(100)) %>%
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
                                                  column_labels.hidden = TRUE,
                                      )
                                  })
  
  m4_away = reactive(data.frame(week = week_num(), 
                                matchup_id = 4, 
                                h_or_a = 'Away', 
                                team_id = schedule %>% filter(matchupPeriodId == week_num()) %>% filter(row_number() == 4) %>% select(away.teamId) %>% as.numeric(),
                                score = schedule %>% filter(matchupPeriodId == week_num()) %>% filter(row_number() == 4) %>% select(away.totalPoints) %>% as.numeric()))
  
  m4_home = reactive(data.frame(week = week_num(), 
                                matchup_id = 4, 
                                h_or_a = 'Home', 
                                team_id = schedule %>% filter(matchupPeriodId == week_num()) %>% filter(row_number() == 4) %>% select(home.teamId) %>% as.numeric(),
                                score = schedule %>% filter(matchupPeriodId == week_num()) %>% filter(row_number() == 4) %>% select(home.totalPoints) %>% as.numeric()))
  
  m4_table = reactive(rbind(m4_away(), m4_home()) %>%
                        mutate(score = ifelse(team_id == 12, ghost_score(), score)))
  
  output$wk_matchup_4 = render_gt(if(sub("(\\w+).*", "\\1", input$week) == "Postseason"){}
                                  else{m4_table() %>% 
                                      left_join(team %>% mutate(id = as.double(id)) %>% 
                                                  select(id, logo, name, record.overall.wins, record.overall.losses, record.overall.ties), 
                                                by = c('team_id' = 'id')) %>%
                                      select(logo, name, score) %>%
                                      gt() %>%
                                      gt_img_circle(logo, height = 100) %>%
                                      cols_width(logo ~ px(100),
                                                 name ~ px(300),
                                                 score ~ px(100)) %>%
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
                                                  column_labels.hidden = TRUE,
                                      )
                                  })
  
  m5_away = reactive(data.frame(week = week_num(), 
                                matchup_id = 5, 
                                h_or_a = 'Away', 
                                team_id = schedule %>% filter(matchupPeriodId == week_num()) %>% filter(row_number() == 5) %>% select(away.teamId) %>% as.numeric(),
                                score = schedule %>% filter(matchupPeriodId == week_num()) %>% filter(row_number() == 5) %>% select(away.totalPoints) %>% as.numeric()))
  
  m5_home = reactive(data.frame(week = week_num(), 
                                matchup_id = 5, 
                                h_or_a = 'Home', 
                                team_id = schedule %>% filter(matchupPeriodId == week_num()) %>% filter(row_number() == 5) %>% select(home.teamId) %>% as.numeric(),
                                score = schedule %>% filter(matchupPeriodId == week_num()) %>% filter(row_number() == 5) %>% select(home.totalPoints) %>% as.numeric()))
  
  m5_table = reactive(rbind(m5_away(), m5_home()) %>%
                        mutate(score = ifelse(team_id == 12, ghost_score(), score)))
  
  output$wk_matchup_5 = render_gt(if(sub("(\\w+).*", "\\1", input$week) == "Postseason"){}
                                  else{m5_table() %>% 
                                      left_join(team %>% mutate(id = as.double(id)) %>% 
                                                  select(id, logo, name, record.overall.wins, record.overall.losses, record.overall.ties), 
                                                by = c('team_id' = 'id')) %>%
                                      select(logo, name, score) %>%
                                      gt() %>%
                                      gt_img_circle(logo, height = 100) %>%
                                      cols_width(logo ~ px(100),
                                                 name ~ px(300),
                                                 score ~ px(100)) %>%
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
                                                  column_labels.hidden = TRUE,
                                      )
                                  })
  
  m6_away = reactive(data.frame(week = week_num(), 
                                matchup_id = 6, 
                                h_or_a = 'Away', 
                                team_id = schedule %>% filter(matchupPeriodId == week_num()) %>% filter(row_number() == 6) %>% select(away.teamId) %>% as.numeric(),
                                score = schedule %>% filter(matchupPeriodId == week_num()) %>% filter(row_number() == 6) %>% select(away.totalPoints) %>% as.numeric()))
  
  m6_home = reactive(data.frame(week = week_num(), 
                                matchup_id = 6, 
                                h_or_a = 'Home', 
                                team_id = schedule %>% filter(matchupPeriodId == week_num()) %>% filter(row_number() == 6) %>% select(home.teamId) %>% as.numeric(),
                                score = schedule %>% filter(matchupPeriodId == week_num()) %>% filter(row_number() == 6) %>% select(home.totalPoints) %>% as.numeric()))
  
  m6_table = reactive(rbind(m6_away(), m6_home()) %>%
                        mutate(score = ifelse(team_id == 12, ghost_score(), score)))
  
  output$wk_matchup_6 = render_gt(if(sub("(\\w+).*", "\\1", input$week) == "Postseason"){}
                                  else{m6_table() %>% 
                                      left_join(team %>% mutate(id = as.double(id)) %>% 
                                                  select(id, logo, name, record.overall.wins, record.overall.losses, record.overall.ties), 
                                                by = c('team_id' = 'id')) %>%
                                      select(logo, name, score) %>%
                                      gt() %>%
                                      gt_img_circle(logo, height = 100) %>%
                                      cols_width(logo ~ px(100),
                                                 name ~ px(300),
                                                 score ~ px(100)) %>%
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
                                                  column_labels.hidden = TRUE,
                                      )
                                  })
  
}

shinyApp(ui, server)
