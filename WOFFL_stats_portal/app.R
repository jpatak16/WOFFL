library(shiny)
library(pacman)
p_load(dplyr, readxl, magrittr, janitor)

AllGames = read_excel("AllGames.xlsx", sheet = "All Games") %>%
  clean_names() %>%
  mutate(result = ifelse(score > opponent_score, 1, 0)) %>%
  group_by(ov_wk) %>%
  mutate(ov_wins = order(order(score, decreasing = FALSE)) - 1) %>%
  mutate(ov_losses = order(order(score, decreasing = TRUE)) - 1) %>%
  mutate(opp_ov_wins = order(order(opponent_score, decreasing = FALSE)) - 1) %>%
  mutate(opp_ov_losses = order(order(opponent_score, decreasing = TRUE)) - 1) %>%
  ungroup()

CS = unique(AllGames$season) %>% max()
CW = AllGames %>% filter(season==CS) %>% filter(!is.na(score)) %>% filter(score>0) %>% 
  arrange(desc(week)) %>% select(week) %>% unlist() %>% max()
CW = ifelse(CW == -Inf, 0, CW)

week_selector_options = AllGames %>% filter(season==CS) %>% select(week) %>% unlist() %>% unique() %>% paste("Week", ., sep = " ")


ui = navbarPage("WOFFL Portal", fluid = TRUE,
                tabPanel("Weekly Scoreboard",
                         fluidRow(column(9, h1(span("White Oak Fantasy Football League Portal", style = 'color:#8A8A8A; text-shadow: black 0.0em 0.1em 0.2em')), 
                                         h1(span("Weekly Scoreboard", style = 'font-size: 60px; font-weight: bold; color:#FFFFFF; text-shadow: black 0.0em 0.18em 0.2em'))),
                                  column(3, img(src="3d.jpg", height = 150, width = 210)),
                                  style = 'margin-top:-20px; padding-top:10px; padding-bottom:10px; background-color:#580515'),
                         fluidRow(column(12, align='center', selectInput("week", "Week Selector", week_selector_options)), 
                                  style = 'padding-top:10px;')
                         ) #end of WS tabPanel
                ) #end of navbarPage

# Define server logic required to draw a histogram
server = function(input, output) {

}

shinyApp(ui, server)
