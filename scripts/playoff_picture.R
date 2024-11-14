library(pacman)
p_load(dplyr, janitor, gt, gtExtras, gtUtils, ggplot2, cowplot)

`%notin%` <- Negate(`%in%`)

SeasonGames <- read.csv(
    "WOFFL_stats_portal/AllGames.csv"
  ) |>
  clean_names() |>
  filter(
    season == 2024
  ) |>
  # Remove extra things we don't need
  select(
    week, team, score, opponent, opponent_score
  ) |>
  mutate(
    win = ifelse(score > opponent_score, 1, 0),
    loss = ifelse(opponent_score > score, 1, 0),
    tie = ifelse(score == opponent_score, 1, 0),
    score = ifelse(score==0, NA, score),
    opponent_score = ifelse(opponent_score==0, NA, opponent_score),
    result = case_when(
      score > opponent_score ~ 1,
      opponent_score > score ~ 0,
      score == opponent_score ~ .5
    )
  ) |>
  filter(
    !is.na(score)
  )

# Summarize current season to date
season_sum <- SeasonGames |> 
  summarise(
    wins = sum(win, na.rm = T), 
    losses = sum(loss, na.rm = T),
    ties = sum(tie, na.rm = T),
    PF = sum(score, na.rm = T),
    results = sum(result, na.rm = T),
    .by = team
  ) |>
  filter(
    team != "Ghost of Dakota Frantum",
    team != ""
  )

weeks_played <- SeasonGames |>
  filter(
    !is.na(score)
  ) |>
  pull(week) |>
  max()

# One Seed
one_seed_n_wins <- season_sum |>
  arrange(
    desc(results)
  ) |>
  pull(results) |>
  max()

teams_tie_1seed <- season_sum |> 
  arrange(
    desc(results)
  ) |>
  filter(
    results == one_seed_n_wins
  ) |> 
  pull(team)

one_seed_table <- season_sum |>
  left_join(
    SeasonGames |>
      filter(
        team %in% teams_tie_1seed,
        opponent %in% teams_tie_1seed
      ) |>
      summarise(
        tb_h2h_wins = sum(win, na.rm = T),
        tb_h2h_losses = sum(loss, na.rm = T),
        tb_h2h_ties = sum(tie, na.rm = T),
        tb_h2h_gp = n(),
        .by = team
      )
  ) |>
  mutate(
    tb_h2h_wp = ifelse(
      is.na(tb_h2h_gp), 
      1, 
      tb_h2h_wins+(.5*tb_h2h_ties) / tb_h2h_gp
    )
  ) |>
  arrange(
    desc(results), 
    desc(tb_h2h_wp), 
    desc(PF)
  ) |>
  mutate(
    record = ifelse(
      ties > 0,
      paste0(wins, "-", losses, "-", ties),
      paste0(wins, "-", losses)
    ),
    tb_h2h_record = ifelse(
      tb_h2h_ties > 0,
      paste0(tb_h2h_wins, "-", tb_h2h_losses, "-", tb_h2h_ties),
      paste0(tb_h2h_wins, "-", tb_h2h_losses)
    )
  ) |>
  filter(
    results >= one_seed_n_wins-1
  ) |>
  select(team, record, tb_h2h_record, PF)

qualified <- one_seed_table |>
  filter(
    row_number() == 1
  ) |>
  pull(team)

# Two Seed
two_seed_n_pf <- season_sum |>
  filter(
    team %notin% qualified
  ) |>
  arrange(
    desc(PF)
  ) |>
  pull(PF) |>
  max()

two_seed_table <- season_sum |>
  filter(
    team %notin% qualified
  ) |>
  arrange(
    desc(PF)
  ) |>
  filter(
    PF > two_seed_n_pf-100
  ) |>
  select(team, PF)

qualified <- append(
    qualified,
    two_seed_table |>
      filter(
        row_number() == 1
      ) |>
      pull(team)
  )

# Three Seed
three_seed_n_wins <- season_sum |>
  filter(
    team %notin% qualified
  ) |>
  pull(results) |>
  max()

teams_tie_3seed <- season_sum |> 
  filter(
    team %notin% qualified
  ) |>
  arrange(
    desc(results)
  ) |>
  filter(
    results == three_seed_n_wins
  ) |> 
  pull(team)

three_seed_table <- season_sum |>
  left_join(
    SeasonGames |>
      filter(
        team %in% teams_tie_3seed,
        opponent %in% teams_tie_3seed
      ) |>
      summarise(
        tb_h2h_wins = sum(win, na.rm = T),
        tb_h2h_losses = sum(loss, na.rm = T),
        tb_h2h_ties = sum(tie, na.rm = T),
        tb_h2h_gp = n(),
        .by = team
      ),
    by = 'team'
  ) |>
  filter(
    team %notin% qualified
  ) |>
  mutate(
    tb_h2h_wp = ifelse(
      is.na(tb_h2h_gp), 
      1, 
      tb_h2h_wins+(.5*tb_h2h_ties) / tb_h2h_gp
    )
  ) |>
  arrange(
    desc(results), 
    desc(tb_h2h_wp), 
    desc(PF)
  ) |>
  mutate(
    record = ifelse(
      ties > 0,
      paste0(wins, "-", losses, "-", ties),
      paste0(wins, "-", losses)
    ),
    tb_h2h_record = ifelse(
      tb_h2h_ties > 0,
      paste0(tb_h2h_wins, "-", tb_h2h_losses, "-", tb_h2h_ties),
      paste0(tb_h2h_wins, "-", tb_h2h_losses)
    )
  ) |>
  filter(
    results >= three_seed_n_wins-1
  ) |>
  select(team, record, tb_h2h_record, PF)

qualified <- append(
  qualified,
  three_seed_table |>
    filter(
      row_number() == 1
    ) |>
    pull(team)
)

# Four Seed
four_seed_n_wins <- season_sum |>
  filter(
    team %notin% qualified
  ) |>
  pull(results) |>
  max()

teams_tie_4seed <- season_sum |> 
  filter(
    team %notin% qualified
  ) |>
  arrange(
    desc(results)
  ) |>
  filter(
    results == four_seed_n_wins
  ) |> 
  pull(team)

four_seed_table <- season_sum |>
  left_join(
    SeasonGames |>
      filter(
        team %in% teams_tie_4seed,
        opponent %in% teams_tie_4seed
      ) |>
      summarise(
        tb_h2h_wins = sum(win, na.rm = T),
        tb_h2h_losses = sum(loss, na.rm = T),
        tb_h2h_ties = sum(tie, na.rm = T),
        tb_h2h_gp = n(),
        .by = team
      ),
    by = 'team'
  ) |>
  filter(
    team %notin% qualified
  ) |>
  mutate(
    tb_h2h_wp = ifelse(
      is.na(tb_h2h_gp), 
      1, 
      tb_h2h_wins+(.5*tb_h2h_ties) / tb_h2h_gp
    )
  ) |>
  arrange(
    desc(results), 
    desc(tb_h2h_wp), 
    desc(PF)
  ) |>
  mutate(
    record = ifelse(
      ties > 0,
      paste0(wins, "-", losses, "-", ties),
      paste0(wins, "-", losses)
    ),
    tb_h2h_record = ifelse(
      tb_h2h_ties > 0,
      paste0(tb_h2h_wins, "-", tb_h2h_losses, "-", tb_h2h_ties),
      paste0(tb_h2h_wins, "-", tb_h2h_losses)
    )
  ) |>
  filter(
    results >= four_seed_n_wins-1
  ) |>
  select(team, record, tb_h2h_record, PF)

qualified <- append(
  qualified,
  four_seed_table |>
    filter(
      row_number() == 1
    ) |>
    pull(team)
)

# Five Seed
five_seed_n_wins <- SeasonGames |> 
  filter(
    team %notin% qualified,
    team != "Ghost of Dakota Frantum",
    team != ""
  ) |>
  filter(
    week >= 7
  ) |>
  summarise(
    l7_wins = sum(result),
    l7_PF = sum(score),
    .by = team
  ) |>
  pull(l7_wins) |>
  max()

five_seed_table <- SeasonGames |> 
  filter(
    team %notin% qualified,
    team != "Ghost of Dakota Frantum",
    team != ""
  ) |>
  filter(
    week >= 7
  ) |>
  summarise(
    l7_wins = sum(result), 
    l7_PF = sum(score),
    .by = team
  ) |>
  arrange(
    desc(l7_wins),
    desc(l7_PF)
  ) |>
  filter(
    l7_wins >= five_seed_n_wins-1
  )

qualified <- append(
  qualified,
  five_seed_table |>
    filter(
      row_number() == 1
    ) |>
    pull(team)
)

# Six Seed
six_seed_n_pf <- season_sum |>
  filter(
    team %notin% qualified
  ) |>
  arrange(
    desc(PF)
  ) |>
  pull(PF) |>
  max()

six_seed_table <- season_sum |>
  filter(
    team %notin% qualified
  ) |>
  arrange(
    desc(PF)
  ) |>
  filter(
    PF > two_seed_n_pf-100
  ) |>
  select(team, PF)

qualified <- append(
  qualified,
  six_seed_table |>
    filter(
      row_number() == 1
    ) |>
    pull(team)
)


# Reseed non-FRB teams
reseed3_n_wins = season_sum %>% 
  filter(
    team %in% qualified[3:6]
  ) |>
  arrange(
    desc(results),
    desc(PF)
  ) %>% 
  filter(
    row_number() == 1
  ) %>% 
  select(results) %>% 
  as.numeric()

teams_tie_resd3 = season_sum %>% 
  filter(
    team %in% qualified[3:6]
  ) |>
  arrange(
    desc(results)
  ) %>% 
  filter(
    row_number() %in% 1:(season_sum %>% 
                           arrange(
                             desc(results)
                           ) %>% 
                           filter(
                             results == reseed3_n_wins
                           ) %>% 
                           select(team) %>% 
                           nrow()
    )
  ) %>% 
  select(team) %>% 
  as.vector() %>% 
  unlist()

po_resd3 = season_sum %>%
  filter(
    team %in% qualified[3:6]
  ) %>%
  left_join(
    SeasonGames %>% 
      filter(
        team %in% teams_tie_resd3,
        opponent %in% teams_tie_resd3
      ) %>% 
      group_by(team) %>% 
      summarise(
        tb_h2h_reseed3 = sum(result)
      ),
    by = 'team'
  ) %>%
  arrange(
    desc(results), 
    desc(tb_h2h_reseed3), 
    desc(PF)
  ) |>
  filter(
    row_number() == 1
  ) |>
  pull(team)

reseed4_n_wins = season_sum %>% 
  filter(
    team %in% qualified[3:6],
    team != po_resd3
  ) |>
  arrange(
    desc(results),
    desc(PF)
  ) %>% 
  filter(
    row_number() == 1
  ) %>% 
  select(results) %>% 
  as.numeric()

teams_tie_resd4 = season_sum %>% 
  filter(
    team %in% qualified[3:6],
    team != po_resd3
  ) |>
  arrange(
    desc(results)
  ) %>% 
  filter(
    row_number() %in% 1:(season_sum %>% 
                           filter(
                             team %in% qualified[3:6],
                             team != po_resd3
                           ) |>
                           arrange(
                             desc(results)
                           ) %>% 
                           filter(
                             results == reseed4_n_wins
                           ) %>% 
                           select(team) %>% 
                           nrow()
    )
  ) %>% 
  select(team) %>% 
  as.vector() %>% 
  unlist()

po_resd4 = season_sum %>%
  filter(
    team %in% qualified[3:6],
    team != po_resd3
  ) |>
  left_join(
    SeasonGames %>% 
      filter(
        team %in% teams_tie_resd4,
        opponent %in% teams_tie_resd4
      ) %>% 
      group_by(team) %>% 
      summarise(
        tb_h2h_reseed4 = sum(result)
      ),
    by = 'team'
  ) %>%
  arrange(
    desc(results), 
    desc(tb_h2h_reseed4), 
    desc(PF)
  ) |>
  filter(
    row_number() == 1
  ) |>
  pull(team)

reseed5_n_wins = season_sum %>% 
  filter(
    team %in% qualified[3:6],
    team != po_resd3,
    team != po_resd4
  ) |>
  arrange(
    desc(results),
    desc(PF)
  ) %>% 
  filter(
    row_number() == 1
  ) %>% 
  select(results) %>% 
  as.numeric()

teams_tie_resd5 = season_sum %>% 
  filter(
    team %in% qualified[3:6],
    team != po_resd3,
    team != po_resd4
  ) |>
  arrange(
    desc(results)
  ) %>% 
  filter(
    row_number() %in% 1:(season_sum %>% 
                           filter(
                             team %in% qualified[3:6],
                             team != po_resd3,
                             team != po_resd4
                           ) |>
                           arrange(
                             desc(results)
                           ) %>% 
                           filter(
                             results == reseed5_n_wins
                           ) %>% 
                           select(team) %>% 
                           nrow()
    )
  ) %>% 
  select(team) %>% 
  as.vector() %>% 
  unlist()

po_resd5 = season_sum %>%
  filter(
    team %in% qualified[3:6],
    team != po_resd3,
    team != po_resd4
  ) |>
  left_join(
    SeasonGames %>% 
      filter(
        team %in% teams_tie_resd5,
        opponent %in% teams_tie_resd5
      ) %>% 
      group_by(team) %>% 
      summarise(
        tb_h2h_reseed5 = sum(result)
      ),
    by = 'team'
  ) %>%
  arrange(
    desc(results), 
    desc(tb_h2h_reseed5), 
    desc(PF)
  ) |>
  filter(
    row_number() == 1
  ) |>
  pull(team)

po_resd6 <- season_sum |>
  filter(
    team %in% qualified[3:6],
    team != po_resd3,
    team != po_resd4,
    team != po_resd5
  ) |>
  pull(team)


# Create GT plots
table1 <- one_seed_table |>
  gt() |>
  gt_theme_pl() |>
  tab_style(
    style = cell_text(align = "center"),
    locations = cells_column_labels(
      c(team, record, tb_h2h_record, PF)
    )
  ) |>
  cols_align(
    align = c("center"),
    columns = c(team, record, tb_h2h_record, PF)
  ) |>
  gt_border_bars_bottom(
    bar_height = 7,
    colors = c("grey", "#580515", "black")
  ) |>
  gt_border_bars_top(
    bar_height = 4,
    colors = c("black", "#580515", "grey")
  ) |>
  tab_header(
    subtitle = "1st Seed (FRB)",
    title = ""
  ) |>
  tab_options(
    heading.align = "center",
    heading.subtitle.font.size = px(30)
  ) |>
  cols_label(
    team ~ "Team",
    record ~ "Record",
    tb_h2h_record ~ "TB Record"
  ) |>
  cols_width(
    team ~ px(150),
    record ~ px(60),
    tb_h2h_record ~ px(60),
    PF ~ px(60)
  ) |>
  tab_style(
    style = list(cell_text(color = "black")),
    locations = cells_column_labels(
      columns = c(team, record, tb_h2h_record, PF)
    )
  ) |>
  tab_style(
    style = list(cell_fill(color = "gold")),
    locations = cells_body(
      rows = 1
    )
  ) |>
  tab_style(
    style = list(cell_text(weight = "bold")),
    locations = cells_body(
      columns = team,
    )
  ) |>
  text_transform(
    locations = cells_body(),
    fn = function(x) {
      ifelse(is.na(x), "", x)
    }
  )

table2 <- two_seed_table |>
  gt() |>
  gt_theme_pl() |>
  tab_style(
    style = cell_text(align = "center"),
    locations = cells_column_labels(
      c(team, PF)
    )
  ) |>
  cols_align(
    align = c("center"),
    columns = c(team, PF)
  ) |>
  gt_border_bars_bottom(
    bar_height = 7,
    colors = c("grey", "#580515", "black")
  ) |>
  gt_border_bars_top(
    bar_height = 4,
    colors = c("black", "#580515", "grey")
  ) |>
  tab_header(
    subtitle = "2nd Seed (FRB)",
    title = ""
  ) |>
  tab_options(
    heading.align = "center",
    heading.subtitle.font.size = px(30)
  ) |>
  cols_label(
    team ~ "Team"
  ) |>
  cols_width(
    team ~ px(150),
    PF ~ px(180)
  ) |>
  tab_style(
    style = list(cell_text(color = "black")),
    locations = cells_column_labels(
      columns = c(team, PF)
    )
  ) |>
  tab_style(
    style = list(cell_fill(color = "gold")),
    locations = cells_body(
      rows = 1
    )
  ) |>
  tab_style(
    style = list(cell_text(weight = "bold")),
    locations = cells_body(
      columns = team,
    )
  ) |>
  text_transform(
    locations = cells_body(),
    fn = function(x) {
      ifelse(is.na(x), "", x)
    }
  )

table3 <- three_seed_table |>
  gt() |>
  gt_theme_pl() |>
  tab_style(
    style = cell_text(align = "center"),
    locations = cells_column_labels(
      c(team, record, tb_h2h_record, PF)
    )
  ) |>
  cols_align(
    align = c("center"),
    columns = c(team, record, tb_h2h_record, PF)
  ) |>
  gt_border_bars_bottom(
    bar_height = 7,
    colors = c("grey", "#580515", "black")
  ) |>
  gt_border_bars_top(
    bar_height = 4,
    colors = c("black", "#580515", "grey")
  ) |>
  tab_header(
    subtitle = "3rd Seed",
    title = ""
  ) |>
  tab_options(
    heading.align = "center",
    heading.subtitle.font.size = px(30)
  ) |>
  cols_label(
    team ~ "Team",
    record ~ "Record",
    tb_h2h_record ~ "TB Record"
  ) |>
  cols_width(
    team ~ px(150),
    record ~ px(60),
    tb_h2h_record ~ px(60),
    PF ~ px(60)
  ) |>
  tab_style(
    style = list(cell_text(color = "black")),
    locations = cells_column_labels(
      columns = c(team, record, tb_h2h_record, PF)
    )
  ) |>
  tab_style(
    style = list(cell_fill(color = "gold")),
    locations = cells_body(
      rows = 1
    )
  ) |>
  tab_style(
    style = list(cell_text(weight = "bold")),
    locations = cells_body(
      columns = team,
    )
  ) |>
  text_transform(
    locations = cells_body(),
    fn = function(x) {
      ifelse(is.na(x), "", x)
    }
  )

table4 <- four_seed_table |>
  gt() |>
  gt_theme_pl() |>
  tab_style(
    style = cell_text(align = "center"),
    locations = cells_column_labels(
      c(team, record, tb_h2h_record, PF)
    )
  ) |>
  cols_align(
    align = c("center"),
    columns = c(team, record, tb_h2h_record, PF)
  ) |>
  gt_border_bars_bottom(
    bar_height = 7,
    colors = c("grey", "#580515", "black")
  ) |>
  gt_border_bars_top(
    bar_height = 4,
    colors = c("black", "#580515", "grey")
  ) |>
  tab_header(
    subtitle = "4th Seed",
    title = ""
  ) |>
  tab_options(
    heading.align = "center",
    heading.subtitle.font.size = px(30)
  ) |>
  cols_label(
    team ~ "Team",
    record ~ "Record",
    tb_h2h_record ~ "TB Record"
  ) |>
  cols_width(
    team ~ px(150),
    record ~ px(60),
    tb_h2h_record ~ px(60),
    PF ~ px(60)
  ) |>
  tab_style(
    style = list(cell_text(color = "black")),
    locations = cells_column_labels(
      columns = c(team, record, tb_h2h_record, PF)
    )
  ) |>
  tab_style(
    style = list(cell_fill(color = "gold")),
    locations = cells_body(
      rows = 1
    )
  ) |>
  tab_style(
    style = list(cell_text(weight = "bold")),
    locations = cells_body(
      columns = team,
    )
  ) |>
  text_transform(
    locations = cells_body(),
    fn = function(x) {
      ifelse(is.na(x), "", x)
    }
  )

table5 <- five_seed_table |>
  gt() |>
  gt_theme_pl() |>
  tab_style(
    style = cell_text(align = "center"),
    locations = cells_column_labels(
      c(team, l7_wins, l7_PF)
    )
  ) |>
  cols_align(
    align = c("center"),
    columns = c(team, l7_wins, l7_PF)
  ) |>
  gt_border_bars_bottom(
    bar_height = 7,
    colors = c("grey", "#580515", "black")
  ) |>
  gt_border_bars_top(
    bar_height = 4,
    colors = c("black", "#580515", "grey")
  ) |>
  tab_header(
    subtitle = "5th Seed",
    title = ""
  ) |>
  tab_options(
    heading.align = "center",
    heading.subtitle.font.size = px(30)
  ) |>
  cols_label(
    team ~ "Team",
    l7_wins ~ "L7 Wins", 
    l7_PF ~ "L7 PF"
  ) |>
  cols_width(
    team ~ px(150),
    l7_wins ~ px(90), 
    l7_PF ~ px(90)
  ) |>
  tab_style(
    style = list(cell_text(color = "black")),
    locations = cells_column_labels(
      columns = c(team, l7_wins, l7_PF)
    )
  ) |>
  tab_style(
    style = list(cell_fill(color = "gold")),
    locations = cells_body(
      rows = 1
    )
  ) |>
  tab_style(
    style = list(cell_text(weight = "bold")),
    locations = cells_body(
      columns = team,
    )
  ) |>
  text_transform(
    locations = cells_body(),
    fn = function(x) {
      ifelse(is.na(x), "", x)
    }
  )


table6 <- six_seed_table |>
  gt() |>
  gt_theme_pl() |>
  tab_style(
    style = cell_text(align = "center"),
    locations = cells_column_labels(
      c(team, PF)
    )
  ) |>
  cols_align(
    align = c("center"),
    columns = c(team, PF)
  ) |>
  gt_border_bars_bottom(
    bar_height = 7,
    colors = c("grey", "#580515", "black")
  ) |>
  gt_border_bars_top(
    bar_height = 4,
    colors = c("black", "#580515", "grey")
  ) |>
  tab_header(
    subtitle = "6th Seed",
    title = ""
  ) |>
  tab_options(
    heading.align = "center",
    heading.subtitle.font.size = px(30)
  ) |>
  cols_label(
    team ~ "Team"
  ) |>
  cols_width(
    team ~ px(150),
    PF ~ px(180)
  ) |>
  tab_style(
    style = list(cell_text(color = "black")),
    locations = cells_column_labels(
      columns = c(team, PF)
    )
  ) |>
  tab_style(
    style = list(cell_fill(color = "gold")),
    locations = cells_body(
      rows = 1
    )
  ) |>
  tab_style(
    style = list(cell_text(weight = "bold")),
    locations = cells_body(
      columns = team,
    )
  )


gtsave(table1, "scripts/table1.png") 
gtsave(table2, "scripts/table2.png") 
gtsave(table3, "scripts/table3.png") 
gtsave(table4, "scripts/table4.png") 
gtsave(table5, "scripts/table5.png") 
gtsave(table6, "scripts/table6.png") 


pp <- ggplot() +
  theme_void() +
  draw_image("scripts\\table1.png", x=0, y=0, width = 3, scale = 2) +
  draw_image("scripts\\table2.png", x=.5, y=0, width = 3, scale = 2) +
  draw_image("scripts\\table3.png", x=0, y=-2, width = 3, scale = 2.1) +
  draw_image("scripts\\table4.png", x=.5, y=-2, width = 3, scale = 2.1) +
  draw_image("scripts\\table5.png", x=0, y=-4, width = 3, scale = 2) +
  draw_image("scripts\\table6.png", x=.5, y=-3.7, width = 3, scale = 1.3) +
  xlim(1.2, 3.05) + ylim(-5, 1.3) +
  annotate("text", 
           x = 2.7, 
           y = 0, 
           label = "Playoff Picture",
           color = "black",
           fontface = "bold",
           size = 5
  ) +
  draw_image("https://i.ibb.co/fSTLLVQ/BlackPNG.png", x=2.2, y=.38) +
  geom_segment(aes(x = 2.3, y = -1, xend = 2.6, yend = -1), linewidth = 1, color = "#580515") +
  geom_segment(aes(x = 2.3, y = -1.6, xend = 2.6, yend = -1.6), linewidth = 1, color = "#580515") +
  geom_segment(aes(x = 2.3, y = -3.6, xend = 2.6, yend = -3.6), linewidth = 1, color = "#580515") +
  geom_segment(aes(x = 2.3, y = -4.2, xend = 2.6, yend = -4.2), linewidth = 1, color = "#580515") +
  geom_segment(aes(x = 2.6, y = -.97, xend = 2.6, yend = -1.63), linewidth = 1, color = "#580515") +
  geom_segment(aes(x = 2.6, y = -3.57, xend = 2.6, yend = -4.23), linewidth = 1, color = "#580515") +
  geom_segment(aes(x = 2.6, y = -1.3, xend = 2.8, yend = -1.3), linewidth = 1, color = "#580515") +
  geom_segment(aes(x = 2.6, y = -3.9, xend = 2.8, yend = -3.9), linewidth = 1, color = "#580515") +
  geom_segment(aes(x = 2.8 , y = -1.27 , xend = 2.8 , yend = -2.13), linewidth = 1, color = "#580515") +
  geom_segment(aes(x = 2.8, y = -2.1, xend = 2.57, yend = -2.1), linewidth = 1, color = "#580515") +
  geom_segment(aes(x = 2.8 , y = -3.93 , xend = 2.8 , yend = -3.08), linewidth = 1, color = "#580515") +
  geom_segment(aes(x = 2.8, y = -3.1, xend = 2.57, yend = -3.1), linewidth = 1, color = "#580515") +
  geom_segment(aes(x = 2.8, y = -2.6, xend = 3.05, yend = -2.6), linewidth = 1, color = "gold") +
  annotate("text", 
           x = 2.7, 
           y = -2.3, 
           label = qualified[[1]],
           color = "#3b3b3b",
           fontface = "bold",
           size = 2) +
  annotate("text", 
           x = 2.7, 
           y = -2.9, 
           label = qualified[[2]],
           color = "#3b3b3b",
           fontface = "bold",
           size = 2) +
  annotate("text", 
           x = 2.45, 
           y = -3.4, 
           label = qualified[[3]],
           color = "#3b3b3b",
           fontface = "bold",
           size = 2) +
  annotate("text", 
           x = 2.45, 
           y = -4.4, 
           label = qualified[[6]],
           color = "#3b3b3b",
           fontface = "bold",
           size = 2) +
  annotate("text", 
           x = 2.45, 
           y = -1.8, 
           label = qualified[[4]],
           color = "#3b3b3b",
           fontface = "bold",
           size = 2) +
  annotate("text", 
           x = 2.45, 
           y = -.8, 
           label = qualified[[5]],
           color = "#3b3b3b",
           fontface = "bold",
           size = 2) +
  annotate("text", 
           x = 2.7, 
           y = -.45, 
           label = paste0("After Week ", weeks_played),
           color = "black",
           fontface = "bold",
           size = 2)

ggsave(paste0("~/Fantasy/2024/playoff_picture/pp_wk", weeks_played, ".jpeg"), pp)  

file.remove("scripts/table1.png") 
file.remove("scripts/table2.png")
file.remove("scripts/table3.png")
file.remove("scripts/table4.png")
file.remove("scripts/table5.png")
file.remove("scripts/table6.png")
