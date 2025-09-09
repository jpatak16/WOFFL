library(pacman)
p_load(dplyr, readxl, janitor, tidyr)

runs = 20000

#create variable for season we want to simulate the rest of
CS = 2025
#import data sets
SeasonGames = read.csv(
    "WOFFL_stats_portal/AllGames.csv"
  ) %>%
  clean_names() %>%
  filter(
    season==CS
  ) %>%
  #remove extra things we don't need
  select(
    week, team, score, opponent, opponent_score
  ) %>%
  mutate(
    result = case_when(
      score > opponent_score ~ 1,
      opponent_score > score ~ 0,
      score == opponent_score ~ .5
    ),
    score = ifelse(score==0, NA, score),
    opponent_score = ifelse(opponent_score==0, NA, opponent_score),
    result = ifelse(is.na(score), NA, result)
  )

AllGames = read.csv(
    "WOFFL_stats_portal/AllGames.csv"
  ) %>%
  clean_names()

progress_bar = txtProgressBar(min=0, max=runs, style = 3, char="=")

simulate_season = function(n){
  
  weeks_played = if(
    # If weeks with scores inputted is greater than 13
    SeasonGames %>% 
      filter(
        !is.na(score)
      ) %>% 
      select(
        week
      ) %>% 
      unique() %>% 
      nrow() %>% 
      as.numeric() <= 13 
    ){
    SeasonGames %>% 
      filter(
        !is.na(score)
      ) %>% 
      select(week) %>% 
      unique() %>% 
      nrow() %>% 
      as.numeric()
  } else{13}
  
  weeks_remaining = 13 - weeks_played
  total_weeks = weeks_played + weeks_remaining
  
  #get cs stats
  team_stats = SeasonGames %>% 
    filter(
      !is.na(score)
    ) %>% 
    group_by(team) %>% 
    summarise(
      cs_mean = mean(score), cs_sd = sd(score)
    ) %>%
    right_join(
      SeasonGames %>% 
        select(team) %>% 
        unique(), 
      by = 'team'
    ) %>%
    filter(
      !is.na(team),
      team != "Ghost of Dakota Frantum",
      team != ""
    ) %>% 
    mutate(
      cs_mean = ifelse(is.na(cs_mean), 100, cs_mean),
      cs_sd = ifelse(is.na(cs_sd), 1, cs_sd)
    )
  
  # Add empty columns for every regular season week
  for(w in 1:total_weeks){
    
    colnamenew = paste("wk", w, sep = '')
    
    team_stats = team_stats %>% 
      mutate(
        colname = NA
      ) %>% 
      rename(
        !!colnamenew := colname
      )
  }
  
  # Fill in the score for each team for weeks already played
  for(t in 1:nrow(team_stats)){
    
    if(weeks_played == 0){next}
    
    TEAM = team_stats[[t, 1]]
    
    team_games = SeasonGames %>% filter(team==TEAM)
    
    for(wp in 1:weeks_played){
      team_stats[t,wp+3] = team_games[wp, 3]}
  }
  
  # Get stats from older seasons and l5 games
  hist_stats = AllGames %>% 
    filter(
      season<CS
    ) %>% 
    group_by(
      team
    ) %>% 
    summarise(
      hist_mean = mean(score, na.rm=T), 
      hist_sd = sd(score, na.rm=T)
    )
  
  l5_stats = SeasonGames %>% 
    filter(
      week > weeks_played-5,
      week <= weeks_played
    ) %>%
    group_by(
      team
    ) %>% 
    summarise(
      l5_mean = mean(score, na.rm = T), 
      l5_sd = sd(score, na.rm=T)
    ) %>%
    mutate(
      team = as.character(team)
    ) %>%
    right_join(
      SeasonGames %>% 
        select(team) %>% 
        unique(), 
      by = 'team'
    ) %>%
    mutate(
      l5_mean = ifelse(is.na(l5_mean), 0, l5_mean),
      l5_sd = ifelse(is.na(l5_sd), 1, l5_sd)
    )
  
  team_stats = team_stats %>% 
    left_join(
      hist_stats, 
      by='team'
    ) %>% 
    left_join(
      l5_stats, 
      by = 'team'
    )
  
  #get a ts df for the po
  team_stats_po = team_stats %>%
    select(
      -c(wk1:wk13)
    ) %>%
    mutate(
      wk14 = NA, 
      wk15 = NA, 
      wk16 = NA
    )
  
  # Fill in the score for each team for playoff weeks already played
  for(t in team_stats_po$team){
    
    team_games = SeasonGames %>% 
      filter(
        team == t
      )
    
    for(wp in 14:16){
      
      if(team_games %>% 
         filter(
           week == wp
         ) %>% 
         nrow() == 0){next}
      
      team_stats_po[t,wp-6] = team_games[wp, 3]
    }
  }
  
  #empty df to fill later
  all_sims = data.frame(
    sim_num = NA, 
    team=NA, 
    proj_PF=NA, 
    proj_wins=NA, 
    playoffs=NA, 
    FRB=NA, 
    champion=NA,
    second = NA, 
    third = NA, 
    fourth = NA, 
    fifth = NA, 
    sixth = NA, 
    seventh = NA, 
    eigth = NA,
    ninth = NA, 
    tenth = NA, 
    eleventh = NA
    ) 
  
  all_sims = all_sims[-1,]
  
  for(i in 1:n){
    #update the progress bar
    setTxtProgressBar(progress_bar, value = i)
    
    #set weight put on each part of means and sd
    weight_cs = case_when(
      weeks_played == 0 ~ weeks_played/55,
      weeks_played %in% c(1:5) ~ sum(1:weeks_played) / 55,
      weeks_played >=7  ~ .4545,
      .default = (15+(5*(weeks_played-5))) / 55
    )
    
    weight_all = case_when(
      weeks_played <= 5 ~ .7,
      weeks_played >= 10 ~ .34,
      weeks_played >= 8 ~ .4,
      .default = .55
    )
    
    weight_l5 = case_when(
      weeks_played < 8 ~ 0,
      weeks_played >= 10 ~ .16,
      .default = .10
    )
    
    weight_hist = ifelse(
      1 - weight_cs - weight_all - weight_l5 < 0, 
      0, 
      1 - weight_cs - weight_all - weight_l5
    )
    
    
    #simulate scores for rs
    ts_sim_i = team_stats %>%
      mutate(
        across(everything(), 
               ~ ifelse(is.na(.), 
                        weight_cs*rnorm(n(), cs_mean, cs_sd) + 
                          weight_hist*rnorm(n(), hist_mean, hist_sd) + 
                          weight_l5*rnorm(n(), l5_mean, l5_sd) + 
                          weight_all*rnorm(n(), 95.7, 22.8),
                        .
        )))
    
    ts_sim_po_i = team_stats_po %>%
      mutate(
        across(everything(),
               ~ ifelse(is.na(.), 
                        weight_cs*rnorm(n(), cs_mean, cs_sd) + 
                          weight_hist*rnorm(n(), hist_mean, hist_sd) + 
                          weight_l5*rnorm(n(), l5_mean, l5_sd) + 
                          weight_all*rnorm(n(), 95.7, 22.8),
                        .
        )))
    
    #add weekly scores into matchups df
    ts_piv = ts_sim_i %>% 
      select(
        !c(cs_mean, cs_sd, hist_mean, hist_sd, l5_mean, l5_sd)
      ) %>% 
      pivot_longer(!team) %>% 
      rename(
        week = name, 
        score = value
      ) %>%
      mutate(
        week = gsub("[A-z]", "", week) %>% 
          as.numeric()
      )
    
    #calculate weekly ghost score
    ghost_wk_score = ts_piv %>%
      anti_join(
        SeasonGames %>% 
          filter(
            team=="Ghost of Dakota Frantum"
          ) %>% 
          select(
            week, 
            team = opponent
          ), 
        by = c('week', 'team')
      ) %>%
      group_by(week) %>%
      summarise(
        score = mean(score)
      ) %>% 
      mutate(
        team = "Ghost of Dakota Frantum"
      )
    
    ts_piv = rbind(ts_piv, ghost_wk_score)
    
    sim_season = left_join(
        SeasonGames, 
        ts_piv, 
        by=c('team', 'week')
      ) %>% 
      filter(
        week <= total_weeks
      ) %>%
      select(
        week, 
        team, 
        score.y, 
        opponent, 
        opponent_score, 
        result
      ) %>% 
      rename(
        score=score.y
      ) %>%
      left_join(
        ts_piv, 
        by=c('opponent' = 'team', 'week' = 'week')
      ) %>%
      select(
        week, 
        team, 
        score.x, 
        opponent, 
        score.y, 
        result
      ) %>% 
      rename(
        score = score.x, 
        opponent_score = score.y
      ) %>%
      mutate(
        result = case_when(
          score > opponent_score ~ 1,
          opponent_score > score ~ 0,
          score == opponent_score ~ .5
        )
      )
    
    next_wk_wins = sim_season |>
      filter(
        week == weeks_played + 1
      ) |>
      select(
        team, result
      ) |>
      rename(
        win_nxt_game = result
      )
    
    #summarise all simulated matchups by summing wins and pts by team
    season_i_res = sim_season %>% 
      group_by(team) %>% 
      summarise(
        wins = sum(result), 
        PF = sum(score)
      ) %>%
      filter(
        team != "Ghost of Dakota Frantum"
      )
    
    #decide the tiebreaker for rs champ
    n_wins_tie_1 = season_i_res %>% 
      arrange(
        desc(wins)
      ) %>% 
      filter(
        row_number() == 1
      ) %>% 
      select(wins) %>% 
      as.numeric()
    
    teams_tie_1 = season_i_res %>% 
      arrange(
        desc(wins)
      ) %>% 
      filter(
        row_number() %in% 1:(season_i_res %>% 
                               arrange(desc(wins)) %>% 
                               filter(wins == n_wins_tie_1) %>% 
                               select(team) %>% 
                               nrow()
                             )
      ) %>% 
      select(team) %>% 
      as.vector() %>% 
      unlist()
    
    season_i_res = season_i_res %>%
      left_join(
        sim_season %>% 
          filter(
            team %in% teams_tie_1,
            opponent %in% teams_tie_1
          ) %>% 
          group_by(team) %>% 
          summarise(
            tb_h2h_wins = sum(result), 
            tb_h2h_gp = n()
          ),
        by = 'team'
      ) %>%
      mutate(
        tb_h2h_wp = ifelse(
          is.na(tb_h2h_gp), 
          1, 
          tb_h2h_wins / tb_h2h_gp
        )
      ) %>%
      arrange(
        desc(wins), 
        desc(tb_h2h_wp), 
        desc(PF)
      ) %>%
      mutate(
        rs_champs = ifelse(
          row_number() == 1, 
          1, 
          0
        )
      )
    
    #decide tiebreaker for rs ru
    rs_ru_tab = season_i_res %>%
      filter(
        rs_champs != 1
      ) %>%
      arrange(
        desc(PF)
      ) %>%
      mutate(
        rs_ru = ifelse(
          row_number() == 1, 
          1, 
          0
        )
      )
    
    season_i_res = left_join(
        season_i_res, 
        rs_ru_tab
      ) %>%
      suppressMessages() %>%
      mutate(
        frb = ifelse(
          rs_champs == 1 | rs_ru == 1, 
          1, 
          0
        )
      )
    
    #decide playoff team 3
    n_wins_tie_3 = season_i_res %>% 
      filter(
        rs_champs != 1, 
        rs_ru != 1
      ) %>% 
      arrange(
        desc(wins)
      ) %>% 
      filter(
        row_number() == 1
      ) %>% 
      select(wins) %>% 
      as.numeric()
    
    teams_tie_3 = season_i_res %>% 
      filter(
        rs_champs != 1, 
        rs_ru != 1
      ) %>% 
      arrange(
        desc(wins)
      ) %>% 
      filter(
        row_number() %in% 1:(season_i_res %>% 
                               filter(
                                 rs_champs != 1, 
                                 rs_ru != 1
                                ) %>% 
                               arrange(
                                 desc(wins)
                                ) %>% 
                               filter(
                                 wins == n_wins_tie_3
                                ) %>% 
                               select(team) %>% 
                               nrow()
                             )
        ) %>% 
      select(team) %>% 
      as.vector() %>% 
      unlist()
    
    po_sd3_tab = season_i_res %>%
      left_join(
        sim_season %>% 
          filter(
            team %in% teams_tie_3,
            opponent %in% teams_tie_3
          ) %>% 
          group_by(team) %>% 
          summarise(
            tb_h2h_wins_3 = sum(result), 
            tb_h2h_gp_3 = n()
          ),
        by = 'team'
      ) %>%
      mutate(
        tb_h2h_wp_3 = ifelse(
          is.na(tb_h2h_gp_3), 
          1, 
          tb_h2h_wins_3 / tb_h2h_gp_3)
      ) %>%
      arrange(
        desc(wins), 
        desc(tb_h2h_wp_3), 
        desc(PF)
      ) %>%
      filter(
        frb != 1
      ) %>%
      mutate(
        po_sd3 = ifelse(
          row_number() == 1,
          1,
          0
        )
      )
    
    season_i_res = left_join(
        season_i_res, 
        po_sd3_tab
      ) %>% 
      suppressMessages()
    
    #decide po team 4
    n_wins_tie_4 = season_i_res %>% 
      filter(
        rs_champs != 1, 
        rs_ru != 1, 
        po_sd3 != 1
      ) %>% 
      arrange(
        desc(wins)
      ) %>% 
      filter(
        row_number() == 1
      ) %>% 
      select(wins) %>% 
      as.numeric()
    
    teams_tie_4 = season_i_res %>% 
      filter(
        rs_champs != 1, 
        rs_ru != 1, 
        po_sd3 != 1
      ) %>% 
      arrange(
        desc(wins)
      ) %>% 
      filter(
        row_number() %in% 1:(season_i_res %>% 
                               filter(
                                 rs_champs != 1, 
                                 rs_ru != 1, 
                                 po_sd3 != 1
                                ) %>% 
                               arrange(
                                 desc(wins)
                                ) %>% 
                               filter(
                                 wins == n_wins_tie_4
                                ) %>% 
                               select(team) %>% 
                               nrow()
                            )
      ) %>% 
      select(team) %>% 
      as.vector() %>% 
      unlist()
    
    po_sd4_tab = season_i_res %>%
      left_join(
        sim_season %>% 
          filter(
            team %in% teams_tie_4,
            opponent %in% teams_tie_4
          ) %>% 
          group_by(team) %>% 
          summarise(
            tb_h2h_wins_4 = sum(result), 
            tb_h2h_gp_4 = n()
          ),
        by = 'team'
      ) %>%
      mutate(
        tb_h2h_wp_4 = ifelse(
          is.na(tb_h2h_gp_4), 
          1, 
          tb_h2h_wins_4 / tb_h2h_gp_4
        )
      ) %>%
      arrange(
        desc(wins), 
        desc(tb_h2h_wp_4), 
        desc(PF)
      ) %>%
      filter(
        frb != 1, 
        po_sd3 != 1
      ) %>%
      mutate(
        po_sd4 = ifelse(
          row_number() == 1, 
          1, 
          0
        )
      )
    
    season_i_res = left_join(
        season_i_res, 
        po_sd4_tab
      ) %>% 
      suppressMessages()
    
    #decide hf wc
    non_wc_po_tms = season_i_res %>% 
      filter(
        rs_champs == 1 | rs_ru == 1 | po_sd3 == 1 | po_sd4 == 1
      ) %>%
      select(team) %>% 
      as.vector() %>% 
      unlist()
    
    hf_wc_tab = sim_season %>% 
      filter(
        team != "Ghost of Dakota Frantum"
      ) %>% 
      filter(
        week > total_weeks-7
      ) %>% 
      filter(
        !team %in% non_wc_po_tms
      ) %>%
      group_by(team) %>%
      summarise(
        l7_wins = sum(result), 
        l7_PF = sum(score)
      ) %>%
      arrange(
        desc(l7_wins), 
        desc(l7_PF)
      ) %>%
      mutate(
        hf_wc = ifelse(
          row_number() == 1, 
          1, 
          0
        )
      )
    
    season_i_res = left_join(
        season_i_res, 
        hf_wc_tab
      ) %>% 
      suppressMessages()
    
    #decide pf wc
    season_i_res = season_i_res %>% 
      left_join(
        season_i_res %>% 
          filter(
            rs_champs != 1, 
            rs_ru != 1, 
            po_sd3 != 1, 
            po_sd4 != 1, 
            hf_wc != 1
          ) %>%
          arrange(
            desc(PF)
          ) %>%
          mutate(
            pts_wc = ifelse(
              row_number() == 1, 
              1, 
              0
            )
          )
      ) %>%
      suppressMessages() %>%
      mutate(
        playoffs = ifelse(
          frb == 1 | po_sd3 == 1 | po_sd4 == 1 | hf_wc == 1 | pts_wc == 1, 
          1, 
          0
        )
      ) %>%
      select(
        team, 
        wins, 
        PF, 
        rs_champs, 
        rs_ru, 
        frb, 
        po_sd3, 
        po_sd4, 
        hf_wc, 
        pts_wc, 
        playoffs
      )
    
    #list of po scores by team
    ts_piv_po = ts_sim_po_i %>% 
      select(
        !c(cs_mean, cs_sd, hist_mean, hist_sd, l5_mean, l5_sd)
      ) %>% 
      pivot_longer(!team) %>% 
      rename(
        week = name, 
        score = value
      ) %>%
      mutate(
        week = gsub("[A-z]", "", week) %>% 
          as.numeric()
      )
    
    #seed po teams
    po_seeds = season_i_res %>%
      filter(
        playoffs == 1, 
        frb != 1
      ) %>%
      arrange(
        desc(wins),
        desc(PF)
      ) %>%
      select(
        team, 
        wins, 
        PF
      )
    
    n_wins_seed3 = po_seeds %>% 
      arrange(
        desc(wins),
        desc(PF)
      ) %>% 
      filter(
        row_number() == 1
      ) %>% 
      select(wins) %>% 
      as.numeric()
    
    teams_tie_sd3 = po_seeds %>% 
      arrange(
        desc(wins)
      ) %>% 
      filter(
        row_number() %in% 1:(po_seeds %>% 
                               arrange(
                                 desc(wins)
                               ) %>% 
                               filter(
                                 wins == n_wins_seed3
                               ) %>% 
                               select(team) %>% 
                               nrow()
        )
      ) %>% 
      select(team) %>% 
      as.vector() %>% 
      unlist()
    
    po_sd3_tab = po_seeds %>%
      left_join(
        sim_season %>% 
          filter(
            team %in% teams_tie_sd3,
            opponent %in% teams_tie_sd3
          ) %>% 
          group_by(team) %>% 
          summarise(
            tb_h2h_seed3 = sum(result)
          ),
        by = 'team'
      ) %>%
      arrange(
        desc(wins), 
        desc(tb_h2h_seed3), 
        desc(PF)
      ) %>%
      mutate(
        seed3 = ifelse(
          row_number() == 1, 
          1, 
          0
        )
      )
    
    po_sd3 <- po_sd3_tab |>
      filter(
        row_number() == 1
      ) |>
      pull(team)
    
    n_wins_seed4 = po_seeds %>% 
      filter(
        team != po_sd3
      ) %>% 
      arrange(
        desc(wins),
        desc(PF)
      ) %>% 
      filter(
        row_number() == 1
      ) %>% 
      select(wins) %>% 
      as.numeric()
    
    teams_tie_sd4 = po_seeds %>% 
      filter(
        team != po_sd3
      ) %>% 
      arrange(
        desc(wins)
      ) %>% 
      filter(
        row_number() %in% 1:(po_seeds %>% 
                               filter(
                                 team != po_sd3
                               ) %>% 
                               arrange(
                                 desc(wins)
                               ) %>% 
                               filter(
                                 wins == n_wins_seed4
                               ) %>% 
                               select(team) %>% 
                               nrow()
        )
      ) %>% 
      select(team) %>% 
      as.vector() %>% 
      unlist()
    
    po_sd4_tab = po_seeds %>%
      left_join(
        sim_season %>% 
          filter(
            team %in% teams_tie_sd4,
            opponent %in% teams_tie_sd4
          ) %>% 
          group_by(team) %>% 
          summarise(
            tb_h2h_seed4 = sum(result)
          ),
        by = 'team'
      ) %>%
      arrange(
        desc(wins), 
        desc(tb_h2h_seed4), 
        desc(PF)
      ) %>%
      filter(
        team != po_sd3
      ) %>%
      mutate(
        seed4 = ifelse(
          row_number() == 1, 
          1, 
          0
        )
      )
    
    po_sd4 <- po_sd4_tab |>
      filter(
        row_number() == 1
      ) |>
      pull(team)
    
    n_wins_seed5 = po_seeds %>% 
      filter(
        team != po_sd3, 
        team != po_sd4
      ) %>% 
      arrange(
        desc(wins),
        desc(PF)
      ) %>% 
      filter(
        row_number() == 1
      ) %>% 
      select(wins) %>% 
      as.numeric()
    
    teams_tie_sd5 = po_seeds %>% 
      filter(
        team != po_sd3, 
        team != po_sd4
      ) %>% 
      arrange(
        desc(wins)
      ) %>% 
      filter(
        row_number() %in% 1:(po_seeds %>% 
                               filter(
                                 team != po_sd3, 
                                 team != po_sd4
                                ) %>% 
                               arrange(
                                 desc(wins)
                                ) %>% 
                               filter(
                                 wins == n_wins_seed5
                                ) %>% 
                               select(team) %>% 
                               nrow()
                             )
        ) %>% 
      select(team) %>% 
      as.vector() %>% 
      unlist()
    
    po_sd5_tab = po_seeds %>%
      left_join(
        sim_season %>% 
          filter(
            team %in% teams_tie_sd5,
            opponent %in% teams_tie_sd5
          ) %>% 
          group_by(team) %>% 
          summarise(
            tb_h2h_seed5 = sum(result)
          ),
        by = 'team'
      ) %>%
      arrange(
        desc(wins), 
        desc(tb_h2h_seed5), 
        desc(PF)
      ) %>%
      filter(
        team != po_sd3, 
        team != po_sd4
      ) %>%
      mutate(
        seed5 = ifelse(
          row_number() == 1, 
          1, 
          0
        )
      )
    
    po_seeds = left_join(
        po_seeds, 
        po_sd3_tab
      ) %>%
      left_join(
        po_sd4_tab
      ) |>
      left_join(
        po_sd5_tab
      ) |>
      suppressMessages() %>%
      mutate(
        seed6 = 1 - seed3 - seed4 - seed5
      ) %>%
      select(
        -tb_h2h_seed5, 
        -wins, 
        -PF
      ) 
    
    #set consol seeds
    consol_seeds = season_i_res %>%
      filter(
        playoffs != 1
      ) %>%
      arrange(
        desc(wins), 
        desc(PF)
      ) %>%
      select(team) %>% 
      mutate(
        seed7 = ifelse(row_number() == 1, 1, 0),
        seed8 = ifelse(row_number() == 2, 1, 0),
        seed9 = ifelse(row_number() == 3, 1, 0),
        seed10 = ifelse(row_number() == 4, 1, 0),
        seed11 = ifelse(row_number() == 5, 1, 0)
      )
    
    po_seeds = full_join(
        po_seeds, 
        consol_seeds
      ) %>% 
      suppressMessages() %>%
      full_join(
        season_i_res %>% 
          select(
            team, 
            rs_champs, 
            rs_ru
          )
      ) %>%
      suppressMessages() %>%
      rename(
        seed1 = rs_champs, 
        seed2 = rs_ru
      )
    
    
    #create rd1 matchups
    gm1 = data.frame(
      team1 = po_seeds %>% 
        filter(
          seed3 == 1
        ) %>% 
        select(
          team
        ) %>% 
        as.character(),
      score1 = NA,
      team2= po_seeds %>% 
        filter(
          seed6 == 1
        ) %>% 
        select(team) %>% 
        as.character(),
      score2 = NA,
      note = "Round 1, 3v6"
    )
    
    gm2 = data.frame(
      team1 = po_seeds %>% 
        filter(
          seed4 == 1
        ) %>% 
        select(team) %>% 
        as.character(),
      score1 = NA,
      team2= po_seeds %>% 
        filter(
          seed5 == 1
        ) %>% 
        select(team) %>% 
        as.character(),
      score2 = NA,
      note = "Round 1, 4v5"
    )
    
    consol_gm1 = data.frame(
      team1 = po_seeds %>% 
        filter(
          seed7 == 1
        ) %>% 
        select(team) %>% 
        as.character(),
      score1 = NA,
      team2= po_seeds %>% 
        filter(
          seed11 == 1
        ) %>% 
        select(team) %>% 
        as.character(),
      score2 = NA,
      note = "Consol Round 1, 7v11"
    )
    
    consol_gm2 = data.frame(
      team1 = po_seeds %>% 
        filter(
          seed8 == 1
        ) %>% 
        select(team) %>% 
        as.character(),
      score1 = NA,
      team2= po_seeds %>% 
        filter(
          seed9 == 1
        ) %>% 
        select(team) %>% 
        as.character(),
      score2 = NA,
      team3 = po_seeds %>% 
        filter(
          seed10 == 1
        ) %>% 
        select(team) %>% 
        as.character(),
      score3 = NA,
      note = "Consol Round 1, 8v9v10"
    )
    
    #get scores for rd1 matchups
    gm1 = gm1 %>%
      mutate(
        score1 = ts_sim_po_i %>% 
          filter(
            team == po_seeds %>% 
              filter(
                seed3 == 1
              ) %>% 
              select(team) %>% 
              as.character()
          ) %>% 
          select(wk14) %>% 
          as.numeric(),
        score2 = ts_sim_po_i %>% 
          filter(
            team == po_seeds %>% 
              filter(
                seed6 == 1
              ) %>% 
              select(team) %>% 
              as.character()
          ) %>% 
          select(wk14) %>% 
          as.numeric(),
        winner = ifelse(score1 > score2, team1, team2),
        loser = ifelse(score1 > score2, team2, team1))
    
    gm2 = gm2 %>%
      mutate(
        score1 = ts_sim_po_i %>% 
          filter(
            team == po_seeds %>% 
              filter(
                seed4 == 1
                ) %>% 
              select(team) %>% 
              as.character()
          ) %>% 
          select(wk14) %>% 
          as.numeric(),
        score2 = ts_sim_po_i %>% 
          filter(
            team == po_seeds %>% 
              filter(
                seed5 == 1
              ) %>% 
              select(team) %>% 
              as.character()
          ) %>% 
          select(wk14) %>% 
          as.numeric(),
        winner = ifelse(score1 > score2, team1, team2),
        loser = ifelse(score1 > score2, team2, team1))
    
    consol_gm1 = consol_gm1 %>%
      mutate(
        score1 = ts_sim_po_i %>% 
          filter(
            team == po_seeds %>% 
              filter(
                seed7 == 1
              ) %>% 
              select(team) %>% 
              as.character()
          ) %>% 
          select(wk14) %>% 
          as.numeric(),
        score2 = ts_sim_po_i %>% 
          filter(
            team == po_seeds %>% 
              filter(
                seed11 == 1
              ) %>% 
              select(team) %>% 
              as.character()
          ) %>% 
          select(wk14) %>% 
          as.numeric(),
        winner = ifelse(score1 > score2, team1, team2),
        loser = ifelse(score1 > score2, team2, team1))
    
    consol_gm2 = consol_gm2 %>%
      mutate(
        score1 = ts_sim_po_i %>% 
          filter(
            team == po_seeds %>% 
              filter(
                seed8 == 1
              ) %>% 
              select(team) %>% 
              as.character()
          ) %>% 
          select(wk14) %>% 
          as.numeric(),
        score2 = ts_sim_po_i %>% 
          filter(
            team == po_seeds %>% 
              filter(
                seed9 == 1
              ) %>% 
              select(team) %>% 
              as.character()
          ) %>% 
          select(wk14) %>% 
          as.numeric(),
        score3 = ts_sim_po_i %>% 
          filter(
            team == po_seeds %>% 
              filter(
                seed10 == 1
                ) %>% 
              select(team) %>% 
              as.character()
          ) %>% 
          select(wk14) %>% 
          as.numeric(),
        winner = case_when(
          score1 > score2 & score1 > score3 ~ team1,
          score2 > score1 & score2 > score3 ~ team2,
          score3 > score1 & score3 > score2 ~ team3),
        loser1 = case_when(
          score1 > score2 & score1 > score3 ~ team2,
          score2 > score1 & score2 > score3 ~ team1,
          score3 > score1 & score3 > score2 ~ team1),
        loser2 = case_when(
          score1 > score2 & score1 > score3 ~ team3,
          score2 > score1 & score2 > score3 ~ team3,
          score3 > score1 & score3 > score2 ~ team2)
        )
    
    #create rd2 matchups
    gm3 = data.frame(
      team1 = po_seeds %>% 
        filter(
          seed2 == 1
        ) %>% 
        select(team) %>% 
        as.character(),
      score1 = NA,
      team2= gm1 %>% 
        select(winner) %>% 
        as.character(),
      score2 = NA,
      note = "Round 2, 2v3/6"
    )
    
    gm4 = data.frame(
      team1 = po_seeds %>% 
        filter(
          seed1 == 1
        ) %>% 
        select(team) %>% 
        as.character(),
      score1 = NA,
      team2= gm2 %>% 
        select(winner) %>% 
        as.character(),
      score2 = NA,
      note = "Round 2, 1v4/5"
    )
    
    consol_gm3 = data.frame(
      team1 = gm1 %>% 
        select(loser) %>% 
        as.character(),
      score1 = NA,
      team2= consol_gm1 %>% 
        select(winner) %>% 
        as.character(),
      score2 = NA,
      note = "Consol Round 2, 3/6v7/11"
    )
    
    consol_gm4 = data.frame(
      team1 = gm2 %>% 
        select(loser) %>% 
        as.character(),
      score1 = NA,
      team2= consol_gm2 %>% 
        select(winner) %>% 
        as.character(),
      score2 = NA,
      note = "Consol Round 2, 4/5v8/9/10"
    )
    
    consol_gm5 = data.frame(
      team1 = consol_gm1 %>% 
        select(loser) %>% 
        as.character(),
      score1 = NA,
      team2= consol_gm2 %>% 
        select(loser1) %>% 
        as.character(),
      score2 = NA,
      team3 = consol_gm2 %>% 
        select(loser2) %>% 
        as.character(),
      score3 = NA,
      note = "9th Place Match"
    )
    
    #get scores for rd2 matchups
    gm3 = gm3 %>%
      mutate(
        score1 = ts_sim_po_i %>% 
          filter(
            team == po_seeds %>% 
              filter(
                seed2 == 1
              ) %>% 
              select(team) %>% 
              as.character()
          ) %>% 
          select(wk15) %>% 
          as.numeric(),
        score2 = ts_sim_po_i %>% 
          filter(
            team == gm1 %>% 
              select(winner) %>% 
              as.character()
          ) %>% 
          select(wk15) %>% 
          as.numeric(),
        winner = ifelse(score1 > score2, team1, team2),
        loser = ifelse(score1 > score2, team2, team1)
      )
    
    gm4 = gm4 %>%
      mutate(
        score1 = ts_sim_po_i %>% 
          filter(
            team == po_seeds %>% 
              filter(
                seed1 == 1
              ) %>% 
              select(team) %>% 
              as.character()
          ) %>% 
          select(wk15) %>% 
          as.numeric(),
        score2 = ts_sim_po_i %>% 
          filter(
            team == gm2 %>% 
              select(winner) %>% 
              as.character()
          ) %>% 
          select(wk15) %>% 
          as.numeric(),
        winner = ifelse(score1 > score2, team1, team2),
        loser = ifelse(score1 > score2, team2, team1))
    
    consol_gm3 = consol_gm3 %>%
      mutate(
        score1 = ts_sim_po_i %>% 
          filter(
            team == gm1 %>% 
              select(loser) %>% 
              as.character()
          ) %>% 
          select(wk15) %>% 
          as.numeric(),
             score2 = ts_sim_po_i %>% 
          filter(
            team == consol_gm1 %>% 
              select(winner) %>% 
              as.character()
          ) %>% 
          select(wk15) %>% 
          as.numeric(),
        winner = ifelse(score1 > score2, team1, team2),
        loser = ifelse(score1 > score2, team2, team1)
      )
    
    consol_gm4 = consol_gm4 %>%
      mutate(
        score1 = ts_sim_po_i %>% 
          filter(
            team == gm2 %>% 
              select(loser) %>% 
              as.character()
          ) %>% 
          select(wk15) %>% 
          as.numeric(),
        score2 = ts_sim_po_i %>% 
          filter(
            team == consol_gm2 %>% 
              select(winner) %>% 
              as.character()
          ) %>% 
          select(wk15) %>% 
          as.numeric(),
        winner = ifelse(score1 > score2, team1, team2),
        loser = ifelse(score1 > score2, team2, team1))
    
    consol_gm5 = consol_gm5 %>%
      mutate(
        score1 = ts_sim_po_i %>% 
          filter(
            team == consol_gm1 %>% 
              select(loser) %>% 
              as.character()
          ) %>% 
          select(wk15) %>% 
          as.numeric(),
        score2 = ts_sim_po_i %>% 
          filter(
            team == consol_gm2 %>% 
              select(loser1) %>% 
              as.character()
          ) %>% 
          select(wk15) %>% 
          as.numeric(),
        score3 = ts_sim_po_i %>% 
          filter(
            team == consol_gm2 %>% 
              select(loser2) %>% 
              as.character()
          ) %>% 
          select(wk15) %>% 
          as.numeric(),
        winner = case_when(
          score1 > score2 & score1 > score3 ~ team1,
          score2 > score1 & score2 > score3 ~ team2,
          score3 > score1 & score3 > score2 ~ team3
        ),
        loser1 = case_when(
          score1 > score2 & score1 > score3 ~ team2,
          score2 > score1 & score2 > score3 ~ team1,
          score3 > score1 & score3 > score2 ~ team1
        ),
        loser2 = case_when(
          score1 > score2 & score1 > score3 ~ team3,
          score2 > score1 & score2 > score3 ~ team3,
          score3 > score1 & score3 > score2 ~ team2
        )
      )
    
    #create rd3 matchups
    gm5 = data.frame(
      team1 = gm3 %>% 
        select(winner) %>% 
        as.character(),
      score1 = NA,
      team2= gm4 %>% 
        select(winner) %>% 
        as.character(),
      score2 = NA,
      note = "Championship Game"
    )
    
    gm6 = data.frame(
      team1 = gm3 %>% 
        select(loser) %>% 
        as.character(),
      score1 = NA,
      team2= gm4 %>% 
        select(loser) %>% 
        as.character(),
      score2 = NA,
      note = "Third Place Game"
    )
    
    consol_gm6 = data.frame(
      team1 = consol_gm3 %>% 
        select(winner) %>% 
        as.character(),
      score1 = NA,
      team2= consol_gm4 %>% 
        select(winner) %>% 
        as.character(),
      score2 = NA,
      note = "Consol Championship"
    )
    
    consol_gm7 = data.frame(
      team1 = consol_gm3 %>% 
        select(loser) %>% 
        as.character(),
      score1 = NA,
      team2= consol_gm4 %>% 
        select(loser) %>% 
        as.character(),
      score2 = NA,
      note = "7th Place Game"
    )
    
    consol_gm8 = data.frame(
      team1 = consol_gm5 %>% 
        select(loser1) %>% 
        as.character(),
      score1 = NA,
      team2= consol_gm5 %>% 
        select(loser2) %>% 
        as.character(),
      score2 = NA,
      note = "Last Place Game"
    )
    
    #get scores for rd3 matchups
    gm5 = gm5 %>%
      mutate(
        score1 = ts_sim_po_i %>% 
          filter(
            team == gm3 %>% 
              select(winner) %>% 
              as.character()
          ) %>% 
          select(wk16) %>% 
          as.numeric(),
        score2 = ts_sim_po_i %>% 
          filter(
            team == gm4 %>% 
              select(winner) %>% 
              as.character()
          ) %>% 
          select(wk16) %>% 
          as.numeric(),
        winner = ifelse(score1 > score2, team1, team2),
        loser = ifelse(score1 > score2, team2, team1)
      )
    
    gm6 = gm6 %>%
      mutate(
        score1 = ts_sim_po_i %>% 
          filter(
            team == gm3 %>% 
              select(loser) %>% 
              as.character()
          ) %>% 
          select(wk16) %>% 
          as.numeric(),
        score2 = ts_sim_po_i %>% 
          filter(
            team == gm4 %>% 
              select(loser) %>% 
              as.character()
            ) %>% 
          select(wk16) %>% 
          as.numeric(),
        winner = ifelse(score1 > score2, team1, team2),
        loser = ifelse(score1 > score2, team2, team1)
      )
    
    consol_gm6 = consol_gm6 %>%
      mutate(
        score1 = ts_sim_po_i %>% 
          filter(
            team == consol_gm3 %>% 
              select(winner) %>% 
              as.character()
          ) %>% 
          select(wk16) %>% 
          as.numeric(),
        score2 = ts_sim_po_i %>% 
          filter(
            team == consol_gm4 %>% 
              select(winner) %>% 
              as.character()
          ) %>% 
          select(wk16) %>% 
          as.numeric(),
        winner = ifelse(score1 > score2, team1, team2),
        loser = ifelse(score1 > score2, team2, team1)
      )
    
    consol_gm7 = consol_gm7 %>%
      mutate(
        score1 = ts_sim_po_i %>% 
          filter(
            team == consol_gm3 %>% 
              select(loser) %>% 
              as.character()
          ) %>% 
          select(wk16) %>% 
          as.numeric(),
        score2 = ts_sim_po_i %>% 
          filter(
            team == consol_gm4 %>% 
              select(loser) %>% 
              as.character()
          ) %>% 
          select(wk16) %>% 
          as.numeric(),
        winner = ifelse(score1 > score2, team1, team2),
        loser = ifelse(score1 > score2, team2, team1)
      )
    
    consol_gm8 = consol_gm8 %>%
      mutate(
        score1 = ts_sim_po_i %>% 
          filter(
            team == consol_gm5 %>% 
              select(loser1) %>% 
              as.character()
            ) %>% 
          select(wk16) %>% 
          as.numeric(),
        score2 = ts_sim_po_i %>% 
          filter(
            team == consol_gm5 %>% 
              select(loser2) %>% 
              as.character()
          ) %>% 
          select(wk16) %>% 
          as.numeric(),
        winner = ifelse(score1 > score2, team1, team2),
        loser = ifelse(score1 > score2, team2, team1))
    
    #create a var for each place a player gets
    first = gm5 %>% 
      select(winner) %>% 
      as.character()
    second = gm5 %>% 
      select(loser) %>% 
      as.character()
    third = gm6 %>% 
      select(winner) %>% 
      as.character()
    fourth = gm6 %>% 
      select(loser) %>% 
      as.character()
    fifth = consol_gm6 %>% 
      select(winner) %>% 
      as.character()
    sixth = consol_gm6 %>% 
      select(loser) %>% 
      as.character()
    seventh = consol_gm7 %>% 
      select(winner) %>% 
      as.character()
    eigth = consol_gm7 %>% 
      select(loser) %>% 
      as.character()
    ninth = consol_gm5 %>% 
      select(winner) %>% 
      as.character()
    tenth = consol_gm8 %>% 
      select(winner) %>% 
      as.character()
    eleventh = consol_gm8 %>% 
      select(loser) %>% 
      as.character()
    
    season_i_res = season_i_res %>%
      mutate(
        sim_num = i,
        champion = ifelse(team == first, 1, 0),
        second = ifelse(team == second, 1, 0),
        third = ifelse(team == third, 1, 0),
        fourth = ifelse(team == fourth, 1, 0),
        fifth = ifelse(team == fifth, 1, 0),
        sixth = ifelse(team == sixth, 1, 0),
        seventh = ifelse(team == seventh, 1, 0),
        eigth = ifelse(team == eigth, 1, 0),
        ninth = ifelse(team == ninth, 1, 0),
        tenth = ifelse(team == tenth, 1, 0),
        eleventh = ifelse(team == eleventh, 1, 0)) %>%
      select(
        sim_num, 
        team, 
        wins, 
        PF, 
        playoffs, 
        frb, 
        champion, 
        second:eleventh
      ) |>
      left_join(
        next_wk_wins,
        by = "team"
      )
    
    all_sims = rbind(all_sims, season_i_res)
    
    }
    
  sim_results = all_sims %>% 
    group_by(team) %>%
    summarise(
      across(c('wins':'win_nxt_game'), 
             mean)
    )
  
  return(list(sim_results, all_sims))
}


a = simulate_season(runs)

simulated_season_results = a[[1]] %>%
  mutate(
    wk = SeasonGames %>% 
      filter(
        !is.na(score)
      ) %>% 
      select(week) %>% 
      unique() %>% 
      nrow() %>% 
      as.numeric()
  )

all_simulated_results = a[[2]] 

wl_pofrb_chances <- all_simulated_results |>
  summarise(
    po_wl = mean(playoffs),
    frb_wl = mean(frb),
    .by = c("team", "win_nxt_game")
  )

win_nxt_chances <- wl_pofrb_chances |>
  filter(
    win_nxt_game == 1
  ) |>
  select(-win_nxt_game) |>
  rename(
    win_po = po_wl,
    win_frb = frb_wl
  )

lose_nxt_chances <- wl_pofrb_chances |>
  filter(
    win_nxt_game == 0
  ) |>
  select(-win_nxt_game) |>
  rename(
    lose_po = po_wl,
    lose_frb = frb_wl
  )

simulated_season_results <- simulated_season_results |>
  left_join(
    win_nxt_chances,
    by = "team"
  ) |>
  left_join(
    lose_nxt_chances,
    by = "team"
  ) |>
  relocate(
    wk, .after = last_col()
  )

path_out = "C:/Users/jerem/Documents/Fantasy/WOFFL/WOFFL_stats_portal/sim_results/sim_results_2025.csv"
read.csv(path_out) %>% 
  rbind(simulated_season_results) %>% 
  write.csv(path_out, row.names = F)
