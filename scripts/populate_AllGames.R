library(pacman)
p_load(dplyr, openxlsx, janitor, readxl, fflr)

AllGames = read.csv("WOFFL_stats_portal/AllGames.csv") %>%
  clean_names()

CS = unique(AllGames$season) %>% max()
next_week = ifelse(AllGames %>% filter(score != 0) %>% filter(season == CS) %>% select(week) %>% max() == -Inf, 1, AllGames %>% filter(score != 0) %>% filter(season == CS) %>% select(week) %>% max() + 1)

options(fflr.leagueId = "313259")

round2 = function(x, digits) {
  posneg = sign(x)
  z = abs(x)*10^digits
  z = z + 0.5 + sqrt(.Machine$double.eps)
  z = trunc(z)
  z = z/10^digits
  z*posneg
}



rosters = team_roster(scoringPeriodId = 1)

get_weekly_rows = function(df, rosters){
  
  weekly_filled = data.frame()
  for(t in 1:length(rosters)){
    row_filled = data.frame(season = rosters[[t]] %>% select(seasonId) %>% unique() %>% as.numeric(),
                            week = rosters[[t]] %>% select(scoringPeriodId) %>% unique() %>% as.numeric(),
                            team_id = rosters[[t]] %>% select(teamId) %>% unique() %>% as.numeric(),
                            #qb
                            qb_name_new = rosters[[t]] %>% filter(lineupSlot == "QB") %>% select(playerId) %>% as.character(),
                            qb_tm_new = rosters[[t]] %>% filter(lineupSlot == "QB") %>% select(proTeam) %>% as.character(),
                            qb_proj_new = rosters[[t]] %>% filter(lineupSlot == "QB") %>% select(projectedScore) %>% as.numeric() %>% round2(digits = 2),
                            qb_score_new = rosters[[t]] %>% filter(lineupSlot == "QB") %>% select(actualScore) %>% as.numeric(),
                            #rb1
                            rb1_name_new = rosters[[t]] %>% filter(lineupSlot == "RB") %>% filter(row_number()==1) %>% select(playerId) %>% as.character(),
                            rb1_tm_new = rosters[[t]] %>% filter(lineupSlot == "RB") %>% filter(row_number()==1) %>% select(proTeam) %>% as.character(),
                            rb1_proj_new = rosters[[t]] %>% filter(lineupSlot == "RB") %>% filter(row_number()==1) %>% select(projectedScore) %>% as.numeric() %>% round2(digits = 2),
                            rb1_score_new = rosters[[t]] %>% filter(lineupSlot == "RB") %>% filter(row_number()==1) %>% select(actualScore) %>% as.numeric(),
                            #rb2
                            rb2_name_new = rosters[[t]] %>% filter(lineupSlot == "RB") %>% filter(row_number()==2) %>% select(playerId) %>% as.character(),
                            rb2_tm_new = rosters[[t]] %>% filter(lineupSlot == "RB") %>% filter(row_number()==2) %>% select(proTeam) %>% as.character(),
                            rb2_proj_new = rosters[[t]] %>% filter(lineupSlot == "RB") %>% filter(row_number()==2) %>% select(projectedScore) %>% as.numeric() %>% round2(digits = 2),
                            rb2_score_new = rosters[[t]] %>% filter(lineupSlot == "RB") %>% filter(row_number()==2) %>% select(actualScore) %>% as.numeric(),
                            #wr1
                            wr1_name_new = rosters[[t]] %>% filter(lineupSlot == "WR") %>% filter(row_number()==1) %>% select(playerId) %>% as.character(),
                            wr1_tm_new = rosters[[t]] %>% filter(lineupSlot == "WR") %>% filter(row_number()==1) %>% select(proTeam) %>% as.character(),
                            wr1_proj_new = rosters[[t]] %>% filter(lineupSlot == "WR") %>% filter(row_number()==1) %>% select(projectedScore) %>% as.numeric() %>% round2(digits = 2),
                            wr1_score_new = rosters[[t]] %>% filter(lineupSlot == "WR") %>% filter(row_number()==1) %>% select(actualScore) %>% as.numeric(),
                            #wr2
                            wr2_name_new = rosters[[t]] %>% filter(lineupSlot == "WR") %>% filter(row_number()==2) %>% select(playerId) %>% as.character(),
                            wr2_tm_new = rosters[[t]] %>% filter(lineupSlot == "WR") %>% filter(row_number()==2) %>% select(proTeam) %>% as.character(),
                            wr2_proj_new = rosters[[t]] %>% filter(lineupSlot == "WR") %>% filter(row_number()==2) %>% select(projectedScore) %>% as.numeric() %>% round2(digits = 2),
                            wr2_score_new = rosters[[t]] %>% filter(lineupSlot == "WR") %>% filter(row_number()==2) %>% select(actualScore) %>% as.numeric(),
                            #wrte
                            wrte_name_new = rosters[[t]] %>% filter(lineupSlot == "WR/TE") %>% select(playerId) %>% as.character(),
                            wrte_tm_new = rosters[[t]] %>% filter(lineupSlot == "WR/TE") %>% select(proTeam) %>% as.character(),
                            wrte_pos_new = rosters[[t]] %>% filter(lineupSlot == "WR/TE") %>% select(position) %>% as.character(),
                            wrte_proj_new = rosters[[t]] %>% filter(lineupSlot == "WR/TE") %>% select(projectedScore) %>% as.numeric() %>% round2(digits = 2),
                            wrte_score_new = rosters[[t]] %>% filter(lineupSlot == "WR/TE") %>% select(actualScore) %>% as.numeric(),
                            #te
                            te_name_new = rosters[[t]] %>% filter(lineupSlot == "TE") %>% select(playerId) %>% as.character(),
                            te_tm_new = rosters[[t]] %>% filter(lineupSlot == "TE") %>% select(proTeam) %>% as.character(),
                            te_proj_new = rosters[[t]] %>% filter(lineupSlot == "TE") %>% select(projectedScore) %>% as.numeric() %>% round2(digits = 2),
                            te_score_new = rosters[[t]] %>% filter(lineupSlot == "TE") %>% select(actualScore) %>% as.numeric(),
                            #flex
                            flex_name_new = rosters[[t]] %>% filter(lineupSlot == "FLEX") %>% select(playerId) %>% as.character(),
                            flex_tm_new = rosters[[t]] %>% filter(lineupSlot == "FLEX") %>% select(proTeam) %>% as.character(),
                            flex_pos_new = rosters[[t]] %>% filter(lineupSlot == "FLEX") %>% select(position) %>% as.character(),
                            flex_proj_new = rosters[[t]] %>% filter(lineupSlot == "FLEX") %>% select(projectedScore) %>% as.numeric() %>% round2(digits = 2),
                            flex_score_new = rosters[[t]] %>% filter(lineupSlot == "FLEX") %>% select(actualScore) %>% as.numeric(),
                            #dst
                            dst_name_new = rosters[[t]] %>% filter(lineupSlot == "D/ST") %>% select(playerId) %>% as.character(),
                            dst_tm_new = rosters[[t]] %>% filter(lineupSlot == "D/ST") %>% select(proTeam) %>% as.character(),
                            dst_proj_new = rosters[[t]] %>% filter(lineupSlot == "D/ST") %>% select(projectedScore) %>% as.numeric() %>% round2(digits = 2),
                            dst_score_new = rosters[[t]] %>% filter(lineupSlot == "D/ST") %>% select(actualScore) %>% as.numeric(),
                            #ben1
                            ben1_name_new = rosters[[t]] %>% filter(lineupSlot == "BE") %>% filter(row_number()==1) %>% select(playerId) %>% as.character(),
                            ben1_tm_new = rosters[[t]] %>% filter(lineupSlot == "BE") %>% filter(row_number()==1) %>% select(proTeam) %>% as.character(),
                            ben1_pos_new = rosters[[t]] %>% filter(lineupSlot == "BE") %>% filter(row_number()==1) %>% select(position) %>% as.character(),
                            ben1_proj_new = rosters[[t]] %>% filter(lineupSlot == "BE") %>% filter(row_number()==1) %>% select(projectedScore) %>% as.numeric() %>% round2(digits = 2),
                            ben1_score_new = rosters[[t]] %>% filter(lineupSlot == "BE") %>% filter(row_number()==1) %>% select(actualScore) %>% as.numeric(),
                            #ben2
                            ben2_name_new = rosters[[t]] %>% filter(lineupSlot == "BE") %>% filter(row_number()==2) %>% select(playerId) %>% as.character(),
                            ben2_tm_new = rosters[[t]] %>% filter(lineupSlot == "BE") %>% filter(row_number()==2) %>% select(proTeam) %>% as.character(),
                            ben2_pos_new = rosters[[t]] %>% filter(lineupSlot == "BE") %>% filter(row_number()==2) %>% select(position) %>% as.character(),
                            ben2_proj_new = rosters[[t]] %>% filter(lineupSlot == "BE") %>% filter(row_number()==2) %>% select(projectedScore) %>% as.numeric() %>% round2(digits = 2),
                            ben2_score_new = rosters[[t]] %>% filter(lineupSlot == "BE") %>% filter(row_number()==2) %>% select(actualScore) %>% as.numeric(),
                            #ben3
                            ben3_name_new = rosters[[t]] %>% filter(lineupSlot == "BE") %>% filter(row_number()==3) %>% select(playerId) %>% as.character(),
                            ben3_tm_new = rosters[[t]] %>% filter(lineupSlot == "BE") %>% filter(row_number()==3) %>% select(proTeam) %>% as.character(),
                            ben3_pos_new = rosters[[t]] %>% filter(lineupSlot == "BE") %>% filter(row_number()==3) %>% select(position) %>% as.character(),
                            ben3_proj_new = rosters[[t]] %>% filter(lineupSlot == "BE") %>% filter(row_number()==3) %>% select(projectedScore) %>% as.numeric() %>% round2(digits = 2),
                            ben3_score_new = rosters[[t]] %>% filter(lineupSlot == "BE") %>% filter(row_number()==3) %>% select(actualScore) %>% as.numeric(),
                            #ben4
                            ben4_name_new = rosters[[t]] %>% filter(lineupSlot == "BE") %>% filter(row_number()==4) %>% select(playerId) %>% as.character(),
                            ben4_tm_new = rosters[[t]] %>% filter(lineupSlot == "BE") %>% filter(row_number()==4) %>% select(proTeam) %>% as.character(),
                            ben4_pos_new = rosters[[t]] %>% filter(lineupSlot == "BE") %>% filter(row_number()==4) %>% select(position) %>% as.character(),
                            ben4_proj_new = rosters[[t]] %>% filter(lineupSlot == "BE") %>% filter(row_number()==4) %>% select(projectedScore) %>% as.numeric() %>% round2(digits = 2),
                            ben4_score_new = rosters[[t]] %>% filter(lineupSlot == "BE") %>% filter(row_number()==4) %>% select(actualScore) %>% as.numeric(),
                            #ben5
                            ben5_name_new = rosters[[t]] %>% filter(lineupSlot == "BE") %>% filter(row_number()==5) %>% select(playerId) %>% as.character(),
                            ben5_tm_new = rosters[[t]] %>% filter(lineupSlot == "BE") %>% filter(row_number()==5) %>% select(proTeam) %>% as.character(),
                            ben5_pos_new = rosters[[t]] %>% filter(lineupSlot == "BE") %>% filter(row_number()==5) %>% select(position) %>% as.character(),
                            ben5_proj_new = rosters[[t]] %>% filter(lineupSlot == "BE") %>% filter(row_number()==5) %>% select(projectedScore) %>% as.numeric() %>% round2(digits = 2),
                            ben5_score_new = rosters[[t]] %>% filter(lineupSlot == "BE") %>% filter(row_number()==5) %>% select(actualScore) %>% as.numeric(),
                            #ben6
                            ben6_name_new = rosters[[t]] %>% filter(lineupSlot == "BE") %>% filter(row_number()==6) %>% select(playerId) %>% as.character(),
                            ben6_tm_new = rosters[[t]] %>% filter(lineupSlot == "BE") %>% filter(row_number()==6) %>% select(proTeam) %>% as.character(),
                            ben6_pos_new = rosters[[t]] %>% filter(lineupSlot == "BE") %>% filter(row_number()==6) %>% select(position) %>% as.character(),
                            ben6_proj_new = rosters[[t]] %>% filter(lineupSlot == "BE") %>% filter(row_number()==6) %>% select(projectedScore) %>% as.numeric() %>% round2(digits = 2),
                            ben6_score_new = rosters[[t]] %>% filter(lineupSlot == "BE") %>% filter(row_number()==6) %>% select(actualScore) %>% as.numeric(),
                            #ben7
                            ben7_name_new = rosters[[t]] %>% filter(lineupSlot == "BE") %>% filter(row_number()==7) %>% select(playerId) %>% as.character(),
                            ben7_tm_new = rosters[[t]] %>% filter(lineupSlot == "BE") %>% filter(row_number()==7) %>% select(proTeam) %>% as.character(),
                            ben7_pos_new = rosters[[t]] %>% filter(lineupSlot == "BE") %>% filter(row_number()==7) %>% select(position) %>% as.character(),
                            ben7_proj_new = rosters[[t]] %>% filter(lineupSlot == "BE") %>% filter(row_number()==7) %>% select(projectedScore) %>% as.numeric() %>% round2(digits = 2),
                            ben7_score_new = rosters[[t]] %>% filter(lineupSlot == "BE") %>% filter(row_number()==7) %>% select(actualScore) %>% as.numeric(),
                            #ben8
                            ben8_name_new = rosters[[t]] %>% filter(lineupSlot == "BE") %>% filter(row_number()==8) %>% select(playerId) %>% as.character(),
                            ben8_tm_new = rosters[[t]] %>% filter(lineupSlot == "BE") %>% filter(row_number()==8) %>% select(proTeam) %>% as.character(),
                            ben8_pos_new = rosters[[t]] %>% filter(lineupSlot == "BE") %>% filter(row_number()==8) %>% select(position) %>% as.character(),
                            ben8_proj_new = rosters[[t]] %>% filter(lineupSlot == "BE") %>% filter(row_number()==8) %>% select(projectedScore) %>% as.numeric() %>% round2(digits = 2),
                            ben8_score_new = rosters[[t]] %>% filter(lineupSlot == "BE") %>% filter(row_number()==8) %>% select(actualScore) %>% as.numeric(),
                            #ir 
                            ir_name_new = rosters[[t]] %>% filter(lineupSlot == "IR") %>% select(playerId) %>% as.character(),
                            ir_tm_new = rosters[[t]] %>% filter(lineupSlot == "IR") %>% select(proTeam) %>% as.character(),
                            ir_pos_new = rosters[[t]] %>% filter(lineupSlot == "IR") %>% select(position) %>% as.character(),
                            ir_proj_new = rosters[[t]] %>% filter(lineupSlot == "IR") %>% select(projectedScore) %>% as.numeric() %>% round2(digits = 2),
                            ir_score_new = rosters[[t]] %>% filter(lineupSlot == "IR") %>% select(actualScore) %>% as.numeric()) %>%
      mutate(across(c(qb_name_new:ir_score_new), ~ ifelse(is.na(.), as.numeric(0), .))) %>%
      mutate(across(c(qb_name_new:ir_score_new), ~ ifelse(.=="integer(0)", "Empty", .)))
    
    weekly_filled = rbind(weekly_filled, row_filled)
    }
  
  weekly_filled = weekly_filled %>% mutate(team = case_when(team_id==1 ~ "Jeremy Patak",
                                            team_id==2 ~ "Austin Iske",
                                            team_id==3 ~ "Brody Morgan",
                                            team_id==4 ~ "Dax Davis",
                                            team_id==5 ~ "Landry Sheridan",
                                            team_id==6 ~ "Stone Palmer",
                                            team_id==7 ~ "Seth Lassiter",
                                            team_id==8 ~ "Nick McFarland",
                                            team_id==9 ~ "Nike Simmons",
                                            team_id==10 ~ "Daniel Potichko")) %>% select(-team_id)
  
  new_df = df %>% left_join(weekly_filled) %>%
    mutate(qb_name = ifelse(is.na(qb_name_new), qb_name, qb_name_new),
           qb_tm = ifelse(is.na(qb_tm_new), qb_tm, qb_tm_new),
           qb_proj = ifelse(is.na(qb_proj_new), qb_proj, qb_proj_new),
           qb_pts = ifelse(is.na(qb_score_new), qb_pts, qb_score_new),
           #rb1
           rb1_name = ifelse(is.na(rb1_name_new), rb1_name, rb1_name_new),
           rb1_tm = ifelse(is.na(rb1_tm_new), rb1_tm, rb1_tm_new),
           rb1_proj = ifelse(is.na(rb1_proj_new), rb1_proj, rb1_proj_new),
           rb1_pts = ifelse(is.na(rb1_score_new), rb1_pts, rb1_score_new),
           #rb2
           rb2_name = ifelse(is.na(rb2_name_new), rb2_name, rb2_name_new),
           rb2_tm = ifelse(is.na(rb2_tm_new), rb2_tm, rb2_tm_new),
           rb2_proj = ifelse(is.na(rb2_proj_new), rb2_proj, rb2_proj_new),
           rb2_pts = ifelse(is.na(rb2_score_new), rb2_pts, rb2_score_new),
           #wr1
           wr1_name = ifelse(is.na(wr1_name_new), wr1_name, wr1_name_new),
           wr1_tm = ifelse(is.na(wr1_tm_new), wr1_tm, wr1_tm_new),
           wr1_proj = ifelse(is.na(wr1_proj_new), wr1_proj, wr1_proj_new),
           wr1_pts = ifelse(is.na(wr1_score_new), wr1_pts, wr1_score_new),
           #wr2
           wr2_name = ifelse(is.na(wr2_name_new), wr2_name, wr2_name_new),
           wr2_tm = ifelse(is.na(wr2_tm_new), wr2_tm, wr2_tm_new),
           wr2_proj = ifelse(is.na(wr2_proj_new), wr2_proj, wr2_proj_new),
           wr2_pts = ifelse(is.na(wr2_score_new), wr2_pts, wr2_score_new),
           #wrte
           wr_te_name = ifelse(is.na(wrte_name_new), wr_te_name, wrte_name_new),
           wr_te_tm = ifelse(is.na(wrte_tm_new), wr_te_tm, wrte_tm_new),
           wr_te_pos = ifelse(is.na(wrte_pos_new), wr_te_pos, wrte_pos_new),
           wr_te_proj = ifelse(is.na(wrte_proj_new), wr_te_proj, wrte_proj_new),
           wr_te_pts = ifelse(is.na(wrte_score_new), wr_te_pts, wrte_score_new),
           #te
           te_name = ifelse(is.na(te_name_new), te_name, te_name_new),
           te_tm = ifelse(is.na(te_tm_new), te_tm, te_tm_new),
           te_proj = ifelse(is.na(te_proj_new), te_proj, te_proj_new),
           te_pts = ifelse(is.na(te_score_new), te_pts, te_score_new),
           #flex
           flex_name = ifelse(is.na(flex_name_new), flex_name, flex_name_new),
           flex_tm = ifelse(is.na(flex_tm_new), flex_tm, flex_tm_new),
           flex_pos = ifelse(is.na(flex_pos_new), flex_pos, flex_pos_new),
           flex_proj = ifelse(is.na(flex_proj_new), flex_proj, flex_proj_new),
           flex_pts = ifelse(is.na(flex_score_new), flex_pts, flex_score_new),
           #dst
           dst_name = ifelse(is.na(dst_name_new), dst_name, dst_name_new),
           dst_tm = ifelse(is.na(dst_tm_new), dst_tm, dst_tm_new),
           dst_proj = ifelse(is.na(dst_proj_new), dst_proj, dst_proj_new),
           dst_pts = ifelse(is.na(dst_score_new), dst_pts, dst_score_new),
           #b1
           b1_name = ifelse(is.na(ben1_name_new), b1_name, ben1_name_new),
           b1_tm = ifelse(is.na(ben1_tm_new), b1_tm, ben1_tm_new),
           b1_pos = ifelse(is.na(ben1_pos_new), b1_pos, ben1_pos_new),
           b1_proj = ifelse(is.na(ben1_proj_new), b1_proj, ben1_proj_new),
           b1_pts = ifelse(is.na(ben1_score_new), b1_pts, ben1_score_new),
           #b2
           b2_name = ifelse(is.na(ben2_name_new), b2_name, ben2_name_new),
           b2_tm = ifelse(is.na(ben2_tm_new), b2_tm, ben2_tm_new),
           b2_pos = ifelse(is.na(ben2_pos_new), b2_pos, ben2_pos_new),
           b2_proj = ifelse(is.na(ben2_proj_new), b2_proj, ben2_proj_new),
           b2_pts = ifelse(is.na(ben2_score_new), b2_pts, ben2_score_new),
           #b3
           b3_name = ifelse(is.na(ben3_name_new), b3_name, ben3_name_new),
           b3_tm = ifelse(is.na(ben3_tm_new), b3_tm, ben3_tm_new),
           b3_pos = ifelse(is.na(ben3_pos_new), b3_pos, ben3_pos_new),
           b3_proj = ifelse(is.na(ben3_proj_new), b3_proj, ben3_proj_new),
           b3_pts = ifelse(is.na(ben3_score_new), b3_pts, ben3_score_new),
           #b4
           b4_name = ifelse(is.na(ben4_name_new), b4_name, ben4_name_new),
           b4_tm = ifelse(is.na(ben4_tm_new), b4_tm, ben4_tm_new),
           b4_pos = ifelse(is.na(ben4_pos_new), b4_pos, ben4_pos_new),
           b4_proj = ifelse(is.na(ben4_proj_new), b4_proj, ben4_proj_new),
           b4_pts = ifelse(is.na(ben4_score_new), b4_pts, ben4_score_new),
           #b5
           b5_name = ifelse(is.na(ben5_name_new), b5_name, ben5_name_new),
           b5_tm = ifelse(is.na(ben5_tm_new), b5_tm, ben5_tm_new),
           b5_pos = ifelse(is.na(ben5_pos_new), b5_pos, ben5_pos_new),
           b5_proj = ifelse(is.na(ben5_proj_new), b5_proj, ben5_proj_new),
           b5_pts = ifelse(is.na(ben5_score_new), b5_pts, ben5_score_new),
           #b6
           b6_name = ifelse(is.na(ben6_name_new), b6_name, ben6_name_new),
           b6_tm = ifelse(is.na(ben6_tm_new), b6_tm, ben6_tm_new),
           b6_pos = ifelse(is.na(ben6_pos_new), b6_pos, ben6_pos_new),
           b6_proj = ifelse(is.na(ben6_proj_new), b6_proj, ben6_proj_new),
           b6_pts = ifelse(is.na(ben6_score_new), b6_pts, ben6_score_new),
           #b7
           b7_name = ifelse(is.na(ben7_name_new), b7_name, ben7_name_new),
           b7_tm = ifelse(is.na(ben7_tm_new), b7_tm, ben7_tm_new),
           b7_pos = ifelse(is.na(ben7_pos_new), b7_pos, ben7_pos_new),
           b7_proj = ifelse(is.na(ben7_proj_new), b7_proj, ben7_proj_new),
           b7_pts = ifelse(is.na(ben7_score_new), b7_pts, ben7_score_new),
           #b8
           b8_name = ifelse(is.na(ben8_name_new), b8_name, ben8_name_new),
           b8_tm = ifelse(is.na(ben8_tm_new), b8_tm, ben8_tm_new),
           b8_pos = ifelse(is.na(ben8_pos_new), b8_pos, ben8_pos_new),
           b8_proj = ifelse(is.na(ben8_proj_new), b8_proj, ben8_proj_new),
           b8_pts = ifelse(is.na(ben8_score_new), b8_pts, ben8_score_new),
           #ir
           ir_name = ifelse(is.na(ir_name_new), ir_name, ir_name_new),
           ir_tm = ifelse(is.na(ir_tm_new), ir_tm, ir_tm_new),
           ir_pos = ifelse(is.na(ir_pos_new), ir_pos, ir_pos_new),
           ir_proj = ifelse(is.na(ir_proj_new), ir_proj, ir_proj_new),
           ir_pts = ifelse(is.na(ir_score_new), ir_pts, ir_score_new)) %>%
    select(-c(qb_name_new:ir_score_new))
  
  return(new_df)
  
}


a = get_weekly_rows(AllGames, team_roster(scoringPeriodId = 1))

write.csv(a, "WOFFL_stats_portal/AllGames.csv", row.names = F)
