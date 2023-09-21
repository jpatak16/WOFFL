library(shiny)
library(pacman)
p_load(dplyr, readxl, magrittr, janitor, httr, jsonlite, gt, gtExtras, tidyr, ggplot2)

AllGames = read.csv("AllGames.csv") %>%
  clean_names() %>%
  mutate(result = ifelse(score > opponent_score, 1, 0))

woffl_id = 313259

CS = unique(AllGames$season) %>% max()

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

gt_merge_stack_with_plots<-function(gt_object, week, col1, col2, col3, col4, palette = c("black", "grey"), 
                              ..., small_cap = TRUE, font_size = c("14px", "10px"), font_weight = c("bold", "bold"), 
                              max_wins=16, width=max_wins/.65, color1="#013369", color2="#D50A0A", color3="gray",
                              height_plot = 7, palette2=c("black", "black", "purple", "green", "lightgrey"),
                              fig_dim=c(height_plot, width), same_limit = TRUE, type = "default", label = TRUE) {
  
  colors <- scales::col2hcl(palette, ...)
  col1_bare <- rlang::enexpr(col1) %>% rlang::as_string()
  row_name_var <- gt_object[["_boxhead"]][["var"]][which(gt_object[["_boxhead"]][["type"]] == "stub")]
  data_in2 <- gtExtras::gt_index(gt_object, column = {{col2}})
  data_in3 <- gtExtras::gt_index(gt_object, column = {{col3}})
  
  list_vals <- gt_index(gt_object = gt_object, {{col3}}, as_vector = TRUE)
  
  plot_fn_pill <- function(vals) {
    if (all(is.na(vals) | is.null(vals))) {
      plot_out <- ggplot() +
        theme_void()
    } else {
      input_data <- data.frame(
        x = 1:length(vals),
        xend = 1:length(vals),
        y = ifelse(vals == 0.5, 0.4, vals),
        yend = ifelse(vals == 0, 0.6, ifelse(vals > 0.5, 0.4, 0.6)),
        color = ifelse(vals == 0, color2, ifelse(vals == 1, color1, color3))
      )
      
      plot_out <- ggplot(input_data) +
        geom_segment(
          aes(
            x = .data$x,
            xend = .data$xend,
            y = .data$y,
            yend = .data$yend,
            color = I(.data$color)
          ),
          linewidth = 1,
          lineend = "round"
        ) +
        scale_x_continuous(limits = c(0.5, max_wins + 0.5)) +
        scale_y_continuous(limits = c(-.2, 1.2)) +
        theme_void()
    }
    
    out_name <- file.path(
      tempfile(pattern = "file", tmpdir = tempdir(), fileext = ".svg")
    )
    
    ggsave(
      out_name,
      plot = plot_out,
      dpi = 20,
      height = height_plot,
      width = width,
      units = "mm"
    )
    
    img_plot <- out_name %>%
      readLines() %>%
      paste0(collapse = "") %>%
      gt::html()
    
    on.exit(file.remove(out_name), add = TRUE)
    
    img_plot
  }
  
  col_bare <- dplyr::select(gt_object[["_data"]], {{col4}}) %>% names()
  
  list_data_in <- gt_index(gt_object, col_bare, as_vector = TRUE)
  
  data_in <- unlist(list_data_in)
  
  total_rng <- grDevices::extendrange(data_in, r = range(data_in, na.rm = TRUE), f = 0.02)
  
  plot_fn_spark <- function(list_data_in) {
    if (all(list_data_in %in% c(NA, NULL))) {
      return("<div></div>")
    }
    
    # vals <- as.double(stats::na.omit(list_data_in))
    vals <- as.double(list_data_in)
    
    max_val <- max(vals, na.rm = TRUE)
    min_val <- min(vals, na.rm = TRUE)
    
    x_max <- vals[vals == max_val]
    x_min <- vals[vals == min_val]
    
    point_data <- dplyr::tibble(
      x = c(
        c(1:length(vals))[vals == min_val],
        c(1:length(vals))[vals == max_val]
      ),
      y = c(x_min, x_max),
      colors = c(
        rep(palette2[3], length(x_min)),
        rep(palette2[4], length(x_max))
      )
    )
    
    input_data <- dplyr::tibble(
      x = 1:length(vals),
      y = vals
    )
    
    plot_base <- ggplot(input_data) +
      theme_void()
    
    med_y_rnd <- round(stats::median(input_data$y, na.rm = TRUE))
    last_val_label <- input_data[nrow(vals), 2]
    
    if (isTRUE(same_limit) && isFALSE(label)) {
      plot_base <- plot_base +
        scale_y_continuous(expand = expansion(mult = 0.05)) +
        coord_cartesian(
          clip = "off",
          ylim = grDevices::extendrange(total_rng, f = 0.09)
        )
    } else if (isFALSE(same_limit) && isFALSE(label)) {
      plot_base <- plot_base +
        scale_y_continuous(expand = expansion(mult = 0.05)) +
        coord_cartesian(
          clip = "off",
          ylim = grDevices::extendrange(vals, f = 0.09)
        )
    } else if (isFALSE(same_limit) && isTRUE(label)) {
      plot_base <- plot_base +
        geom_text(
          data = filter(input_data, .data$x == max(.data$x)),
          aes(
            x = .data$x,
            y = .data$y,
            label = scales::label_number(
              scale_cut = scales::cut_short_scale(),
              accuracy = if (med_y_rnd > 0) {
                .1
              } else if (med_y_rnd == 0) {
                .01
              }
            )(.data$y)
          ),
          size = 2,
          family = "mono",
          hjust = 0,
          vjust = 0.5,
          position = position_nudge(x = max(input_data$x) * 0.05),
          color = palette[2],
          na.rm = TRUE
        ) +
        scale_y_continuous(expand = expansion(mult = 0.05)) +
        coord_cartesian(
          clip = "off",
          ylim = grDevices::extendrange(vals, f = 0.09),
          xlim = c(0.25, length(vals) * 1.25)
        )
    } else if (isTRUE(same_limit) && isTRUE(label)) {
      plot_base <- plot_base +
        geom_text(
          data = filter(input_data, .data$x == max(.data$x)),
          aes(
            x = .data$x,
            y = .data$y,
            label = scales::label_number(
              scale_cut = scales::cut_short_scale(),
              accuracy = if (med_y_rnd > 0) {
                .1
              } else if (med_y_rnd == 0) {
                .01
              }
            )(.data$y)
          ),
          size = 2,
          family = "mono",
          hjust = 0,
          vjust = 0.5,
          position = position_nudge(x = max(input_data$x) * 0.05),
          color = palette2[2],
          na.rm = TRUE
        ) +
        scale_y_continuous(expand = expansion(mult = 0.05)) +
        coord_cartesian(
          clip = "off",
          ylim = grDevices::extendrange(total_rng, f = 0.09),
          xlim = c(0.25, length(vals) * 1.25)
        )
    }
    
    plot_out <- plot_base +
      geom_line(
        aes(x = .data$x, y = .data$y, group = 1),
        linewidth = 0.5,
        color = palette2[1],
        na.rm = TRUE
      ) +
      geom_point(
        data = filter(input_data, .data$x == max(.data$x)),
        aes(x = .data$x, y = .data$y),
        size = 0.5,
        color = palette2[2],
        na.rm = TRUE
      ) +
      geom_point(
        data = point_data,
        aes(x = .data$x, y = .data$y, color = I(.data$colors), group = 1),
        size = 0.5,
        na.rm = TRUE
      )
    
    ### Shaded area
    if (type == "shaded") {
      plot_out$layers <- c(
        geom_area(
          aes(x = .data$x, y = .data$y),
          fill = palette2[5],
          alpha = 0.75,
          na.rm = TRUE
        ),
        plot_out$layers
      )
      
      ### Horizontal ref line at median
    } else if (type == "ref_median") {
      plot_out$layers <- c(
        geom_segment(
          aes(
            x = min(.data$x),
            y = stats::median(.data$y),
            xend = max(.data$x),
            yend = stats::median(.data$y)
          ),
          color = palette2[5],
          linewidth = 0.1,
          na.rm = TRUE
        ),
        plot_out$layers
      )
      ### dots on all points
    } else if (type == "points") {
      plot_out$layers <- c(
        geom_point(
          aes(x = .data$x, y = .data$y),
          color = palette2[5],
          size = 0.4,
          na.rm = TRUE
        ),
        plot_out$layers
      )
      ### Horizontal ref line at mean
    } else if (type == "ref_mean") {
      plot_out$layers <- c(
        geom_segment(
          aes(
            x = min(.data$x),
            y = mean(.data$y),
            xend = max(.data$x),
            yend = mean(.data$y)
          ),
          color = palette2[5],
          linewidth = 0.1,
          na.rm = TRUE
        ),
        plot_out$layers
      )
      ### Horizontal ref line at last point
    } else if (type == "ref_last") {
      plot_out$layers <- c(
        geom_segment(
          aes(
            x = min(.data$x),
            y = last(.data$y),
            xend = max(.data$x),
            yend = last(.data$y)
          ),
          color = palette2[5],
          linewidth = 0.1,
          na.rm = TRUE
        ),
        plot_out$layers
      )
      ### Horizontal area/ribbon for 25/75 interquantile range
    } else if (type == "ref_iqr") {
      ribbon_df <- input_data %>%
        summarise(
          q25 = stats::quantile(.data$y, 0.25),
          q75 = stats::quantile(.data$y, 0.75)
        )
      
      plot_out$layers <- c(
        geom_ribbon(
          aes(x = .data$x, ymin = ribbon_df$q25, ymax = ribbon_df$q75),
          fill = palette2[5],
          alpha = 0.5,
          na.rm = TRUE
        ),
        geom_segment(
          aes(
            x = min(.data$x),
            y = stats::median(.data$y),
            xend = max(.data$x),
            yend = stats::median(.data$y)
          ),
          color = palette2[5],
          linewidth = 0.1,
          na.rm = TRUE
        ),
        plot_out$layers
      )
    }
    
    out_name <- file.path(
      tempfile(pattern = "file", tmpdir = tempdir(), fileext = ".svg")
    )
    
    ggsave(
      out_name,
      plot = plot_out,
      dpi = 25.4,
      height = fig_dim[1],
      width = fig_dim[2],
      units = "mm"
    )
    
    img_plot <- out_name %>%
      readLines() %>%
      paste0(collapse = "") %>%
      gt::html()
    
    on.exit(file.remove(out_name), add = TRUE)
    
    img_plot
  }
  
  
  gt_object %>% text_transform(locations = 
                                 if (isTRUE(row_name_var == col1_bare)) {cells_stub(rows = gt::everything())
                                   
                                 } else {cells_body(columns = {{col1}})}, 
                               fn = function(x) {
                                 if (small_cap) {font_variant <- "small-caps"
                                 } else {font_variant <- "normal"}
                                 if(week == 1){glue::glue("<div style='line-height:{font_size[1]}; padding-top:7px'><div style='font-weight:{font_weight[1]};font-variant:{font_variant};color:{colors[1]};font-size:{font_size[1]}'>{x}</div></div>
                                            <div style='line-height:{font_size[2]}; padding-top:4px'><div style ='font-weight:{font_weight[2]};color:{colors[2]};font-size:{font_size[2]}'>{data_in2}</div></div>
                                            <div style='line-height:50px'><span style='background-image: {lapply(list_vals,plot_fn_pill)}</span></div>")}
                                 else{glue::glue("<div style='line-height:{font_size[1]}; padding-top:7px'><div style='font-weight:{font_weight[1]};font-variant:{font_variant};color:{colors[1]};font-size:{font_size[1]}'>{x}</div></div>
                                            <div style='line-height:{font_size[2]}; padding-top:4px'><div style ='font-weight:{font_weight[2]};color:{colors[2]};font-size:{font_size[2]}'>{data_in2}</div></div>
                                            <div style='line-height:50px'><span style='background-image: {lapply(list_vals,plot_fn_pill)}</span><span style='background-image: {lapply(list_data_in,plot_fn_spark)}</span></div>")}
                                 
                                 }) %>% 
    cols_hide(columns = c({{col2}}, {{col3}}, {{col4}}))}



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
    df = dat$teams$roster$entries[[i]]
    finished = df$playerPoolEntry$rosterLocked
    pos = df$lineupSlotId
    df2 = data.frame(team_id = i, finished_game = finished, position_id = pos)
    blank = rbind(blank, df2)
  }
  return(blank)
}

dat <- ffl_api(leagueId = 313259, view = c("mScoreboard", "mRoster"), seasonId = 2023)
CW = dat$scoringPeriodId

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

still_playing = yet_to_play() %>% filter(position_id != 20, position_id != 21, finished_game==F) %>% group_by(team_id) %>% summarise(still_playing = n()) %>%
  right_join(team %>% select(id), by = c("team_id" = "id")) %>% mutate(still_playing = ifelse(is.na(still_playing), 0, still_playing),
                                                                       still_playing = ifelse(team_id == 12, sum(still_playing), still_playing))

ui = navbarPage("WOFFL Portal", fluid = TRUE,
                tabPanel("Weekly Scoreboard",
                         fluidRow(column(9, h1(span("White Oak Fantasy Football League Portal", style = 'color:#8A8A8A; text-shadow: black 0.0em 0.1em 0.2em')), 
                                         h1(span("Weekly Scoreboard", style = 'font-size: 60px; font-weight: bold; color:#FFFFFF; text-shadow: black 0.0em 0.18em 0.2em'))),
                                  column(3, img(src="3d.jpg", height = 150, width = 210)),
                                  style = 'margin-top:-20px; padding-top:10px; padding-bottom:10px; background-color:#580515'),
                         fluidRow(column(12, align='center', selectInput("week", "Week Selector", week_selector_options, selected = CW)), 
                                  style = 'padding-top:10px;'),
                         fluidRow(column(12, align='center', uiOutput("playoffs"))),
                         fluidRow(column(6, gt_output('wk_matchup_1')),
                                  column(6, gt_output('wk_matchup_2'))),
                         fluidRow(column(6, gt_output('wk_matchup_3')),
                                  column(6, gt_output('wk_matchup_4'))),
                         fluidRow(column(6, gt_output('wk_matchup_5')),
                                  column(6, gt_output('wk_matchup_6')))
                         ), #end of WS tabPanel
                tabPanel("Playoff Picture",
                         fluidRow(column(9, h1(span("White Oak Fantasy Football League Portal", style = 'color:#8A8A8A; text-shadow: black 0.0em 0.1em 0.2em')), 
                                         h1(span("Playoff Picture", style = 'font-size: 60px; font-weight: bold; color:#FFFFFF; text-shadow: black 0.0em 0.18em 0.2em'))),
                                  column(3, img(src="3d.jpg", height = 150, width = 210)),
                                  style = 'margin-top:-20px; padding-top:10px; padding-bottom:10px; background-color:#580515'),
                         fluidRow(column(4, gt_output('standings')))) #end of PP tabPanel
                ) #end of navbarPage


server = function(input, output, session) {
  
  session$onSessionEnded(function() {
    stopApp()
  })
  
  output$playoffs = renderUI(if(as.numeric(input$week) > 13){img(src="playoffs.gif", height = 300)}
                             else{img(src="playoffs.gif", height = 0)})
  
  standings = reactive(team %>%
                         select(id, logo, name, owner) %>%
                         filter(id != 12) %>%
                         left_join(AllGames %>% filter(season == CS, reg_po == "Regular Season") %>% group_by(team) %>% summarise(p_wins = sum(result, na.rm=T)), 
                                   by = c('owner' = 'team')) %>%
                         mutate(p_gp = AllGames %>% filter(season==CS, reg_po == "Regular Season", !is.na(score)) %>% select(week) %>% unique() %>% max(),
                                p_losses = p_gp - p_wins) %>%
                         left_join(AllGames %>% filter(season==CS, reg_po == "Regular Season") %>% group_by(team) %>% summarise(p_pf = sum(score, na.rm=T), p_pa = sum(opponent_score, na.rm=T)), 
                                   by = c('owner' = 'team')) %>%
                         left_join(AllGames %>% 
                                     filter(season == CS,
                                            team != "Ghost of Dakota Frantum",
                                            !is.na(score)) %>%
                                     group_by(ov_wk) %>%
                                     mutate(ov_wins = order(order(score, decreasing = FALSE)) - 1,
                                            ov_losses = order(order(score, decreasing = TRUE)) - 1) %>%
                                     group_by(team) %>%
                                     summarise(ov_wins = sum(ov_wins), ov_losses = sum(ov_losses)) %>%
                                     rename(owner = team)) %>%
                         left_join(schedule %>% filter(matchupPeriodId == AllGames %>% filter(season==CS, reg_po == "Regular Season", !is.na(score)) %>% select(week) %>% unique() %>% max() %>% sum(1)) %>% select(totalPointsLive, teamId),
                                   by = c("id" = "teamId")) %>%
                         left_join(AllGames %>% filter(season == CS, week == AllGames %>% filter(season==CS, reg_po == "Regular Season", !is.na(score)) %>% select(week) %>% unique() %>% max() %>% sum(1)) %>%
                                     select(owner = team, opponent)) %>%
                         left_join(team %>% select(opp_id = id, owner), by = c('opponent' = 'owner')) %>%
                         left_join(schedule %>% filter(matchupPeriodId == AllGames %>% filter(season==CS, reg_po == "Regular Season", !is.na(score)) %>% select(week) %>% unique() %>% max() %>% sum(1)) %>% select(opp_score_live=totalPointsLive, teamId),
                                   by = c("opp_id" = "teamId")) %>%
                         mutate(opp_id = ifelse(is.na(opp_id), 12, opp_id),
                                opp_score_live = ifelse(is.na(opp_score_live), ghost_score(), opp_score_live)) %>%
                         left_join(still_playing, by = c("id" = "team_id")) %>%
                         left_join(still_playing %>% rename(opp_still_playing=still_playing), by = c("opp_id" = "team_id")) %>%
                         mutate(cw_win = ifelse(totalPointsLive>opp_score_live & still_playing==0 & opp_still_playing==0, 1, 0),
                                cw_gp = ifelse(still_playing==0 & opp_still_playing==0, 1, 0)) %>%
                         mutate(t_wins = cw_win + p_wins,
                                t_gp = cw_gp + p_gp,
                                t_losses = t_gp - t_wins,
                                t_pf = p_pf + totalPointsLive,
                                t_pa = p_pa + opp_score_live) %>%
                         mutate(record = paste0(t_wins, '-', t_losses),
                                ov_record = paste0(ov_wins, '-', ov_losses)) %>%
                         select(logo, name, owner, record, ov_record, t_wins, t_losses, t_pf, t_pa, ov_wins, ov_losses))
  
  team_record = reactive(AllGames %>% 
                           filter(season == CS, week < as.numeric(input$week)) %>%
                           mutate(wins = ifelse(score>opponent_score, 1, 0),
                                  losses = ifelse(opponent_score>score, 1, 0),
                                  ties = ifelse(opponent_score == score & score != 0 , 1, 0)) %>%
                           select(team, wins, losses, ties) %>%
                           group_by(team) %>%
                           summarise(wins = sum(wins, na.rm = T), losses = sum(losses, na.rm = T), ties = sum(ties, na.rm = T)) %>%
                           full_join(team %>% select(owner, id), by = c('team'='owner')) %>%
                           filter(row_number() != 13) %>%
                           mutate(across(c(wins:ties), ~ ifelse(is.na(.), 0, .))) %>%
                           mutate(id = ifelse(team == "Ghost of Dakota Frantum", 12, id)) %>%
                           mutate(record = ifelse(ties==0, paste0("(", wins, "-", losses, ")"), paste0("(", wins, "-", losses, "-", ties, ")"))) %>%
                           select(id, record))
  
  team_ov_record = reactive(AllGames %>% 
                              filter(season == CS,
                                     team != "Ghost of Dakota Frantum",
                                     week < as.numeric(input$week)) %>%
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
  
  team_wl = reactive(AllGames %>% filter(season == CS, !is.na(team)) %>% 
                       group_by(team) %>%
                       mutate(result = ifelse(row_number() < as.numeric(input$week), result, 0.5)) %>%
                       mutate(result = ifelse(is.na(result), 0.5, result)) %>% 
                       summarise(wl = list(result)) %>%
                       left_join(team %>% select(owner, id), by = c('team'='owner')) %>%
                       mutate(id = ifelse(team=='Ghost of Dakota Frantum', 12, id)) %>%
                       select(id, wl))
  
  team_scores = reactive(AllGames %>% filter(season == CS, !is.na(team)) %>% 
                           group_by(team) %>%
                           mutate(score = ifelse(row_number() < as.numeric(input$week), score, NA)) %>%
                           summarise(scores = list(score)) %>%
                           left_join(team %>% select(owner, id), by = c('team'='owner')) %>%
                           mutate(id = ifelse(team=='Ghost of Dakota Frantum', 12, id)) %>%
                           select(id, scores))
  
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
                                      left_join(team_wl(), by = c("team_id" = "id")) %>%
                                      left_join(team_scores(), by = c("team_id" = "id")) %>%
                                      mutate(still_playing = case_when(as.numeric(input$week) == CW ~ still_playing,
                                                                       as.numeric(input$week) < CW ~ 0,
                                                                       as.numeric(input$week) > CW ~ 15)) %>%
                                      mutate(projection = ifelse(still_playing == 0, "", projection)) %>% 
                                      select(logo, name, score, projection, owner, still_playing, record, ov_record, wl, scores) %>%
                                      mutate(win = ifelse(sum(still_playing) == 0 & score>mean(score), 1, 0)) %>%
                                      gt() %>% 
                                      gt_highlight_rows(columns = gt::everything(), rows = win > 0,
                                                        fill = "#ead89e", alpha = 0.8, font_weight = "normal") %>%
                                      gt_merge_stack(col1 = score, col2 = projection,
                                                     font_size = c('25px', '15px'),
                                                     font_weight = c('bold', 'normal')) %>%
                                      gt_merge_stack_with_plots(week = as.numeric(input$week), 
                                                                col1 = name, col2 = owner, col3 = wl, col4 = scores,
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
                                      left_join(team_wl(), by = c("team_id" = "id")) %>%
                                      left_join(team_scores(), by = c("team_id" = "id")) %>%
                                      mutate(still_playing = case_when(as.numeric(input$week) == CW ~ still_playing,
                                                                       as.numeric(input$week) < CW ~ 0,
                                                                       as.numeric(input$week) > CW ~ 15)) %>%
                                      mutate(projection = ifelse(still_playing == 0, "", projection)) %>%
                                      select(logo, name, score, projection, owner, still_playing, record, ov_record, wl, scores) %>%
                                      mutate(win = ifelse(sum(still_playing) == 0 & score>mean(score), 1, 0)) %>%
                                      gt() %>%
                                      gt_highlight_rows(columns = gt::everything(), rows = win > 0,
                                                        fill = "#ead89e", alpha = 0.8) %>%
                                      gt_merge_stack(col1 = score, col2 = projection,
                                                     font_size = c('25px', '15px'),
                                                     font_weight = c('bold', 'normal')) %>%
                                      gt_merge_stack_with_plots(week = as.numeric(input$week),
                                                                col1 = name, col2 = owner, col3 = wl, col4 = scores,
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
                                      left_join(team_wl(), by = c("team_id" = "id")) %>%
                                      left_join(team_scores(), by = c("team_id" = "id")) %>%
                                      mutate(still_playing = case_when(as.numeric(input$week) == CW ~ still_playing,
                                                                       as.numeric(input$week) < CW ~ 0,
                                                                       as.numeric(input$week) > CW ~ 15)) %>%
                                      mutate(projection = ifelse(still_playing == 0, "", projection)) %>%
                                      select(logo, name, score, projection, owner, still_playing, record, ov_record, wl, scores) %>%
                                      mutate(win = ifelse(sum(still_playing) == 0 & score>mean(score), 1, 0)) %>%
                                      gt() %>%
                                      gt_highlight_rows(columns = gt::everything(), rows = win > 0,
                                                        fill = "#ead89e", alpha = 0.8) %>%
                                      gt_merge_stack(col1 = score, col2 = projection,
                                                     font_size = c('25px', '15px'),
                                                     font_weight = c('bold', 'normal')) %>%
                                      gt_merge_stack_with_plots(week = as.numeric(input$week),
                                                                col1 = name, col2 = owner, col3 = wl, col4 = scores,
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
                                      left_join(team_wl(), by = c("team_id" = "id")) %>%
                                      left_join(team_scores(), by = c("team_id" = "id")) %>%
                                      mutate(still_playing = case_when(as.numeric(input$week) == CW ~ still_playing,
                                                                       as.numeric(input$week) < CW ~ 0,
                                                                       as.numeric(input$week) > CW ~ 15)) %>%
                                      mutate(projection = ifelse(still_playing == 0, "", projection)) %>%
                                      select(logo, name, score, projection, owner, still_playing, record, ov_record, wl, scores) %>%
                                      mutate(win = ifelse(sum(still_playing) == 0 & score>mean(score), 1, 0)) %>%
                                      gt() %>%
                                      gt_highlight_rows(columns = gt::everything(), rows = win > 0,
                                                        fill = "#ead89e", alpha = 0.8) %>%
                                      gt_merge_stack(col1 = score, col2 = projection,
                                                     font_size = c('25px', '15px'),
                                                     font_weight = c('bold', 'normal')) %>%
                                      gt_merge_stack_with_plots(week = as.numeric(input$week),
                                                                col1 = name, col2 = owner, col3 = wl, col4 = scores,
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
                                      left_join(team_wl(), by = c("team_id" = "id")) %>%
                                      left_join(team_scores(), by = c("team_id" = "id")) %>%
                                      mutate(still_playing = case_when(as.numeric(input$week) == CW ~ still_playing,
                                                                       as.numeric(input$week) < CW ~ 0,
                                                                       as.numeric(input$week) > CW ~ 15)) %>%
                                      mutate(projection = ifelse(still_playing == 0, "", projection)) %>%
                                      select(logo, name, score, projection, owner, still_playing, record, ov_record, wl, scores) %>%
                                      mutate(win = ifelse(sum(still_playing) == 0 & score>mean(score), 1, 0)) %>%
                                      gt() %>%
                                      gt_highlight_rows(columns = gt::everything(), rows = win > 0,
                                                        fill = "#ead89e", alpha = 0.8) %>%
                                      gt_merge_stack(col1 = score, col2 = projection,
                                                     font_size = c('25px', '15px'),
                                                     font_weight = c('bold', 'normal')) %>%
                                      gt_merge_stack_with_plots(week = as.numeric(input$week),
                                                                col1 = name, col2 = owner, col3 = wl, col4 = scores,
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
                                      left_join(team_wl(), by = c("team_id" = "id")) %>%
                                      left_join(team_scores(), by = c("team_id" = "id")) %>%
                                      mutate(still_playing = case_when(as.numeric(input$week) == CW ~ still_playing,
                                                                       as.numeric(input$week) < CW ~ 0,
                                                                       as.numeric(input$week) > CW ~ 15)) %>%
                                      mutate(projection = ifelse(still_playing == 0, "", projection)) %>%
                                      select(logo, name, score, projection, owner, still_playing, record, ov_record, wl, scores) %>%
                                      mutate(win = ifelse(sum(still_playing) == 0 & score>mean(score), 1, 0)) %>%
                                      gt() %>%
                                      gt_highlight_rows(columns = gt::everything(), rows = win > 0,
                                                        fill = "#ead89e", alpha = 0.8) %>%
                                      gt_merge_stack(col1 = score, col2 = projection,
                                                     font_size = c('25px', '15px'),
                                                     font_weight = c('bold', 'normal')) %>%
                                      gt_merge_stack_with_plots(week = as.numeric(input$week),
                                                                col1 = name, col2 = owner, col3 = wl, col4 = scores,
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
  
  output$standings = render_gt(standings() %>%
                                 arrange(desc(t_wins), desc(t_pf)) %>%
                                 select(logo, name, owner, record, ov_record, t_pf, t_pa) %>%
                                 gt() %>%
                                 gt_img_circle(logo, height = 40) %>%
                                 gt_merge_stack(name, owner) %>%
                                 gt_theme_pff() %>%
                                 cols_label(logo = "",
                                            name = "Team",
                                            record = "Record",
                                            ov_record = "Overall Record",
                                            t_pf = "PF",
                                            t_pa = "PA") %>%
                                 cols_width(logo ~ px(50),
                                            name ~ px(90),
                                            record ~ px(50),
                                            ov_record ~ px(50),
                                            t_pf ~ px(60),
                                            t_pa ~ px(60)) %>%
                                 cols_align(align = "center", columns = c(logo, record, ov_record, t_pf, t_pa)) %>%
                                 fmt_number(columns = c(t_pf, t_pa), decimals = 1) %>%
                                 tab_header(title = html("<center><b>STANDINGS</b></center>")) %>%
                                 tab_options(heading.title.font.size = "18px",
                                             heading.background.color = "grey"))
  
}

shinyApp(ui, server)
