library(shiny)
library(pacman)
library(dplyr)
library(readxl)
library(magrittr)
library(janitor)
library(httr)
library(jsonlite)
library(gt)
library(gtExtras)
library(tidyr)
library(ggplot2)
library(shinyWidgets)
library(ggthemes)
library(ggimage)
library(rsvg)
library(bslib)
library(shinyMobile)
library(svglite)
library(shiny.pwa)

#p_load(dplyr, readxl, magrittr, janitor, httr, jsonlite, gt, gtExtras, tidyr, ggplot2, shinyWidgets, ggthemes, ggimage, rsvg, bslib, shinyMobile, shiny.pwa)

AllGames = read.csv("https://raw.githubusercontent.com/jpatak16/WOFFL/main/WOFFL_stats_portal/AllGames.csv") %>%
  clean_names() %>%
  mutate(result = ifelse(score > opponent_score, 1, 0))

sim_results = read.csv("https://raw.githubusercontent.com/jpatak16/WOFFL/main/WOFFL_stats_portal/sim_results_2023.csv") %>% clean_names() %>%
  arrange(desc(champion)) %>%
  group_by(wk) %>%
  mutate(rank = rank(-champion)) %>%
  ungroup()
  

delta_sim_results = sim_results %>%
  filter(wk == max(wk) | wk == max(wk) - 1) %>%
  arrange(wk) %>%
  group_by(team) %>%
  summarise(delta_rank = diff(rank),
            delta_wins = diff(wins),
            delta_pf = diff(pf),
            delta_playoffs = diff(playoffs),
            delta_frb = diff(frb),
            delta_champion = diff(champion)) %>%
  mutate(delta_rank = delta_rank * -1,
         delta_wins = round(delta_wins, digits = 1),
         delta_pf = round(delta_pf, digits = 0),
         delta_playoffs = round(delta_playoffs*100, digits = 1),
         delta_frb = round(delta_frb*100, digits = 1),
         delta_champion = round(delta_champion*100, digits = 1),) %>%
  mutate(delta_rank = case_when(delta_rank > 0 ~ paste0("+", delta_rank),
                                delta_rank == 0 ~ "",
                                .default = as.character(delta_rank)),
         delta_wins = case_when(delta_wins > 0 ~ paste0("+", delta_wins),
                                delta_wins == 0 ~ "",
                                .default = as.character(delta_wins)),
         delta_pf = case_when(delta_pf > 0 ~ paste0("+", delta_pf),
                                delta_pf == 0 ~ "",
                                .default = as.character(delta_pf)),
         delta_playoffs = case_when(delta_playoffs > 0 ~ paste0("+", delta_playoffs, "%"),
                                delta_playoffs == 0 ~ "",
                                .default = paste0(delta_playoffs, "%")),
         delta_frb = case_when(delta_frb > 0 ~ paste0("+", delta_frb, "%"),
                                    delta_frb == 0 ~ "",
                                    .default = paste0(delta_frb, "%")),
         delta_champion = case_when(delta_champion > 0 ~ paste0("+", delta_champion, "%"),
                                    delta_champion == 0 ~ "",
                                    .default = paste0(delta_champion, "%")),)

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

round_real <- function(x, digits) {
  posneg = sign(x)
  z = abs(x)*10^digits
  z = z + 0.5 + sqrt(.Machine$double.eps)
  z = trunc(z)
  z = z/10^digits
  z*posneg
}
gt_merge_img_circle <- function(gt_object, col1, col2, col3, palette = c("black", "grey"), 
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
                                            <div style='text-align:center;line-height:{font_size[2]}'><span style ='font-weight:{font_weight[2]};color:{colors[2]};font-size:{font_size[2]}'>{data_in2}</div>
                                            <div style='text-align:center;line-height:{font_size[2]}'><span style ='font-weight:{font_weight[2]};color:{colors[2]};font-size:{font_size[2]}'>{data_in3}</div>")}) %>% 
    cols_hide(columns = c({{col2}}, {{col3}}))}

gt_merge_stack_with_plots <- function(gt_object, week, col1, col2, col3, col4, palette = c("black", "grey"), 
                              ..., small_cap = TRUE, font_size = c("14px", "10px"), font_weight = c("bold", "bold"), 
                              max_wins=16, width=max_wins/.85, color1="#013369", color2="#D50A0A", color3="gray",
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
gt_merge_stack_delta <- function(gt_object, col1, col2, ..., small_cap = TRUE,
                           font_size = c("14px", "10px"), font_weight = c("bold", "bold")) {
  
  
  col1_bare <- rlang::enexpr(col1) %>% rlang::as_string()
  
  row_name_var <- gt_object[["_boxhead"]][["var"]][which(gt_object[["_boxhead"]][["type"]] == "stub")]
  
  # segment data with bare string column name
  data_in <- gt_index(gt_object, column = {{ col2 }})
  
  gt_object %>%
    text_transform(
      locations = if (isTRUE(row_name_var == col1_bare)) {
        cells_stub(rows = gt::everything())
      } else {
        cells_body(columns = {{ col1 }})
      },
      fn = function(x) {
        if (small_cap) {
          font_variant <- "small-caps"
        } else {
          font_variant <- "normal"
        }
        
        glue::glue(
          "<div style='line-height:{font_size[1]}'><span style='font-weight:{font_weight[1]};font-variant:{font_variant};color:{'black'};font-size:{font_size[1]}'>{x}</span></div>
        <div style='line-height:{font_size[2]}'><span style ='font-weight:{font_weight[2]};color:{case_when(grepl('+', data_in, fixed=T) ~ '#66FF00', grepl('-', data_in, fixed=T) ~ 'red')};font-size:{font_size[2]}'>{data_in}</span></div>"
        )
      }
    ) %>%
    cols_hide(columns = {{ col2 }})
}



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

still_playing = data.frame(id = dat$schedule$away$teamId, proj = dat$schedule$away$totalProjectedPointsLive, score = dat$schedule$away$totalPointsLive) %>%
  rbind(data.frame(id = dat$schedule$home$teamId, proj = dat$schedule$home$totalProjectedPointsLive, score = dat$schedule$home$totalPointsLive)) %>%
  filter(!is.na(score)) %>%
  mutate(still_playing = ifelse(round2(proj, digits=2) == score, 0, 1),
         still_playing = ifelse(id==12, sum(still_playing), still_playing)) %>%
  select(id, still_playing)

ui = f7Page(div(h1(span("White Oak Fantasy Football League", style = 'color:#8A8A8A; text-shadow: black 0.0em 0.1em 0.2em')), 
                h2(span(img(src="3d.jpg", height = 75, width = 105), style = "float:right;margin-top:-50px;margin-right:30px;"),
                   span("Stats Portal", style = 'clear:right; font-weight: bold; color:#FFFFFF; text-shadow: black 0.0em 0.18em 0.2em')
                   ),
                style = 'margin-top:-20px; padding-top:10px; padding-bottom:10px; background-color:#580515'),
            allowPWA = TRUE, 
            f7Tabs(id = "tabs", style = "toolbar",
                   f7Tab(tabName = "Scoreboard",
                         active = TRUE,
                         icon = f7Icon("square_fill_line_vertical_square"),
                         h1(div("Scoreboard", style = "color:#580515;font-weight:bolder;font-size:40px;text-align:center;")),
                         fluidRow(column(12, align='center', f7Select("week", "Week Selector", week_selector_options, selected = CW, width = "60%")), 
                                  style = 'padding-top:10px;'),
                         fluidRow(column(12, align='center', uiOutput("playoffs"))),
                         pwa("https://jpatak.shinyapps.io/WOFFL/", output = "www", title = "WOFFL", icon = "www/iconlogo.png"),
                         gt_output('wk_matchup_1'),
                         gt_output('wk_matchup_2'),
                         gt_output('wk_matchup_3'),
                         gt_output('wk_matchup_4'),
                         gt_output('wk_matchup_5'),
                         fluidRow(gt_output('wk_matchup_6'), style = 'padding-bottom:200px')),
                   f7Tab(tabName = "Standings",
                         icon = icon("list-ol"),
                         h1(div("Standings", style = "color:#580515;font-weight:bolder;font-size:40px;text-align:center;")),
                         fluidRow(gt_output('standings'), style = 'padding-bottom:200px')),
                   f7Tab(tabName = "CurrentSeeding",
                         icon = html("<i class='fa-solid fa-code-fork fa-rotate-270'></i>"),
                         h1(div("Current Seeding", style = "color:#580515;font-weight:bolder;font-size:40px;text-align:center;")),
                         gt_output("seed1"),
                         gt_output("seed2"),
                         gt_output("seed3"),
                         gt_output("seed4"),
                         gt_output("seed5"),
                         fluidRow(gt_output('seed6'), style = 'padding-bottom:200px')),
                   f7Tab(tabName = "PowerRankings",
                         icon = icon("medal"),
                         h1(div("Power Rankings", style = "color:#580515;font-weight:bolder;font-size:40px;text-align:center;")),
                         fluidRow(gt_output('powerRankings'), style = 'padding-bottom:200px')),
                   f7Tab(tabName = "PowerRankingsTrends",
                         icon = icon("arrow-trend-up"),
                         h1(div("Power Ranking Trends", style = "color:#580515;font-weight:bolder;font-size:40px;text-align:center;")),
                         dropdown(
                           label = "Plot Inputs", style = "unite", size = "sm", 
                           icon = icon('gear'),
                           f7Select("yaxis", "Y-Axis", 
                                       choices = c("Rank", "Projected Wins"="Projected_Wins", "Projected PF"="Projected_PF", "Playoff Berth Liklihood"="Playoff_Berth_Liklihood", "First Round Bye Liklihood"="First_Round_Bye_Liklihood", "Championship Liklihood"="Championship_Liklihood"), 
                                       selected = sample(c("Rank", "Projected Wins"="Projected_Wins", "Projected PF"="Projected_PF", "Playoff Berth Liklihood"="Playoff_Berth_Liklihood", "First Round Bye Liklihood"="First_Round_Bye_Liklihood", "Championship Liklihood"="Championship_Liklihood"), 1)),
                           f7CheckboxGroup("teams", "Selected Teams", choices = team$owner[1:11], selected = sample(team$owner[1:11], 1)),
                           f7Button("selectall", label="Select/Deselect All", rounded = T)),
                         fluidRow(plotOutput("powerRankingPlot", height = "600px"), style = 'padding-bottom:325px; padding-top:20px;')),
                   f7Tab(tabName = "Survivor",
                         icon = icon("skull-crossbones"),
                         h1(div("Survivor Contest", style = "color:#580515;font-weight:bolder;font-size:40px;text-align:center;")),
                         gt_output("sur_wk10"),
                         gt_output("sur_wk9"),
                         gt_output("sur_wk8"),
                         gt_output("sur_wk7"),
                         gt_output("sur_wk6"),
                         gt_output("sur_wk5"),
                         gt_output("sur_wk4"),
                         gt_output("sur_wk3"),
                         gt_output("sur_wk2"),
                         fluidRow(gt_output('sur_wk1'), style = 'padding-bottom:200px'))
                   )
            )
                

server = function(input, output, session) {
  
  output$playoffs = renderUI(if(as.numeric(input$week) > 13){img(src="playoffs.gif")}
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
                         left_join(still_playing) %>%
                         left_join(still_playing %>% rename(opp_still_playing=still_playing), by = c("opp_id" = "id")) %>%
                         mutate(cw_win = ifelse(totalPointsLive>opp_score_live & still_playing==0 & opp_still_playing==0 & CW>p_gp, 1, 0),
                                cw_gp = ifelse(still_playing==0 & opp_still_playing==0 & CW>p_gp, 1, 0)) %>%
                         mutate(t_wins = cw_win + p_wins,
                                t_gp = cw_gp + p_gp,
                                t_losses = t_gp - t_wins,
                                t_pf = p_pf + totalPointsLive,
                                t_pa = p_pa + opp_score_live) %>%
                         mutate(record = paste0(t_wins, '-', t_losses),
                                ov_record = paste0(ov_wins, '-', ov_losses),
                                t_wp = t_wins/t_gp) %>%
                         select(logo, name, owner, record, ov_record, t_wins, t_losses, t_wp, t_gp, t_pf, t_pa, ov_wins, ov_losses, p_pf))
  
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
                                      left_join(still_playing, by = c("team_id"="id")) %>%
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
                                                     font_size = c('20px', '15px'),
                                                     font_weight = c('bold', 'normal')) %>%
                                      gt_merge_stack_with_plots(week = as.numeric(input$week), 
                                                                col1 = name, col2 = owner, col3 = wl, col4 = scores,
                                                                font_size = c("20px", "15px"),
                                                                font_weight = c("bold", "normal"),
                                                                type = "shaded") %>%
                                      gt_merge_img_circle(col1 = logo, col2 = record, col3 = ov_record, height = 75) %>%
                                      cols_width(logo ~ pct(25),
                                                 name ~ pct(50),
                                                 score ~ pct(25)) %>%
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
                                      left_join(still_playing, by=c("team_id"="id")) %>%
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
                                                     font_size = c('20px', '15px'),
                                                     font_weight = c('bold', 'normal')) %>%
                                      gt_merge_stack_with_plots(week = as.numeric(input$week),
                                                                col1 = name, col2 = owner, col3 = wl, col4 = scores,
                                                                font_size = c("20px", "15px"),
                                                                font_weight = c("bold", "normal"),
                                                                type = "shaded") %>%
                                      gt_merge_img_circle(col1 = logo, col2 = record, col3 = ov_record, height = 75) %>%
                                      cols_width(logo ~ pct(25),
                                                 name ~ pct(50),
                                                 score ~ pct(25)) %>%
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
                                      left_join(still_playing, by=c("team_id"="id")) %>%
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
                                                     font_size = c('20px', '15px'),
                                                     font_weight = c('bold', 'normal')) %>%
                                      gt_merge_stack_with_plots(week = as.numeric(input$week),
                                                                col1 = name, col2 = owner, col3 = wl, col4 = scores,
                                                                font_size = c("20px", "15px"),
                                                                font_weight = c("bold", "normal"),
                                                                type = "shaded") %>%
                                      gt_merge_img_circle(col1 = logo, col2 = record, col3 = ov_record, height = 75) %>%
                                      cols_width(logo ~ pct(25),
                                                 name ~ pct(50),
                                                 score ~ pct(25)) %>%
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
                                      left_join(still_playing, by=c("team_id"="id")) %>%
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
                                                     font_size = c('20px', '15px'),
                                                     font_weight = c('bold', 'normal')) %>%
                                      gt_merge_stack_with_plots(week = as.numeric(input$week),
                                                                col1 = name, col2 = owner, col3 = wl, col4 = scores,
                                                                font_size = c("20px", "15px"),
                                                                font_weight = c("bold", "normal"),
                                                                type = "shaded") %>%
                                      gt_merge_img_circle(col1 = logo, col2 = record, col3 = ov_record, height = 75) %>%
                                      cols_width(logo ~ pct(25),
                                                 name ~ pct(50),
                                                 score ~ pct(25)) %>%
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
                                      left_join(still_playing, by=c("team_id"="id")) %>%
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
                                                     font_size = c('20px', '15px'),
                                                     font_weight = c('bold', 'normal')) %>%
                                      gt_merge_stack_with_plots(week = as.numeric(input$week),
                                                                col1 = name, col2 = owner, col3 = wl, col4 = scores,
                                                                font_size = c("20px", "15px"),
                                                                font_weight = c("bold", "normal"),
                                                                type = "shaded") %>%
                                      gt_merge_img_circle(col1 = logo, col2 = record, col3 = ov_record, height = 75) %>%
                                      cols_width(logo ~ pct(25),
                                                 name ~ pct(50),
                                                 score ~ pct(25)) %>%
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
                                      left_join(still_playing, by=c("team_id"="id")) %>%
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
                                                     font_size = c('20px', '15px'),
                                                     font_weight = c('bold', 'normal'),) %>%
                                      gt_merge_stack_with_plots(week = as.numeric(input$week),
                                                                col1 = name, col2 = owner, col3 = wl, col4 = scores,
                                                                font_size = c("20px", "15px"),
                                                                font_weight = c("bold", "normal"),
                                                                type = "shaded") %>%
                                      gt_merge_img_circle(col1 = logo, col2 = record, col3 = ov_record, height = 75) %>%
                                      cols_width(logo ~ pct(25),
                                                 name ~ pct(50),
                                                 score ~ pct(25)) %>%
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
                                 arrange(desc(t_wp), desc(t_wins), t_losses, desc(t_pf)) %>%
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
                                 cols_width(logo ~ pct(14),
                                            name ~ pct(25),
                                            record ~ pct(14),
                                            ov_record ~ pct(14),
                                            t_pf ~ pct(16),
                                            t_pa ~ pct(16)) %>%
                                 cols_align(align = "center", columns = c(logo, record, ov_record, t_pf, t_pa)) %>%
                                 fmt_number(columns = c(t_pf, t_pa), decimals = 1))
  
  output$seed1 = render_gt(standings() %>%
                             filter(t_wp >= standings() %>% arrange(desc(t_wp)) %>% filter(row_number() == 1) %>% select(t_wp) %>% as.numeric() - 1) %>%
                             left_join(AllGames %>% filter(season == CS,
                                                           week <= 13,
                                                           team %in% (standings() %>% arrange(desc(t_wp)) %>% filter(t_wp == standings() %>% arrange(desc(t_wp)) %>% filter(row_number() == 1) %>% select(t_wp) %>% as.numeric()) %>% pull(owner)),
                                                           opponent %in% (standings() %>% arrange(desc(t_wp)) %>% filter(t_wp == standings() %>% arrange(desc(t_wp)) %>% filter(row_number() == 1) %>% select(t_wp) %>% as.numeric()) %>% pull(owner))) %>% 
                                         group_by(team) %>% 
                                         summarise(tb_h2h_wins = sum(result, na.rm=T)),
                                       by = c('owner'='team')) %>%
                             mutate(tb_h2h_wins = ifelse(is.na(tb_h2h_wins), 0, tb_h2h_wins)) %>%
                             left_join(AllGames %>% filter(season == CS,
                                                           week <= 13,
                                                           team %in% (standings() %>% arrange(desc(t_wp)) %>% filter(t_wp == standings() %>% arrange(desc(t_wp)) %>% filter(row_number() == 1) %>% select(t_wp) %>% as.numeric()) %>% pull(owner)),
                                                           opponent %in% (standings() %>% arrange(desc(t_wp)) %>% filter(t_wp == standings() %>% arrange(desc(t_wp)) %>% filter(row_number() == 1) %>% select(t_wp) %>% as.numeric()) %>% pull(owner))) %>% 
                                         group_by(team) %>% 
                                         summarise(tb_h2h_gp = sum(!is.na(score))),
                                       by = c('owner'='team')) %>%
                             mutate(tb_h2h_gp = ifelse(is.na(tb_h2h_gp), 0, as.numeric(tb_h2h_gp)),
                                    tb_h2h_losses = tb_h2h_gp - tb_h2h_wins,
                                    tb_h2h_wp = tb_h2h_wins/tb_h2h_gp,
                                    tb_h2h_record = paste0(tb_h2h_wins, "-", tb_h2h_losses)) %>%
                             mutate(tb_h2h_wp = ifelse(tb_h2h_gp==0, 1, tb_h2h_wp)) %>%
                             arrange(desc(t_wp), desc(t_wins), t_losses, desc(tb_h2h_wp), desc(t_pf)) %>%
                             filter(row_number() <= 4) %>%
                             mutate(tb_h2h_record = ifelse(t_wins != standings() %>% arrange(desc(t_wins)) %>% filter(row_number() == 1) %>% select(t_wins) %>% as.numeric(), "", tb_h2h_record)) %>%
                             select(logo, name, owner, record, tb_h2h_record, t_pf) %>%
                             gt() %>%
                             gt_img_circle(logo, height = 40) %>%
                             gt_merge_stack(name, owner) %>%
                             gt_theme_pff() %>%
                             cols_label(logo = "",
                                        name = "Team",
                                        record = "Record",
                                        t_pf = "PF",
                                        tb_h2h_record = "TB Record") %>%
                             cols_width(logo ~ pct(14),
                                        name ~ pct(30),
                                        record ~ pct(18),
                                        t_pf ~ pct(19),
                                        tb_h2h_record ~ pct(18)) %>%
                             cols_align(align = "center", columns = c(logo, record, t_pf, tb_h2h_record)) %>%
                             fmt_number(columns = c(t_pf), decimals = 1) %>%
                             tab_header(title = html("<center><b>1st Seed Race</b></center>")) %>%
                             tab_options(heading.title.font.size = "18px",
                                         heading.background.color = "grey") %>%
                             tab_style(style = list(cell_fill(color = "gold")),
                               locations = cells_body(rows = 1)))
  
  current_seed_1 = reactive(standings() %>%
                              filter(t_wins >= standings() %>% arrange(desc(t_wins)) %>% filter(row_number() == 1) %>% select(t_wins) %>% as.numeric() - 1) %>%
                              left_join(AllGames %>% filter(season == CS,
                                                            week <= 13,
                                                            team %in% (standings() %>% arrange(desc(t_wins)) %>% filter(t_wins == standings() %>% arrange(desc(t_wins)) %>% filter(row_number() == 1) %>% select(t_wins) %>% as.numeric()) %>% pull(owner)),
                                                            opponent %in% (standings() %>% arrange(desc(t_wins)) %>% filter(t_wins == standings() %>% arrange(desc(t_wins)) %>% filter(row_number() == 1) %>% select(t_wins) %>% as.numeric()) %>% pull(owner))) %>% 
                                          group_by(team) %>% 
                                          summarise(tb_h2h_wins = sum(result, na.rm=T)),
                                        by = c('owner'='team')) %>%
                              mutate(tb_h2h_wins = ifelse(is.na(tb_h2h_wins), 0, tb_h2h_wins)) %>%
                              left_join(AllGames %>% filter(season == CS,
                                                            week <= 13,
                                                            team %in% (standings() %>% arrange(desc(t_wins)) %>% filter(t_wins == standings() %>% arrange(desc(t_wins)) %>% filter(row_number() == 1) %>% select(t_wins) %>% as.numeric()) %>% pull(owner)),
                                                            opponent %in% (standings() %>% arrange(desc(t_wins)) %>% filter(t_wins == standings() %>% arrange(desc(t_wins)) %>% filter(row_number() == 1) %>% select(t_wins) %>% as.numeric()) %>% pull(owner))) %>% 
                                          group_by(team) %>% 
                                          summarise(tb_h2h_gp = sum(!is.na(score))),
                                        by = c('owner'='team')) %>%
                              mutate(tb_h2h_gp = ifelse(is.na(tb_h2h_gp), 0, as.numeric(tb_h2h_gp)),
                                     tb_h2h_losses = tb_h2h_gp - tb_h2h_wins,
                                     tb_h2h_wp = tb_h2h_wins/tb_h2h_gp,
                                     tb_h2h_record = paste0(tb_h2h_wins, "-", tb_h2h_losses)) %>%
                              mutate(tb_h2h_wp = ifelse(tb_h2h_gp==0, 1, tb_h2h_wp)) %>%
                              arrange(desc(t_wp), desc(t_wins), t_losses, desc(tb_h2h_wp), desc(t_pf)) %>%
                              filter(row_number() == 1) %>%
                              pull(owner))
  
  output$seed2 = render_gt(standings() %>%
                             filter(owner != current_seed_1()) %>%
                             arrange(desc(t_pf)) %>%
                             filter(row_number() <= 3) %>%
                             select(logo, name, owner, t_pf) %>%
                             gt() %>%
                             gt_img_circle(logo, height = 40) %>%
                             gt_merge_stack(name, owner) %>%
                             gt_theme_pff() %>%
                             cols_label(logo = "",
                                        name = "Team",
                                        t_pf = "PF") %>%
                             cols_width(logo ~ pct(14),
                                        name ~ pct(30),
                                        t_pf ~ pct(56)) %>%
                             cols_align(align = "center", columns = c(logo, t_pf)) %>%
                             fmt_number(columns = c(t_pf), decimals = 1) %>%
                             tab_header(title = html("<center><b>2nd Seed Race</b></center>")) %>%
                             tab_options(heading.title.font.size = "18px",
                                         heading.background.color = "grey") %>%
                             tab_style(style = list(cell_fill(color = "gold")),
                                       locations = cells_body(rows = 1)))
  
  current_seed_2 = reactive(standings() %>%
                              filter(owner != current_seed_1()) %>%
                              arrange(desc(t_pf)) %>%
                              filter(row_number() == 1) %>%
                              pull(owner))
  
  output$seed3 = render_gt(standings() %>%
                             filter(owner != current_seed_1(),
                                    owner != current_seed_2()) %>%
                             filter(t_wins >= standings() %>% filter(owner != current_seed_1(), owner != current_seed_2()) %>% arrange(desc(t_wins)) %>% filter(row_number() == 1) %>% select(t_wins) %>% as.numeric() - 1) %>%
                             left_join(AllGames %>% filter(season == CS,
                                                           week <= 13,
                                                           team %in% (standings() %>% filter(owner != current_seed_1(), owner != current_seed_2()) %>% arrange(desc(t_wins)) %>% filter(t_wins == standings() %>% arrange(desc(t_wins)) %>% filter(row_number() == 1) %>% select(t_wins) %>% as.numeric()) %>% pull(owner)),
                                                           opponent %in% (standings() %>% filter(owner != current_seed_1(), owner != current_seed_2())%>% arrange(desc(t_wins)) %>% filter(t_wins == standings() %>% arrange(desc(t_wins)) %>% filter(row_number() == 1) %>% select(t_wins) %>% as.numeric()) %>% pull(owner))) %>% 
                                         group_by(team) %>% 
                                         summarise(tb_h2h_wins = sum(result, na.rm=T)),
                                       by = c('owner'='team')) %>%
                             mutate(tb_h2h_wins = ifelse(is.na(tb_h2h_wins), 0, tb_h2h_wins)) %>%
                             left_join(AllGames %>% filter(season == CS,
                                                           week <= 13,
                                                           team %in% (standings() %>% filter(owner != current_seed_1(), owner != current_seed_2()) %>% arrange(desc(t_wins)) %>% filter(t_wins == standings() %>% arrange(desc(t_wins)) %>% filter(row_number() == 1) %>% select(t_wins) %>% as.numeric()) %>% pull(owner)),
                                                           opponent %in% (standings() %>% filter(owner != current_seed_1(), owner != current_seed_2()) %>% arrange(desc(t_wins)) %>% filter(t_wins == standings() %>% arrange(desc(t_wins)) %>% filter(row_number() == 1) %>% select(t_wins) %>% as.numeric()) %>% pull(owner))) %>% 
                                         group_by(team) %>% 
                                         summarise(tb_h2h_gp = sum(!is.na(score))),
                                       by = c('owner'='team')) %>%
                             mutate(tb_h2h_gp = ifelse(is.na(tb_h2h_gp), 0, as.numeric(tb_h2h_gp)),
                                    tb_h2h_losses = tb_h2h_gp - tb_h2h_wins,
                                    tb_h2h_wp = tb_h2h_wins/tb_h2h_gp,
                                    tb_h2h_record = paste0(tb_h2h_wins, "-", tb_h2h_losses)) %>%
                             mutate(tb_h2h_wp = ifelse(tb_h2h_gp==0, 1, tb_h2h_wp)) %>%
                             arrange(desc(t_wp), desc(t_wins), t_losses, desc(tb_h2h_wp), desc(t_pf)) %>%
                             filter(row_number() <= 4) %>%
                             mutate(tb_h2h_record = ifelse(t_wins != standings() %>% filter(owner != current_seed_1(), owner != current_seed_2(), owner != current_seed_3()) %>% arrange(desc(t_wins)) %>% filter(row_number() == 1) %>% select(t_wins) %>% as.numeric(), "", tb_h2h_record)) %>%
                             select(logo, name, owner, record, tb_h2h_record, t_pf) %>%
                             gt() %>%
                             gt_img_circle(logo, height = 40) %>%
                             gt_merge_stack(name, owner) %>%
                             gt_theme_pff() %>%
                             cols_label(logo = "",
                                        name = "Team",
                                        record = "Record",
                                        t_pf = "PF",
                                        tb_h2h_record = "TB Record") %>%
                             cols_width(logo ~ pct(14),
                                        name ~ pct(30),
                                        record ~ pct(18),
                                        t_pf ~ pct(19),
                                        tb_h2h_record ~ pct(18)) %>%
                             cols_align(align = "center", columns = c(logo, record, t_pf, tb_h2h_record)) %>%
                             fmt_number(columns = c(t_pf), decimals = 1) %>%
                             tab_header(title = html("<center><b>3rd Seed Race</b></center>")) %>%
                             tab_options(heading.title.font.size = "18px",
                                         heading.background.color = "grey") %>%
                             tab_style(style = list(cell_fill(color = "gold")),
                                       locations = cells_body(rows = 1)))
  
  current_seed_3 = reactive(standings() %>%
                              filter(owner != current_seed_1(),
                                     owner != current_seed_2()) %>%
                              filter(t_wins >= standings() %>% arrange(desc(t_wins)) %>% filter(row_number() == 1) %>% select(t_wins) %>% as.numeric() - 1) %>%
                              left_join(AllGames %>% filter(season == CS,
                                                            week <= 13,
                                                            team %in% (standings() %>% filter(owner != current_seed_1(), owner != current_seed_2()) %>% arrange(desc(t_wins)) %>% filter(t_wins == standings() %>% arrange(desc(t_wins)) %>% filter(row_number() == 1) %>% select(t_wins) %>% as.numeric()) %>% pull(owner)),
                                                            opponent %in% (standings() %>% filter(owner != current_seed_1(), owner != current_seed_2()) %>% arrange(desc(t_wins)) %>% filter(t_wins == standings() %>% arrange(desc(t_wins)) %>% filter(row_number() == 1) %>% select(t_wins) %>% as.numeric()) %>% pull(owner))) %>% 
                                          group_by(team) %>% 
                                          summarise(tb_h2h_wins = sum(result, na.rm=T)),
                                        by = c('owner'='team')) %>%
                              mutate(tb_h2h_wins = ifelse(is.na(tb_h2h_wins), 0, tb_h2h_wins)) %>%
                              left_join(AllGames %>% filter(season == CS,
                                                            week <= 13,
                                                            team %in% (standings() %>% filter(owner != current_seed_1(), owner != current_seed_2()) %>% arrange(desc(t_wins)) %>% filter(t_wins == standings() %>% arrange(desc(t_wins)) %>% filter(row_number() == 1) %>% select(t_wins) %>% as.numeric()) %>% pull(owner)),
                                                            opponent %in% (standings() %>% filter(owner != current_seed_1(), owner != current_seed_2())%>% arrange(desc(t_wins)) %>% filter(t_wins == standings() %>% arrange(desc(t_wins)) %>% filter(row_number() == 1) %>% select(t_wins) %>% as.numeric()) %>% pull(owner))) %>% 
                                          group_by(team) %>% 
                                          summarise(tb_h2h_gp = sum(!is.na(score))),
                                        by = c('owner'='team')) %>%
                              mutate(tb_h2h_gp = ifelse(is.na(tb_h2h_gp), 0, as.numeric(tb_h2h_gp)),
                                     tb_h2h_losses = tb_h2h_gp - tb_h2h_wins,
                                     tb_h2h_wp = tb_h2h_wins/tb_h2h_gp,
                                     tb_h2h_record = paste0(tb_h2h_wins, "-", tb_h2h_losses)) %>%
                              mutate(tb_h2h_wp = ifelse(tb_h2h_gp==0, 1, tb_h2h_wp)) %>%
                              arrange(desc(t_wp), desc(t_wins), t_losses, desc(tb_h2h_wp), desc(t_pf)) %>%
                              filter(row_number() == 1) %>%
                              pull(owner))
  
  output$seed4 = render_gt(standings() %>%
                             filter(owner != current_seed_1(),
                                    owner != current_seed_2(),
                                    owner != current_seed_3()) %>%
                             filter(t_wins >= standings() %>% filter(owner != current_seed_1(), owner != current_seed_2(), owner != current_seed_3()) %>% arrange(desc(t_wins)) %>% filter(row_number() == 1) %>% select(t_wins) %>% as.numeric() - 1) %>%
                             left_join(AllGames %>% filter(season == CS,
                                                           week <= 13,
                                                           team %in% (standings() %>% filter(owner != current_seed_1(), owner != current_seed_2(), owner != current_seed_3()) %>% arrange(desc(t_wins)) %>% filter(t_wins == standings() %>% arrange(desc(t_wins)) %>% filter(row_number() == 1) %>% select(t_wins) %>% as.numeric()) %>% pull(owner)),
                                                           opponent %in% (standings() %>% filter(owner != current_seed_1(), owner != current_seed_2(), owner != current_seed_3()) %>% arrange(desc(t_wins)) %>% filter(t_wins == standings() %>% arrange(desc(t_wins)) %>% filter(row_number() == 1) %>% select(t_wins) %>% as.numeric()) %>% pull(owner))) %>% 
                                         group_by(team) %>% 
                                         summarise(tb_h2h_wins = sum(result, na.rm=T)),
                                       by = c('owner'='team')) %>%
                             mutate(tb_h2h_wins = ifelse(is.na(tb_h2h_wins), 0, tb_h2h_wins)) %>%
                             left_join(AllGames %>% filter(season == CS,
                                                           week <= 13,
                                                           team %in% (standings() %>% filter(owner != current_seed_1(), owner != current_seed_2(), owner != current_seed_3()) %>% arrange(desc(t_wins)) %>% filter(t_wins == standings() %>% arrange(desc(t_wins)) %>% filter(row_number() == 1) %>% select(t_wins) %>% as.numeric()) %>% pull(owner)),
                                                           opponent %in% (standings() %>% filter(owner != current_seed_1(), owner != current_seed_2(), owner != current_seed_3()) %>% arrange(desc(t_wins)) %>% filter(t_wins == standings() %>% arrange(desc(t_wins)) %>% filter(row_number() == 1) %>% select(t_wins) %>% as.numeric()) %>% pull(owner))) %>% 
                                         group_by(team) %>% 
                                         summarise(tb_h2h_gp = sum(!is.na(score))),
                                       by = c('owner'='team')) %>%
                             mutate(tb_h2h_gp = ifelse(is.na(tb_h2h_gp), 0, as.numeric(tb_h2h_gp)),
                                    tb_h2h_losses = tb_h2h_gp - tb_h2h_wins,
                                    tb_h2h_wp = tb_h2h_wins/tb_h2h_gp,
                                    tb_h2h_record = paste0(tb_h2h_wins, "-", tb_h2h_losses)) %>%
                             mutate(tb_h2h_wp = ifelse(tb_h2h_gp==0, 1, tb_h2h_wp)) %>%
                             arrange(desc(t_wp), desc(t_wins), t_losses, desc(tb_h2h_wp), desc(t_pf)) %>%
                             filter(row_number() <= 4) %>%
                             mutate(tb_h2h_record = ifelse(t_wins != standings() %>% filter(owner != current_seed_1(), owner != current_seed_2(), owner != current_seed_3()) %>% arrange(desc(t_wins)) %>% filter(row_number() == 1) %>% select(t_wins) %>% as.numeric(), "", tb_h2h_record)) %>%
                             select(logo, name, owner, record, tb_h2h_record, t_pf) %>%
                             gt() %>%
                             gt_img_circle(logo, height = 40) %>%
                             gt_merge_stack(name, owner) %>%
                             gt_theme_pff() %>%
                             cols_label(logo = "",
                                        name = "Team",
                                        record = "Record",
                                        t_pf = "PF",
                                        tb_h2h_record = "TB Record") %>%
                             cols_width(logo ~ pct(14),
                                        name ~ pct(30),
                                        record ~ pct(18),
                                        t_pf ~ pct(19),
                                        tb_h2h_record ~ pct(18)) %>%
                             cols_align(align = "center", columns = c(logo, record, t_pf, tb_h2h_record)) %>%
                             fmt_number(columns = c(t_pf), decimals = 1) %>%
                             tab_header(title = html("<center><b>4th Seed Race</b></center>")) %>%
                             tab_options(heading.title.font.size = "18px",
                                         heading.background.color = "grey") %>%
                             tab_style(style = list(cell_fill(color = "gold")),
                                       locations = cells_body(rows = 1)))
  
  current_seed_4 = reactive(standings() %>%
                              filter(owner != current_seed_1(),
                                     owner != current_seed_2(),
                                     owner != current_seed_3()) %>%
                              filter(t_wins >= standings() %>% filter(owner != current_seed_1(), owner != current_seed_2(), owner != current_seed_3()) %>% arrange(desc(t_wins)) %>% filter(row_number() == 1) %>% select(t_wins) %>% as.numeric() - 1) %>%
                              left_join(AllGames %>% filter(season == CS,
                                                            week <= 13,
                                                            team %in% (standings() %>% filter(owner != current_seed_1(), owner != current_seed_2(), owner != current_seed_3()) %>% arrange(desc(t_wins)) %>% filter(t_wins == standings() %>% arrange(desc(t_wins)) %>% filter(row_number() == 1) %>% select(t_wins) %>% as.numeric()) %>% pull(owner)),
                                                            opponent %in% (standings() %>% filter(owner != current_seed_1(), owner != current_seed_2(), owner != current_seed_3()) %>% arrange(desc(t_wins)) %>% filter(t_wins == standings() %>% arrange(desc(t_wins)) %>% filter(row_number() == 1) %>% select(t_wins) %>% as.numeric()) %>% pull(owner))) %>% 
                                          group_by(team) %>% 
                                          summarise(tb_h2h_wins = sum(result, na.rm=T)),
                                        by = c('owner'='team')) %>%
                              mutate(tb_h2h_wins = ifelse(is.na(tb_h2h_wins), 0, tb_h2h_wins)) %>%
                              left_join(AllGames %>% filter(season == CS,
                                                            week <= 13,
                                                            team %in% (standings() %>% filter(owner != current_seed_1(), owner != current_seed_2(), owner != current_seed_3()) %>% arrange(desc(t_wins)) %>% filter(t_wins == standings() %>% arrange(desc(t_wins)) %>% filter(row_number() == 1) %>% select(t_wins) %>% as.numeric()) %>% pull(owner)),
                                                            opponent %in% (standings() %>% filter(owner != current_seed_1(), owner != current_seed_2(), owner != current_seed_3()) %>% arrange(desc(t_wins)) %>% filter(t_wins == standings() %>% arrange(desc(t_wins)) %>% filter(row_number() == 1) %>% select(t_wins) %>% as.numeric()) %>% pull(owner))) %>% 
                                          group_by(team) %>% 
                                          summarise(tb_h2h_gp = sum(!is.na(score))),
                                        by = c('owner'='team')) %>%
                              mutate(tb_h2h_gp = ifelse(is.na(tb_h2h_gp), 0, as.numeric(tb_h2h_gp)),
                                     tb_h2h_losses = tb_h2h_gp - tb_h2h_wins,
                                     tb_h2h_wp = tb_h2h_wins/tb_h2h_gp,
                                     tb_h2h_record = paste0(tb_h2h_wins, "-", tb_h2h_losses)) %>%
                              mutate(tb_h2h_wp = ifelse(tb_h2h_gp==0, 1, tb_h2h_wp)) %>%
                              arrange(desc(t_wp), desc(t_wins), t_losses, desc(tb_h2h_wp), desc(t_pf)) %>%
                              filter(row_number() == 1) %>%
                              pull(owner))
  
  output$seed5 = render_gt(if(CW>=7){standings() %>%
                             left_join(AllGames %>% filter(season == CS,
                                                           week <= 13,
                                                           week >= 7) %>%
                                         mutate(score = ifelse(is.na(score), 0, score),
                                                result = ifelse(is.na(result), 0, result)) %>%
                                         group_by(team) %>% 
                                         summarise(hf_wins = sum(result, na.rm=T), hf_pf = sum(score, na.rm=T)),
                                       by = c('owner'='team')) %>%
                             filter(owner != current_seed_1(),
                                    owner != current_seed_2(),
                                    owner != current_seed_3(),
                                    owner != current_seed_4()) %>%
                             arrange(desc(hf_wins), desc(hf_pf)) %>%
                             filter(row_number() <= 3) %>%
                             select(logo, name, owner, hf_wins, hf_pf) %>%
                             gt() %>%
                             gt_img_circle(logo, height = 40) %>%
                             gt_merge_stack(name, owner) %>%
                             gt_theme_pff() %>%
                             cols_label(logo = "",
                                        name = "Team",
                                        hf_wins = "L7W Wins",
                                        hf_pf = "L7W PF") %>%
                             cols_width(logo ~ pct(14),
                                        name ~ pct(30),
                                        hf_wins ~ pct(18),
                                        hf_pf ~ pct(38)) %>%
                             cols_align(align = "center", columns = c(logo, hf_pf, hf_wins)) %>%
                             fmt_number(columns = c(hf_pf), decimals = 1) %>%
                             tab_header(title = html("<center><b>Hot Finish Wildcard Race</b></center>")) %>%
                             tab_options(heading.title.font.size = "18px",
                                         heading.background.color = "grey") %>%
                             tab_style(style = list(cell_fill(color = "gold")),
                                       locations = cells_body(rows = 1))}
                           else{data.frame(url = c("https://i.pinimg.com/originals/b7/21/34/b72134112b54864e4948865375ecbb11.gif")) %>%
                               gt() %>%
                               gt_img_rows(columns = url, height = 200) %>%
                               cols_width(url ~ pct(100)) %>%
                               gt_theme_pff() %>%
                               cols_label(url = "Starts in Week 7") %>%
                               cols_align(align = "center", columns = c(url)) %>%
                               tab_header(title = html("<center><b>Hot Finish Wildcard Race</b></center>")) %>%
                               tab_options(heading.title.font.size = "18px",
                                           heading.background.color = "grey")})
  
  current_seed_5 = reactive(if(CW>=7){
    standings() %>%
      left_join(AllGames %>% filter(season == CS,
                                    week <= 13,
                                    week >= 7) %>%
                  mutate(score = ifelse(is.na(score), 0, score),
                         result = ifelse(is.na(result), 0, result)) %>%
                  group_by(team) %>% 
                  summarise(hf_wins = sum(result, na.rm=T), hf_pf = sum(score, na.rm=T)),
                by = c('owner'='team')) %>%
      filter(owner != current_seed_1(),
             owner != current_seed_2(),
             owner != current_seed_3(),
             owner != current_seed_4()) %>%
      arrange(desc(hf_wins), desc(hf_pf)) %>%
      filter(row_number() == 1) %>%
      pull(owner)}
      else{""})
  
  output$seed6 = render_gt(standings() %>%
                             filter(owner != current_seed_1(),
                                    owner != current_seed_2(),
                                    owner != current_seed_3(),
                                    owner != current_seed_4(),
                                    owner != current_seed_5()) %>%
                             arrange(desc(t_pf)) %>%
                             filter(row_number() <= 3) %>%
                             select(logo, name, owner, t_pf) %>%
                             gt() %>%
                             gt_img_circle(logo, height = 40) %>%
                             gt_merge_stack(name, owner) %>%
                             gt_theme_pff() %>%
                             cols_label(logo = "",
                                        name = "Team",
                                        t_pf = "PF") %>%
                             cols_width(logo ~ pct(14),
                                        name ~ pct(30),
                                        t_pf ~ pct(56)) %>%
                             cols_align(align = "center", columns = c(logo, t_pf)) %>%
                             fmt_number(columns = c(t_pf), decimals = 1) %>%
                             tab_header(title = html("<center><b>Points Wildcard Race</b></center>")) %>%
                             tab_options(heading.title.font.size = "18px",
                                         heading.background.color = "grey") %>%
                             tab_style(style = list(cell_fill(color = "gold")),
                                       locations = cells_body(rows = 1)))
  
  current_seed_6 = reactive(standings() %>%
                              filter(owner != current_seed_1(),
                                     owner != current_seed_2(),
                                     owner != current_seed_3(),
                                     owner != current_seed_4(),
                                     owner != current_seed_5()) %>%
                              arrange(desc(t_pf)) %>%
                              filter(row_number() == 1) %>%
                              pull(owner))
  
  output$powerRankings = render_gt(sim_results %>%
                                     filter(wk == max(wk)) %>%
                                     select(rank, owner=team, wins, pf, playoffs, frb, champion, wk) %>%
                                     left_join(delta_sim_results, by = c('owner' = 'team')) %>%
                                     left_join(team %>% select(logo, name, owner, id)) %>%
                                     left_join(team_record(), by = 'id') %>%
                                     left_join(team_ov_record(), by = 'id') %>%
                                     left_join(standings() %>% select(owner, p_pf), by = 'owner') %>%
                                     mutate(ppg = p_pf / wk) %>%
                                     select(rank, delta_rank, logo, name, owner, proj_wins = wins, delta_proj_wins = delta_wins, 
                                            proj_pf = pf, delta_proj_pf = delta_pf, playoffs, delta_playoffs, frb, delta_frb, champion, delta_champion) %>%
                                     gt() %>% 
                                     cols_width(rank ~ pct(10),
                                                logo ~ pct(13),
                                                name ~ pct(17),
                                                proj_wins ~ pct(12),
                                                proj_pf ~ pct(12),
                                                playoffs ~ pct(12),
                                                frb ~ pct(12),
                                                champion ~ pct(12)) %>%
                                     gt_img_circle(column = logo, height = 50) %>%
                                     gt_merge_stack(col1 = name, col2 = owner) %>%
                                     gt_merge_stack_delta(col1 = rank, col2 = delta_rank, font_size = c('20px', '10px')) %>%
                                     gt_merge_stack_delta(col1 = proj_wins, col2 = delta_proj_wins, font_weight = c('normal', 'normal'), font_size = c('12px', '8px')) %>%
                                     gt_merge_stack_delta(col1 = proj_pf, col2 = delta_proj_pf, font_weight = c('normal', 'normal'), font_size = c('12px', '8px')) %>%
                                     gt_merge_stack_delta(col1 = playoffs, col2 = delta_playoffs, font_weight = c('normal', 'normal'), font_size = c('12px', '8px')) %>%
                                     gt_merge_stack_delta(col1 = frb, col2 = delta_frb, font_weight = c('normal', 'normal'), font_size = c('12px', '8px')) %>%
                                     gt_merge_stack_delta(col1 = champion, col2 = delta_champion, font_weight = c('normal', 'normal'), font_size = c('12px', '8px')) %>%
                                     cols_align(align = 'center', columns = c(rank, proj_wins, proj_pf, playoffs, frb, champion)) %>%
                                     gt_theme_538() %>% 
                                     cols_label(logo = "", 
                                                name = "Team", 
                                                proj_wins = "Proj. Wins",
                                                proj_pf = "Proj. PF",
                                                playoffs = "Playoff Birth",
                                                frb = "1st Round Bye",
                                                champion = "Champ") %>%
                                     fmt_number(columns = c(proj_wins), decimals = 1) %>%
                                     fmt_number(columns = c(proj_pf), decimals = 0) %>%
                                     fmt_number(columns = c(playoffs, frb, champion), scale_by = 100, decimals = 1) %>%
                                     tab_spanner(
                                       label = "Simulation Liklihood",
                                       columns = c(playoffs, frb, champion)) %>%
                                     tab_options(column_labels.font.size = 8) %>%
                                     tab_footnote(footnote = paste0("Power Rankings and simulations last updated after Week ", 
                                                                    sim_results %>% select(wk) %>% max())))
  
output$powerRankingPlot = renderPlot(sim_results %>%
                                       left_join(data.frame(team = sort(unique(sim_results$team)), 
                                                            col = sort(c('#8b4513', '#008000', '#4682b4', '#4b0082', '#ff0000', '#00ff00', '#00ffff', '#0000ff', '#ffff54', '#ff69b4', '#ffe4c4')))) %>%
                                       mutate(champion = champion*100,
                                              frb = frb*100,
                                              playoffs = playoffs *100) %>%
                                       rename("Rank"=rank,
                                              "Projected_Wins"=wins,
                                              "Projected_PF"=pf,
                                              "Playoff_Berth_Liklihood"=playoffs,
                                              "First_Round_Bye_Liklihood"=frb,
                                              "Championship_Liklihood"=champion) %>%
                                       arrange(wk, team) %>%
                                       filter(team %in% input$teams) %>%
                                       ggplot(mapping=aes(x = wk, y = get(input$yaxis))) + 
                                       geom_smooth(aes(color= I(col))) + 
                                       geom_image(data = sim_results %>% left_join(team %>% select(logo, owner), by = c("team"="owner")) %>% filter(wk == max(wk), team %in% input$teams) %>% mutate(champion = champion*100, frb = frb*100, playoffs = playoffs *100) %>% rename("Rank"=rank, "Projected_Wins"=wins, "Projected_PF"=pf, "Playoff_Berth_Liklihood"=playoffs, "First_Round_Bye_Liklihood"=frb, "Championship_Liklihood"=champion),
                                                  aes(image = logo), size = .05, by = "height") +
                                       ylab(gsub("_", " ", input$yaxis)) + xlab("Week") +
                                       xlim(0,13) +
                                       theme_stata() +
                                       theme(legend.position = "top",
                                             axis.title.y = element_text(size = 24, margin = margin(r=30)),
                                             axis.title.x = element_text(size = 20)) +
                                       scale_color_identity(guide = "legend", name = "", labels = sort(input$teams)) +
                                       if(input$yaxis == "Rank"){scale_y_reverse(lim = c(11,1), breaks = c(11,9,7,5,3,1))})

observe({
  if (input$selectall > 0) {
    if (input$selectall %% 2 != 0){
      updateF7Checkbox(session=session, 
                               inputId="teams",
                               value = team$owner[1:11])
      
    } else {
      updateF7Checkbox(session=session, 
                       inputId="teams",
                       value = "")
    }}})

output$sur_wk1 = render_gt(
  if(AllGames %>% filter(season==CS, !is.na(score)) %>% select(week) %>% unique() %>% max() == 0){
    schedule %>%
      filter(matchupPeriodId == 1, teamId != 12) %>%
      left_join(team %>% select(id, name, owner, logo), by = c('teamId'='id')) %>%
      select(logo, name, owner, totalPointsLive, totalProjectedPointsLive) %>%
      arrange(desc(totalPointsLive), desc(totalProjectedPointsLive)) %>%
      gt() %>%
      gt_merge_stack(col1 = name, col2 = owner) %>%
      gt_merge_stack(col1 = totalPointsLive, col2 = totalProjectedPointsLive) %>%
      gt_img_circle(column = logo, height = 50) %>%
      gt_theme_pff() %>%
      cols_label(totalPointsLive = 'PF',
                 logo = "") %>%
      cols_align(align = 'center', columns = c(totalPointsLive)) %>%
      cols_width(logo ~ pct(20), name ~ pct(50), totalPointsLive ~ pct(30)) %>%
      tab_header(title = html("<center><b>Wk 1</b></center>")) %>%
      tab_options(heading.title.font.size = "14px",
                  heading.background.color = "grey") %>%
      tab_style(style = list(cell_fill(color = "red")),
                locations = cells_body(rows = 11))} 
  else if(AllGames %>% filter(season==CS, !is.na(score)) %>% select(week) %>% unique() %>% max() > 0){
    AllGames %>%
      filter(season == CS, week == 1, team != "Ghost of Dakota Frantum") %>%
      left_join(team %>% select(name, owner, logo), by = c('team'='owner')) %>%
      select(logo, name, team, score) %>%
      arrange(desc(score)) %>%
      gt() %>%
      gt_merge_stack(col1 = name, col2 = team) %>%
      gt_img_circle(column = logo, height = 50) %>%
      gt_theme_pff() %>%
      cols_label(score = 'PF',
                 logo = "") %>%
      cols_align(align = 'center', columns = c(score)) %>%
      cols_width(logo ~ pct(20), name ~ pct(50), score ~ pct(30)) %>%
      tab_header(title = html("<center><b>Wk 1</b></center>")) %>%
      tab_options(heading.title.font.size = "14px",
                  heading.background.color = "grey") %>%
      tab_style(style = list(cell_fill(color = "red")),
                locations = cells_body(rows = 11))
  } 
  else{})

loser_sur_1 = reactive(
  if(AllGames %>% filter(season==CS, !is.na(score)) %>% select(week) %>% unique() %>% max() > 0){
    AllGames %>%
      filter(season == CS, week == 1, team != "Ghost of Dakota Frantum") %>%
      arrange(desc(score)) %>%
      filter(row_number() == 11) %>%
      pull(team)}
  else{""})

output$sur_wk2 = render_gt(
  if(AllGames %>% filter(season==CS, !is.na(score)) %>% select(week) %>% unique() %>% max() == 1){
    schedule %>%
      filter(matchupPeriodId == 2, teamId != 12) %>%
      left_join(team %>% select(id, name, owner, logo), by = c('teamId'='id')) %>%
      select(logo, name, owner, totalPointsLive, totalProjectedPointsLive) %>%
      filter(owner != loser_sur_1()) %>%
      arrange(desc(totalPointsLive), desc(totalProjectedPointsLive)) %>%
      gt() %>%
      gt_merge_stack(col1 = name, col2 = owner) %>%
      gt_merge_stack(col1 = totalPointsLive, col2 = totalProjectedPointsLive) %>%
      gt_img_circle(column = logo, height = 50) %>%
      gt_theme_pff() %>%
      cols_label(totalPointsLive = 'PF',
                 logo = "") %>%
      cols_align(align = 'center', columns = c(totalPointsLive)) %>%
      cols_width(logo ~ pct(20), name ~ pct(50), totalPointsLive ~ pct(30)) %>%
      tab_header(title = html("<center><b>Wk 2</b></center>")) %>%
      tab_options(heading.title.font.size = "14px",
                  heading.background.color = "grey") %>%
      tab_style(style = list(cell_fill(color = "red")),
                locations = cells_body(rows = 10))} 
  else if(AllGames %>% filter(season==CS, !is.na(score)) %>% select(week) %>% unique() %>% max() > 1){
    AllGames %>%
      filter(season == CS, week == 2, team != "Ghost of Dakota Frantum") %>%
      left_join(team %>% select(name, owner, logo), by = c('team'='owner')) %>%
      select(logo, name, team, score) %>%
      filter(team != loser_sur_1()) %>%
      arrange(desc(score)) %>%
      gt() %>%
      gt_merge_stack(col1 = name, col2 = team) %>%
      gt_img_circle(column = logo, height = 50) %>%
      gt_theme_pff() %>%
      cols_label(score = 'PF',
                 logo = "") %>%
      cols_align(align = 'center', columns = c(score)) %>%
      cols_width(logo ~ pct(20), name ~ pct(50), score ~ pct(30)) %>%
      tab_header(title = html("<center><b>Wk 2</b></center>")) %>%
      tab_options(heading.title.font.size = "14px",
                  heading.background.color = "grey") %>%
      tab_style(style = list(cell_fill(color = "red")),
                locations = cells_body(rows = 10))
  } 
  else{})

loser_sur_2 = reactive(
  if(AllGames %>% filter(season==CS, !is.na(score)) %>% select(week) %>% unique() %>% max() > 1){
    AllGames %>%
      filter(season == CS, week == 2, team != "Ghost of Dakota Frantum") %>%
      filter(team != loser_sur_1()) %>%
      arrange(desc(score)) %>%
      filter(row_number() == 10) %>%
      pull(team)}
  else{""})

output$sur_wk3 = render_gt(
  if(AllGames %>% filter(season==CS, !is.na(score)) %>% select(week) %>% unique() %>% max() == 2){
    schedule %>%
      filter(matchupPeriodId == 3, teamId != 12) %>%
      left_join(team %>% select(id, name, owner, logo), by = c('teamId'='id')) %>%
      select(logo, name, owner, totalPointsLive, totalProjectedPointsLive) %>%
      filter(owner != loser_sur_1() & owner != loser_sur_2()) %>%
      arrange(desc(totalPointsLive), desc(totalProjectedPointsLive)) %>%
      gt() %>%
      gt_merge_stack(col1 = name, col2 = owner) %>%
      gt_merge_stack(col1 = totalPointsLive, col2 = totalProjectedPointsLive) %>%
      gt_img_circle(column = logo, height = 50) %>%
      gt_theme_pff() %>%
      cols_label(totalPointsLive = 'PF',
                 logo = "") %>%
      cols_align(align = 'center', columns = c(totalPointsLive)) %>%
      cols_width(logo ~ pct(20), name ~ pct(50), totalPointsLive ~ pct(30)) %>%
      tab_header(title = html("<center><b>Wk 3</b></center>")) %>%
      tab_options(heading.title.font.size = "14px",
                  heading.background.color = "grey") %>%
      tab_style(style = list(cell_fill(color = "red")),
                locations = cells_body(rows = 9))} 
  else if(AllGames %>% filter(season==CS, !is.na(score)) %>% select(week) %>% unique() %>% max() > 2){
    AllGames %>%
      filter(season == CS, week == 3, team != "Ghost of Dakota Frantum") %>%
      left_join(team %>% select(name, owner, logo), by = c('team'='owner')) %>%
      select(logo, name, team, score) %>%
      filter(team != loser_sur_1() & team != loser_sur_2()) %>%
      arrange(desc(score)) %>%
      gt() %>%
      gt_merge_stack(col1 = name, col2 = team) %>%
      gt_img_circle(column = logo, height = 50) %>%
      gt_theme_pff() %>%
      cols_label(score = 'PF',
                 logo = "") %>%
      cols_align(align = 'center', columns = c(score)) %>%
      cols_width(logo ~ pct(20), name ~ pct(50), score ~ pct(30)) %>%
      tab_header(title = html("<center><b>Wk 3</b></center>")) %>%
      tab_options(heading.title.font.size = "14px",
                  heading.background.color = "grey") %>%
      tab_style(style = list(cell_fill(color = "red")),
                locations = cells_body(rows = 9))
  } 
  else{})

loser_sur_3 = reactive(
  if(AllGames %>% filter(season==CS, !is.na(score)) %>% select(week) %>% unique() %>% max() > 2){
    AllGames %>%
      filter(season == CS, week == 3, team != "Ghost of Dakota Frantum") %>%
      filter(team != loser_sur_1() & team != loser_sur_2()) %>%
      arrange(desc(score)) %>%
      filter(row_number() == 9) %>%
      pull(team)}
  else{""})

output$sur_wk4 = render_gt(
  if(AllGames %>% filter(season==CS, !is.na(score)) %>% select(week) %>% unique() %>% max() == 3){
    schedule %>%
      filter(matchupPeriodId == 4, teamId != 12) %>%
      left_join(team %>% select(id, name, owner, logo), by = c('teamId'='id')) %>%
      select(logo, name, owner, totalPointsLive, totalProjectedPointsLive) %>%
      filter(owner != loser_sur_1() & owner != loser_sur_2() & owner != loser_sur_3()) %>%
      arrange(desc(totalPointsLive), desc(totalProjectedPointsLive)) %>%
      gt() %>%
      gt_merge_stack(col1 = name, col2 = owner) %>%
      gt_merge_stack(col1 = totalPointsLive, col2 = totalProjectedPointsLive) %>%
      gt_img_circle(column = logo, height = 50) %>%
      gt_theme_pff() %>%
      cols_label(totalPointsLive = 'PF',
                 logo = "") %>%
      cols_align(align = 'center', columns = c(totalPointsLive)) %>%
      cols_width(logo ~ pct(20), name ~ pct(50), totalPointsLive ~ pct(30)) %>%
      tab_header(title = html("<center><b>Wk 4</b></center>")) %>%
      tab_options(heading.title.font.size = "14px",
                  heading.background.color = "grey") %>%
      tab_style(style = list(cell_fill(color = "red")),
                locations = cells_body(rows = 8))} 
  else if(AllGames %>% filter(season==CS, !is.na(score)) %>% select(week) %>% unique() %>% max() > 3){
    AllGames %>%
      filter(season == CS, week == 4, team != "Ghost of Dakota Frantum") %>%
      left_join(team %>% select(name, owner, logo), by = c('team'='owner')) %>%
      select(logo, name, team, score) %>%
      filter(team != loser_sur_1() & team != loser_sur_2() & team != loser_sur_3()) %>%
      arrange(desc(score)) %>%
      gt() %>%
      gt_merge_stack(col1 = name, col2 = team) %>%
      gt_img_circle(column = logo, height = 50) %>%
      gt_theme_pff() %>%
      cols_label(score = 'PF',
                 logo = "") %>%
      cols_align(align = 'center', columns = c(score)) %>%
      cols_width(logo ~ pct(20), name ~ pct(50), score ~ pct(30)) %>%
      tab_header(title = html("<center><b>Wk 4</b></center>")) %>%
      tab_options(heading.title.font.size = "14px",
                  heading.background.color = "grey") %>%
      tab_style(style = list(cell_fill(color = "red")),
                locations = cells_body(rows = 8))
  } 
  else{})

loser_sur_4 = reactive(
  if(AllGames %>% filter(season==CS, !is.na(score)) %>% select(week) %>% unique() %>% max() > 3){
    AllGames %>%
      filter(season == CS, week == 4, team != "Ghost of Dakota Frantum") %>%
      filter(team != loser_sur_1() & team != loser_sur_2() & team != loser_sur_3()) %>%
      arrange(desc(score)) %>%
      filter(row_number() == 8) %>%
      pull(team)}
  else{""})

output$sur_wk5 = render_gt(
  if(AllGames %>% filter(season==CS, !is.na(score)) %>% select(week) %>% unique() %>% max() == 4){
    schedule %>%
      filter(matchupPeriodId == 5, teamId != 12) %>%
      left_join(team %>% select(id, name, owner, logo), by = c('teamId'='id')) %>%
      select(logo, name, owner, totalPointsLive, totalProjectedPointsLive) %>%
      filter(owner != loser_sur_1() & owner != loser_sur_2() & owner != loser_sur_3() & owner != loser_sur_4()) %>%
      arrange(desc(totalPointsLive), desc(totalProjectedPointsLive)) %>%
      gt() %>%
      gt_merge_stack(col1 = name, col2 = owner) %>%
      gt_merge_stack(col1 = totalPointsLive, col2 = totalProjectedPointsLive) %>%
      gt_img_circle(column = logo, height = 50) %>%
      gt_theme_pff() %>%
      cols_label(totalPointsLive = 'PF',
                 logo = "") %>%
      cols_align(align = 'center', columns = c(totalPointsLive)) %>%
      cols_width(logo ~ pct(20), name ~ pct(50), totalPointsLive ~ pct(30)) %>%
      tab_header(title = html("<center><b>Wk 5</b></center>")) %>%
      tab_options(heading.title.font.size = "14px",
                  heading.background.color = "grey") %>%
      tab_style(style = list(cell_fill(color = "red")),
                locations = cells_body(rows = 7))} 
  else if(AllGames %>% filter(season==CS, !is.na(score)) %>% select(week) %>% unique() %>% max() > 4){
    AllGames %>%
      filter(season == CS, week == 5, team != "Ghost of Dakota Frantum") %>%
      left_join(team %>% select(name, owner, logo), by = c('team'='owner')) %>%
      select(logo, name, team, score) %>%
      filter(team != loser_sur_1() & team != loser_sur_2() & team != loser_sur_3() & team != loser_sur_4()) %>%
      arrange(desc(score)) %>%
      gt() %>%
      gt_merge_stack(col1 = name, col2 = team) %>%
      gt_img_circle(column = logo, height = 50) %>%
      gt_theme_pff() %>%
      cols_label(score = 'PF',
                 logo = "") %>%
      cols_align(align = 'center', columns = c(score)) %>%
      cols_width(logo ~ pct(20), name ~ pct(50), score~ pct(30)) %>%
      tab_header(title = html("<center><b>Wk 5</b></center>")) %>%
      tab_options(heading.title.font.size = "14px",
                  heading.background.color = "grey") %>%
      tab_style(style = list(cell_fill(color = "red")),
                locations = cells_body(rows = 7))
  } 
  else{})

loser_sur_5 = reactive(
  if(AllGames %>% filter(season==CS, !is.na(score)) %>% select(week) %>% unique() %>% max() > 4){
    AllGames %>%
      filter(season == CS, week == 5, team != "Ghost of Dakota Frantum") %>%
      filter(team != loser_sur_1() & team != loser_sur_2() & team != loser_sur_3() & team != loser_sur_4()) %>%
      arrange(desc(score)) %>%
      filter(row_number() == 7) %>%
      pull(team)}
  else{""})

output$sur_wk6 = render_gt(
  if(AllGames %>% filter(season==CS, !is.na(score)) %>% select(week) %>% unique() %>% max() == 5){
    schedule %>%
      filter(matchupPeriodId == 6, teamId != 12) %>%
      left_join(team %>% select(id, name, owner, logo), by = c('teamId'='id')) %>%
      select(logo, name, owner, totalPointsLive, totalProjectedPointsLive) %>%
      filter(owner != loser_sur_1() & owner != loser_sur_2() & owner != loser_sur_3() & owner != loser_sur_4() &
               owner != loser_sur_5()) %>%
      arrange(desc(totalPointsLive), desc(totalProjectedPointsLive)) %>%
      gt() %>%
      gt_merge_stack(col1 = name, col2 = owner) %>%
      gt_merge_stack(col1 = totalPointsLive, col2 = totalProjectedPointsLive) %>%
      gt_img_circle(column = logo, height = 50) %>%
      gt_theme_pff() %>%
      cols_label(totalPointsLive = 'PF',
                 logo = "") %>%
      cols_align(align = 'center', columns = c(totalPointsLive)) %>%
      cols_width(logo ~ pct(20), name ~ pct(50), totalPointsLive ~ pct(30)) %>%
      tab_header(title = html("<center><b>Wk 6</b></center>")) %>%
      tab_options(heading.title.font.size = "14px",
                  heading.background.color = "grey") %>%
      tab_style(style = list(cell_fill(color = "red")),
                locations = cells_body(rows = 6))} 
  else if(AllGames %>% filter(season==CS, !is.na(score)) %>% select(week) %>% unique() %>% max() > 5){
    AllGames %>%
      filter(season == CS, week == 6, team != "Ghost of Dakota Frantum") %>%
      left_join(team %>% select(name, owner, logo), by = c('team'='owner')) %>%
      select(logo, name, team, score) %>%
      filter(team != loser_sur_1() & team != loser_sur_2() & team != loser_sur_3() & team != loser_sur_4() &
               team != loser_sur_5()) %>%
      arrange(desc(score)) %>%
      gt() %>%
      gt_merge_stack(col1 = name, col2 = team) %>%
      gt_img_circle(column = logo, height = 50) %>%
      gt_theme_pff() %>%
      cols_label(score = 'PF',
                 logo = "") %>%
      cols_align(align = 'center', columns = c(score)) %>%
      cols_width(logo ~ pct(20), name ~ pct(50), score~ pct(30)) %>%
      tab_header(title = html("<center><b>Wk 6</b></center>")) %>%
      tab_options(heading.title.font.size = "14px",
                  heading.background.color = "grey") %>%
      tab_style(style = list(cell_fill(color = "red")),
                locations = cells_body(rows = 6))
  } 
  else{})

loser_sur_6 = reactive(
  if(AllGames %>% filter(season==CS, !is.na(score)) %>% select(week) %>% unique() %>% max() > 5){
    AllGames %>%
      filter(season == CS, week == 6, team != "Ghost of Dakota Frantum") %>%
      filter(team != loser_sur_1() & team != loser_sur_2() & team != loser_sur_3() & team != loser_sur_4() &
               team != loser_sur_5()) %>%
      arrange(desc(score)) %>%
      filter(row_number() == 6) %>%
      pull(team)}
  else{""})

output$sur_wk7 = render_gt(
  if(AllGames %>% filter(season==CS, !is.na(score)) %>% select(week) %>% unique() %>% max() == 6){
    schedule %>%
      filter(matchupPeriodId == 7, teamId != 12) %>%
      left_join(team %>% select(id, name, owner, logo), by = c('teamId'='id')) %>%
      select(logo, name, owner, totalPointsLive, totalProjectedPointsLive) %>%
      filter(owner != loser_sur_1() & owner != loser_sur_2() & owner != loser_sur_3() & owner != loser_sur_4() &
               owner != loser_sur_5() & owner != loser_sur_6()) %>%
      arrange(desc(totalPointsLive), desc(totalProjectedPointsLive)) %>%
      gt() %>%
      gt_merge_stack(col1 = name, col2 = owner) %>%
      gt_merge_stack(col1 = totalPointsLive, col2 = totalProjectedPointsLive) %>%
      gt_img_circle(column = logo, height = 50) %>%
      gt_theme_pff() %>%
      cols_label(totalPointsLive = 'PF',
                 logo = "") %>%
      cols_align(align = 'center', columns = c(totalPointsLive)) %>%
      cols_width(logo ~ pct(20), name ~ pct(50), totalPointsLive ~ pct(30)) %>%
      tab_header(title = html("<center><b>Wk 7</b></center>")) %>%
      tab_options(heading.title.font.size = "14px",
                  heading.background.color = "grey") %>%
      tab_style(style = list(cell_fill(color = "red")),
                locations = cells_body(rows = 5))} 
  else if(AllGames %>% filter(season==CS, !is.na(score)) %>% select(week) %>% unique() %>% max() > 6){
    AllGames %>%
      filter(season == CS, week == 7, team != "Ghost of Dakota Frantum") %>%
      left_join(team %>% select(name, owner, logo), by = c('team'='owner')) %>%
      select(logo, name, team, score) %>%
      filter(team != loser_sur_1() & team != loser_sur_2() & team != loser_sur_3() & team != loser_sur_4() &
               team != loser_sur_5() & team != loser_sur_6()) %>%
      arrange(desc(score)) %>%
      gt() %>%
      gt_merge_stack(col1 = name, col2 = team) %>%
      gt_img_circle(column = logo, height = 50) %>%
      gt_theme_pff() %>%
      cols_label(score = 'PF',
                 logo = "") %>%
      cols_align(align = 'center', columns = c(score)) %>%
      cols_width(logo ~ pct(20), name ~ pct(50), score~ pct(30)) %>%
      tab_header(title = html("<center><b>Wk 7</b></center>")) %>%
      tab_options(heading.title.font.size = "14px",
                  heading.background.color = "grey") %>%
      tab_style(style = list(cell_fill(color = "red")),
                locations = cells_body(rows = 5))
  } 
  else{})

loser_sur_7 = reactive(
  if(AllGames %>% filter(season==CS, !is.na(score)) %>% select(week) %>% unique() %>% max() > 6){
    AllGames %>%
      filter(season == CS, week == 7, team != "Ghost of Dakota Frantum") %>%
      filter(team != loser_sur_1() & team != loser_sur_2() & team != loser_sur_3() & team != loser_sur_4() &
               team != loser_sur_5() & team != loser_sur_6()) %>%
      arrange(desc(score)) %>%
      filter(row_number() == 5) %>%
      pull(team)}
  else{""})

output$sur_wk8 = render_gt(
  if(AllGames %>% filter(season==CS, !is.na(score)) %>% select(week) %>% unique() %>% max() == 7){
    schedule %>%
      filter(matchupPeriodId == 8, teamId != 12) %>%
      left_join(team %>% select(id, name, owner, logo), by = c('teamId'='id')) %>%
      select(logo, name, owner, totalPointsLive, totalProjectedPointsLive) %>%
      filter(owner != loser_sur_1() & owner != loser_sur_2() & owner != loser_sur_3() & owner != loser_sur_4() &
               owner != loser_sur_5() & owner != loser_sur_6() & owner != loser_sur_7()) %>%
      arrange(desc(totalPointsLive), desc(totalProjectedPointsLive)) %>%
      gt() %>%
      gt_merge_stack(col1 = name, col2 = owner) %>%
      gt_merge_stack(col1 = totalPointsLive, col2 = totalProjectedPointsLive) %>%
      gt_img_circle(column = logo, height = 50) %>%
      gt_theme_pff() %>%
      cols_label(totalPointsLive = 'PF',
                 logo = "") %>%
      cols_align(align = 'center', columns = c(totalPointsLive)) %>%
      cols_width(logo ~ pct(20), name ~ pct(50), totalPointsLive ~ pct(30)) %>%
      tab_header(title = html("<center><b>Wk 8</b></center>")) %>%
      tab_options(heading.title.font.size = "14px",
                  heading.background.color = "grey") %>%
      tab_style(style = list(cell_fill(color = "red")),
                locations = cells_body(rows = 4))} 
  else if(AllGames %>% filter(season==CS, !is.na(score)) %>% select(week) %>% unique() %>% max() > 7){
    AllGames %>%
      filter(season == CS, week == 8, team != "Ghost of Dakota Frantum") %>%
      left_join(team %>% select(name, owner, logo), by = c('team'='owner')) %>%
      select(logo, name, team, score) %>%
      filter(team != loser_sur_1() & team != loser_sur_2() & team != loser_sur_3() & team != loser_sur_4() &
               team != loser_sur_5() & team != loser_sur_6() & team != loser_sur_7()) %>%
      arrange(desc(score)) %>%
      gt() %>%
      gt_merge_stack(col1 = name, col2 = team) %>%
      gt_img_circle(column = logo, height = 50) %>%
      gt_theme_pff() %>%
      cols_label(score = 'PF',
                 logo = "") %>%
      cols_align(align = 'center', columns = c(score)) %>%
      cols_width(logo ~ pct(20), name ~ pct(50), score~ pct(30)) %>%
      tab_header(title = html("<center><b>Wk 8</b></center>")) %>%
      tab_options(heading.title.font.size = "14px",
                  heading.background.color = "grey") %>%
      tab_style(style = list(cell_fill(color = "red")),
                locations = cells_body(rows = 4))
  } 
  else{})

loser_sur_8 = reactive(
  if(AllGames %>% filter(season==CS, !is.na(score)) %>% select(week) %>% unique() %>% max() > 7){
    AllGames %>%
      filter(season == CS, week == 8, team != "Ghost of Dakota Frantum") %>%
      filter(team != loser_sur_1() & team != loser_sur_2() & team != loser_sur_3() & team != loser_sur_4() &
               team != loser_sur_5() & team != loser_sur_6() & team != loser_sur_7()) %>%
      arrange(desc(score)) %>%
      filter(row_number() == 4) %>%
      pull(team)}
  else{""})

output$sur_wk9 = render_gt(
  if(AllGames %>% filter(season==CS, !is.na(score)) %>% select(week) %>% unique() %>% max() == 8){
    schedule %>%
      filter(matchupPeriodId == 9, teamId != 12) %>%
      left_join(team %>% select(id, name, owner, logo), by = c('teamId'='id')) %>%
      select(logo, name, owner, totalPointsLive, totalProjectedPointsLive) %>%
      filter(owner != loser_sur_1() & owner != loser_sur_2() & owner != loser_sur_3() & owner != loser_sur_4() &
               owner != loser_sur_5() & owner != loser_sur_6() & owner != loser_sur_7() & owner != loser_sur_8()) %>%
      arrange(desc(totalPointsLive), desc(totalProjectedPointsLive)) %>%
      gt() %>%
      gt_merge_stack(col1 = name, col2 = owner) %>%
      gt_merge_stack(col1 = totalPointsLive, col2 = totalProjectedPointsLive) %>%
      gt_img_circle(column = logo, height = 50) %>%
      gt_theme_pff() %>%
      cols_label(totalPointsLive = 'PF',
                 logo = "") %>%
      cols_align(align = 'center', columns = c(totalPointsLive)) %>%
      cols_width(logo ~ pct(20), name ~ pct(50), totalPointsLive ~ pct(30)) %>%
      tab_header(title = html("<center><b>Wk 9</b></center>")) %>%
      tab_options(heading.title.font.size = "14px",
                  heading.background.color = "grey") %>%
      tab_style(style = list(cell_fill(color = "red")),
                locations = cells_body(rows = 3))} 
  else if(AllGames %>% filter(season==CS, !is.na(score)) %>% select(week) %>% unique() %>% max() > 8){
    AllGames %>%
      filter(season == CS, week == 9, team != "Ghost of Dakota Frantum") %>%
      left_join(team %>% select(name, owner, logo), by = c('team'='owner')) %>%
      select(logo, name, team, score) %>%
      filter(team != loser_sur_1() & team != loser_sur_2() & team != loser_sur_3() & team != loser_sur_4() &
               team != loser_sur_5() & team != loser_sur_6() & team != loser_sur_7() & team != loser_sur_8()) %>%
      arrange(desc(score)) %>%
      gt() %>%
      gt_merge_stack(col1 = name, col2 = team) %>%
      gt_img_circle(column = logo, height = 50) %>%
      gt_theme_pff() %>%
      cols_label(score = 'PF',
                 logo = "") %>%
      cols_align(align = 'center', columns = c(score)) %>%
      cols_width(logo ~ pct(20), name ~ pct(50), score~ pct(30)) %>%
      tab_header(title = html("<center><b>Wk 9</b></center>")) %>%
      tab_options(heading.title.font.size = "14px",
                  heading.background.color = "grey") %>%
      tab_style(style = list(cell_fill(color = "red")),
                locations = cells_body(rows = 3))
  } 
  else{})

loser_sur_9 = reactive(
  if(AllGames %>% filter(season==CS, !is.na(score)) %>% select(week) %>% unique() %>% max() > 8){
    AllGames %>%
      filter(season == CS, week == 9, team != "Ghost of Dakota Frantum") %>%
      filter(team != loser_sur_1() & team != loser_sur_2() & team != loser_sur_3() & team != loser_sur_4() &
               team != loser_sur_5() & team != loser_sur_6() & team != loser_sur_7() & team != loser_sur_8()) %>%
      arrange(desc(score)) %>%
      filter(row_number() == 3) %>%
      pull(team)}
  else{""})

output$sur_wk10 = render_gt(
  if(AllGames %>% filter(season==CS, !is.na(score)) %>% select(week) %>% unique() %>% max() == 9){
    schedule %>%
      filter(matchupPeriodId ==10, teamId != 12) %>%
      left_join(team %>% select(id, name, owner, logo), by = c('teamId'='id')) %>%
      select(logo, name, owner, totalPointsLive, totalProjectedPointsLive) %>%
      filter(owner != loser_sur_1() & owner != loser_sur_2() & owner != loser_sur_3() & owner != loser_sur_4() &
               owner != loser_sur_5() & owner != loser_sur_6() & owner != loser_sur_7() & owner != loser_sur_8() & owner != loser_sur_9()) %>%
      arrange(desc(totalPointsLive), desc(totalProjectedPointsLive)) %>%
      gt() %>%
      gt_merge_stack(col1 = name, col2 = owner) %>%
      gt_merge_stack(col1 = totalPointsLive, col2 = totalProjectedPointsLive) %>%
      gt_img_circle(column = logo, height = 50) %>%
      gt_theme_pff() %>%
      cols_label(totalPointsLive = 'PF',
                 logo = "") %>%
      cols_align(align = 'center', columns = c(totalPointsLive)) %>%
      cols_width(logo ~ pct(20), name ~ pct(50), totalPointsLive ~ pct(30)) %>%
      tab_header(title = html("<center><b>Wk 10</b></center>")) %>%
      tab_options(heading.title.font.size = "14px",
                  heading.background.color = "grey") %>%
      tab_style(style = list(cell_fill(color = "red")),
                locations = cells_body(rows = 2)) %>%
      tab_style(style = list(cell_fill(color = "gold")),
                locations = cells_body(rows = 1))} 
  else if(AllGames %>% filter(season==CS, !is.na(score)) %>% select(week) %>% unique() %>% max() > 9){
    AllGames %>%
      filter(season == CS, week == 10, team != "Ghost of Dakota Frantum") %>%
      left_join(team %>% select(name, owner, logo), by = c('team'='owner')) %>%
      select(logo, name, team, score) %>%
      filter(team != loser_sur_1() & team != loser_sur_2() & team != loser_sur_3() & team != loser_sur_4() &
               team != loser_sur_5() & team != loser_sur_6() & team != loser_sur_7() & team != loser_sur_8() & team != loser_sur_9()) %>%
      arrange(desc(score)) %>%
      gt() %>%
      gt_merge_stack(col1 = name, col2 = team) %>%
      gt_img_circle(column = logo, height = 50) %>%
      gt_theme_pff() %>%
      cols_label(score = 'PF',
                 logo = "") %>%
      cols_align(align = 'center', columns = c(score)) %>%
      cols_width(logo ~ pct(20), name ~ pct(50), score~ pct(30)) %>%
      tab_header(title = html("<center><b>Wk 10</b></center>")) %>%
      tab_options(heading.title.font.size = "14px",
                  heading.background.color = "grey") %>%
      tab_style(style = list(cell_fill(color = "red")),
                locations = cells_body(rows = 2)) %>%
      tab_style(style = list(cell_fill(color = "gold")),
                locations = cells_body(rows = 1))
  } 
  else{})

}

shinyApp(ui, server)
