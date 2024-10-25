library(pacman)
p_load(tidyverse, gt, gtExtras, gtUtils, fflr, scales)

gt_merge_stack_delta <- function(gt_object, col1, col2, ..., small_cap = TRUE,
                                 font_size = c("16px", "8px"), font_weight = c("bold", "bold")) {
  
  
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
        <div style='line-height:{font_size[2]}'><span style ='font-weight:{font_weight[2]};color:{case_when(grepl('+', data_in, fixed=T) ~ '#1b7837', grepl('-', data_in, fixed=T) ~ 'darkred')};font-size:{font_size[2]}'>{data_in}</span></div>"
        )
      }
    ) %>%
    cols_hide(columns = {{ col2 }})
}

gt_merge_stack_with_wl_pills <- function(gt_object, col1, col3, palette = "black", 
                                      ..., small_cap = TRUE, font_size = "18px", font_weight = "bold", 
                                      width=25, color1="#013369", color2="#D50A0A", color3="gray",
                                      height_plot = 7, max_wins = 13,
                                      fig_dim=c(height_plot, width), same_limit = TRUE, type = "default", label = TRUE) {
  
  colors <- scales::col2hcl(palette, ...)
  col1_bare <- rlang::enexpr(col1) %>% rlang::as_string()
  row_name_var <- gt_object[["_boxhead"]][["var"]][which(gt_object[["_boxhead"]][["type"]] == "stub")]
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
  
  gt_object %>% text_transform(locations = 
                                 if (isTRUE(row_name_var == col1_bare)) {cells_stub(rows = gt::everything())
                                   
                                 } else {cells_body(columns = {{col1}})}, 
                               fn = function(x) {
                                 if (small_cap) {font_variant <- "small-caps"
                                 } else {font_variant <- "normal"}
                                 if(FALSE){}
                                 else{glue::glue("<div style='line-height:{font_size}; text-align:center; padding-top:4px'><div style='font-weight:{font_weight};font-variant:{font_variant};color:{colors};font-size:{font_size}'>{x}</div></div>
                                            <div style='line-height:20px; text-align:center;'><span style='background-image: {lapply(list_vals,plot_fn_pill)}</span></div>")}
                                 
                               }) %>% 
    cols_hide(columns = c({{col3}}))}

gt_merge_stack_with_density <- function(gt_object, col1, col4, palette = "black",  width = 25, bw = NULL,
                                      ..., small_cap = TRUE, font_size = "18px", font_weight = "bold", 
                                      line_color = "black", fill_color = "grey",
                                      height_plot = 7, palette2=c("black", "black", "purple", "green", "lightgrey"),
                                      fig_dim=c(height_plot, width), same_limit = TRUE, trim = FALSE, type = "density", label = TRUE) {
  
  colors <- scales::col2hcl(palette, ...)
  col1_bare <- rlang::enexpr(col1) %>% rlang::as_string()
  row_name_var <- gt_object[["_boxhead"]][["var"]][which(gt_object[["_boxhead"]][["type"]] == "stub")]
  
  col_bare <- dplyr::select(gt_object[["_data"]], {{col4}}) %>% names()
  
  list_data_in <- gt_index(gt_object, col_bare, as_vector = TRUE)
  
  data_in <- unlist(list_data_in)
  
  total_rng <- grDevices::extendrange(data_in, r = range(data_in, na.rm = TRUE), f = 0.02)
  
  plot_fn_spark <- function(trim, list_data_in, type_in) {
    
    if (all(list_data_in %in% c(NA, NULL))) {
      return("<div></div>")
    }
    
    vals <- as.double(stats::na.omit(list_data_in))
    
    max_val <- max(vals, na.rm = TRUE)
    min_val <- min(vals, na.rm = TRUE)
    
    x_max <- vals[vals == max_val]
    x_min <- vals[vals == min_val]
    
    input_data <- dplyr::tibble(
      x = 1:length(vals),
      y = vals
    )
    
    # respect type column or value
    type = type_in
    
    if (type == "boxplot") {
      plot_base <- ggplot(input_data) +
        theme_void()
      
      if (isTRUE(same_limit)) {
        plot_base <- plot_base +
          scale_x_continuous(expand = expansion(mult = 0.05)) +
          coord_cartesian(
            clip = "off",
            xlim = grDevices::extendrange(total_rng, f = c(0, 0.01)),
            ylim = c(0.9, 1.15)
          )
      } else {
        plot_base <- plot_base +
          scale_x_continuous(expand = expansion(mult = 0.05)) +
          coord_cartesian(
            clip = "off",
            xlim = grDevices::extendrange(vals, f = 0.09),
            ylim = c(0.9, 1.15)
          )
      }
      
      plot_out <- plot_base +
        geom_boxplot(
          aes(x = .data$y, y = 1),
          width = 0.15,
          color = line_color,
          fill = fill_color,
          outlier.size = 0.3,
          linewidth = 0.3
        )
    } else if (type == "rug_strip") {
      plot_base <- ggplot(input_data) +
        theme_void()
      
      if (isTRUE(same_limit)) {
        plot_base <- plot_base +
          scale_x_continuous(expand = expansion(mult = 0.05)) +
          coord_cartesian(
            clip = "off",
            xlim = grDevices::extendrange(total_rng, f = 0.09),
            ylim = c(0.75, 1.15)
          )
      } else {
        plot_base <- plot_base +
          scale_x_continuous(expand = expansion(mult = 0.05)) +
          coord_cartesian(
            clip = "off",
            xlim = grDevices::extendrange(vals, f = 0.09),
            ylim = c(0.75, 1.15)
          )
      }
      
      plot_out <- plot_base +
        geom_point(
          aes(x = .data$y, y = 1),
          alpha = 0.2,
          size = 0.3,
          color = line_color,
          position = position_jitter(height = 0.15, seed = 37)
        ) +
        geom_rug(
          aes(x = .data$y),
          length = unit(0.2, "npc"),
          alpha = 0.5,
          linewidth = 0.2
        )
    } else if (type == "histogram") {
      plot_base <- ggplot(input_data) +
        theme_void()
      
      if (isTRUE(same_limit)) {
        if (is.null(bw)) {
          bw <- bw_calc(data_in)
          
        } else {
          bw <- bw
        }
        
        plot_out <- plot_base +
          {
            if(bw > 0){
              geom_histogram(
                aes(x = .data$y), 
                color = line_color, fill = fill_color, binwidth = bw,
                linewidth = 0.2
              )
            } else if(bw == 0) {
              bw <- 1
              
              geom_histogram(
                aes(x = .data$y), 
                color = line_color, fill = fill_color, binwidth = bw,
                linewidth = 0.2
              )
            } else {
              
              hist_breaks <- graphics::hist(data_in[!is.na(data_in)], breaks = "FD", plot=FALSE)$breaks
              
              geom_histogram(
                aes(x = .data$y), 
                color = line_color, fill = fill_color, breaks = hist_breaks,
                linewidth = 0.2
              )
            }
          } +
          scale_x_continuous(expand = expansion(mult = 0.1)) +
          coord_cartesian(
            clip = "off",
            xlim = grDevices::extendrange(
              data_in,
              r = range(data_in, na.rm = TRUE),
              f = 0.02
            )
          )
      } else {
        if (is.null(bw)) {
          bw <- 2 * stats::IQR(vals, na.rm = TRUE) / length(vals)^(1 / 3)
        } else {
          bw <- bw
        }
        
        plot_out <- plot_base +
          geom_histogram(
            aes(x = .data$y),
            color = line_color,
            fill = fill_color,
            binwidth = bw
          ) +
          coord_cartesian(
            clip = "off",
            xlim = grDevices::extendrange(
              vals,
              r = range(vals, na.rm = TRUE),
              f = 0.02
            )
          )
      }
    } else if (type == "density") {
      if (isTRUE(same_limit)) {
        if (is.null(bw)) {
          bw <- stats::bw.nrd0(stats::na.omit(as.vector(data_in)))
        } else {
          bw <- bw
        }
        
        total_rng_dens <- stats::density(
          as.vector(
            stats::na.omit(data_in)
          ),
          bw = bw
        )[["x"]]
        
        density_calc <- stats::density(input_data[["y"]], bw = bw)
        density_range <- density_calc[["x"]]
        
        density_df <- dplyr::tibble(
          x = density_calc[["x"]],
          y = density_calc[["y"]]
        )
        
        if (trim) { # implementation of filtering values
          # only to actual and slightly outside the range
          filter_range <- range(vals, na.rm = TRUE) %>%
            scales::expand_range(mul = 0.05)
          
          density_df <- dplyr::filter(
            density_df,
            dplyr::between(.data$x, filter_range[1], filter_range[2])
          )
        }
        
        plot_base <- ggplot(density_df) +
          theme_void()
        
        
        plot_out <- plot_base +
          geom_area(
            aes(x = .data$x, y = .data$y),
            color = line_color,
            fill = fill_color,
            linewidth = .5
          ) +
          geom_vline(
            xintercept = mean(vals),
            color = "purple",
            linewidth = .25
          ) +
          xlim(range(density_range)) +
          coord_cartesian(
            xlim = range(total_rng_dens, na.rm = TRUE),
            expand = TRUE,
            clip = "off"
          )
      } else {
        if (is.null(bw)) {
          bw <- stats::bw.nrd0(stats::na.omit(as.vector(data_in)))
        } else {
          bw <- bw
        }
        
        total_rng_dens <- stats::density(stats::na.omit(as.vector(vals)), bw = bw)[["x"]]
        
        density_calc <- stats::density(input_data[["y"]], bw = bw)
        density_range <- density_calc[["x"]]
        
        density_df <- dplyr::tibble(
          x = density_calc[["x"]],
          y = density_calc[["y"]]
        )
        
        if (trim) { # implementation of filtering values
          # only to actual and slightly outside the range
          filter_range <- range(vals, na.rm = TRUE) %>%
            scales::expand_range(mul = 0.05)
          
          density_df <- dplyr::filter(
            density_df,
            dplyr::between(.data$x, filter_range[1], filter_range[2])
          )
        }
        
        plot_base <- ggplot(density_df) +
          theme_void()
        
        plot_out <- plot_base +
          geom_area(
            aes(x = .data$x, y = .data$y),
            color = line_color,
            fill = fill_color
          ) +
          xlim(range(density_range, na.rm = TRUE)) +
          coord_cartesian(
            xlim = range(total_rng_dens, na.rm = TRUE),
            expand = TRUE,
            clip = "off"
          )
      }
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
                                 if(FALSE){}
                                 else{glue::glue("<div style='line-height:20px; text-align:center'><span style='background-image: {lapply(list_data_in, function(ld) plot_fn_spark(FALSE, ld, type))}</span></div>
                                 <div style='line-height:{font_size}; text-align:center; padding-top:1px'><div style='font-weight:{font_weight};font-variant:{font_variant};color:{colors};font-size:{font_size}'>{x}</div></div>")}
                                 
                               }) %>% 
    cols_hide(columns = c({{col4}}))}

gt_plt_bar_stack_4 <- function(gt_object, column = NULL, palette = c("#e6e6e6", "darkred", "#1b7837", "#e6e6e6"),
                             labels = c("", "Group 2", "Group 3", ""),
                             position = "fill",
                             width = 70,
                             fmt_fn = scales::label_number(scale_cut = cut_short_scale(), trim = TRUE),
                             font = "mono") {
  stopifnot("Table must be of class 'gt_tbl'" = "gt_tbl" %in% class(gt_object))
  stopifnot("There must be 2 or 3 labels" = (length(labels) %in% c(2:4)))
  stopifnot("There must be 2 or 3 colors in the palette" = (length(palette) %in% c(2:4)))
  stopifnot("`position` must be one of 'stack' or 'fill'" = (position %in% c("stack", "fill")))
  
  .dt_boxhead_key <- "_boxhead"
  dt_boxhead_get <- function(data) {
    dt__get(data, .dt_boxhead_key)
  }
  dt_boxhead_set <- function(data, boxh) {
    dt__set(data, .dt_boxhead_key, boxh)
  }
  dt_boxhead_init <- function(data) {
    
    # Get the column names from the data table
    vars <- colnames(dt_data_get(data = data))
    
    # If there are any 'labeled' columns in the data table, use those labels
    # wherever possible; otherwise, use column names as the column labels
    if (any_labeled_columns_in_data_tbl(data = data)) {
      column_labels <- get_columns_labels_from_attrs(data = data)
    } else {
      column_labels <- vars
    }
    
    empty_list <- lapply(seq_along(vars), function(x) NULL)
    
    boxh_df <-
      dplyr::tibble(
        # Matches to the name of the `data` column
        var = vars,
        # The mode of the column in the rendered table
        # - `default` appears as a column with values below
        # - `stub` appears as part of a table stub, set to the left
        #   and styled differently
        # - `row_group` uses categorical values and groups rows
        #   under row group headings
        # - `hidden` hides this column from the final table render
        #   but retains values to use in expressions
        # - `hidden_at_px` similar to hidden but takes a list of
        #   screen widths (in px) whereby the column would be hidden
        type = "default",
        # The presentation label, which is a list of labels by
        # render context (e.g., HTML, LaTeX, etc.)
        column_label = as.list(column_labels),
        # Units for the column label, written in a shorthand notation
        column_units = NA_character_,
        # A pattern to use when arranging the `column_label` and the
        # `column_units`
        column_pattern = NA_character_,
        # The alignment of the column ("left", "right", "center")
        column_align = "center",
        # The width of the column in `px`
        column_width = empty_list,
        # The widths at which the column disappears from view (this is
        # HTML specific)
        hidden_px = empty_list
      )
    
    dt_boxhead_set(boxh = boxh_df, data = data)
  }
  dt_boxhead_edit <- function(data, var, ...) {
    
    dt_boxhead <- dt_boxhead_get(data = data)
    
    var_name <- var
    
    val_list <- list(...)
    
    if (length(val_list) != 1) {
      cli::cli_abort("{.fn dt_boxhead_edit} expects a single value at `{cli::symbol$ellipsis}`.")
    }
    
    check_names_dt_boxhead_expr(expr = val_list)
    
    check_vars_dt_boxhead(var = var, dt_boxhead = dt_boxhead)
    
    if (is.list(dt_boxhead[[names(val_list)]])) {
      dt_boxhead[[which(dt_boxhead$var == var_name), names(val_list)]] <- unname(val_list)
    } else {
      dt_boxhead[[which(dt_boxhead$var == var_name), names(val_list)]] <- unlist(val_list)
    }
    
    dt_boxhead_set(data = data, boxh = dt_boxhead)
  }
  dt_boxhead_add_var <- function(
    data,
    var,
    type,
    column_label = list(var),
    column_units = NA_character_,
    column_pattern = NA_character_,
    column_align = "left",
    column_width = list(NULL),
    hidden_px = list(NULL),
    add_where = "top"
  ) {
    
    dt_boxhead <- dt_boxhead_get(data = data)
    
    dt_boxhead_row <-
      dplyr::tibble(
        var = var,
        type = type,
        column_label = column_label,
        column_units = column_units,
        column_pattern = column_pattern,
        column_align = column_align,
        column_width = column_width,
        hidden_px = hidden_px
      )
    add_where <- rlang::arg_match0(add_where, c("top", "bottom"))
    
    if (add_where == "top") {
      dt_boxhead <- dplyr::bind_rows(dt_boxhead_row, dt_boxhead)
    } else if (add_where == "bottom") {
      dt_boxhead <- dplyr::bind_rows(dt_boxhead, dt_boxhead_row)
    }
    
    dt_boxhead_set(data = data, boxh = dt_boxhead)
  }
  dt_boxhead_set_hidden <- function(data, vars) {
    
    dt_boxhead <- dt_boxhead_get(data = data)
    
    dt_boxhead[which(dt_boxhead$var %in% vars & dt_boxhead$type != "stub"), "type"] <- "hidden"
    
    dt_boxhead_set(data = data, boxh = dt_boxhead)
  }
  dt_boxhead_set_not_hidden <- function(data, vars) {
    
    dt_boxhead <- dt_boxhead_get(data = data)
    
    dt_boxhead[which(dt_boxhead$var %in% vars & dt_boxhead$type != "stub"), "type"] <- "default"
    
    dt_boxhead_set(data = data, boxh = dt_boxhead)
  }
  dt_boxhead_set_stub <- function(data, var) {
    
    dt_boxhead <- dt_boxhead_get(data = data)
    
    dt_boxhead[which(dt_boxhead$var == var), "type"] <- "stub"
    dt_boxhead[which(dt_boxhead$var == var), "column_align"] <- "left"
    
    dt_boxhead_set(data = data, boxh = dt_boxhead)
  }
  dt_boxhead_set_row_group <- function(data, vars) {
    
    dt_boxhead <- dt_boxhead_get(data = data)
    
    dt_boxhead[which(dt_boxhead$var %in% vars), "type"] <- "row_group"
    dt_boxhead[which(dt_boxhead$var %in% vars), "column_align"] <- "left"
    
    dt_boxhead_set(data = data, boxh = dt_boxhead)
  }
  dt_boxhead_edit_column_label <- function(data, var, column_label) {
    
    dt_boxhead_edit(
      data = data,
      var = var,
      column_label = column_label
    )
  }
  dt_boxhead_edit_column_units <- function(data, var, column_units) {
    
    dt_boxhead_edit(
      data = data,
      var = var,
      column_units = column_units
    )
  }
  dt_boxhead_edit_column_pattern <- function(data, var, column_pattern) {
    
    dt_boxhead_edit(
      data = data,
      var = var,
      column_pattern = column_pattern
    )
  }
  dt_boxhead_get_vars <- function(data) {
    dt_boxhead_get(data = data)$var
  }
  dt_boxhead_get_vars_default <- function(data) {
    df <- dt_boxhead_get(data = data)
    df$var[df$type == "default"]
  }
  dt_boxhead_get_var_stub <- function(data) {
    
    res <- dt_boxhead_get_var_by_type(data = data, type = "stub")
    # FIXME: don't return NA_character_ here, just return res or NULL
    if (length(res) == 0) {
      NA_character_
    } else {
      res
    }
  }
  dt_boxhead_get_vars_groups <- function(data) {
    
    res <- dt_boxhead_get_var_by_type(data = data, type = "row_group")
    # FIXME: don't return NA_character_ here, just return res or NULL
    if (length(res) == 0) {
      NA_character_
    } else {
      res
    }
  }
  dt_boxhead_get_alignments_in_stub <- function(data) {
    
    stub_layout <- get_stub_layout(data = data)
    alignments <- NULL
    
    if ("group_label" %in% stub_layout) {
      grp_vars <- dt_boxhead_get_vars_groups(data = data)
      # non-initialized grp_vars
      grp_alignment <-
        dt_boxhead_get_alignment_by_var(
          data = data,
          var = grp_vars
        )
      
      alignments <- c(alignments, grp_alignment)
    }
    
    if ("rowname" %in% stub_layout) {
      row_alignment <- dt_boxhead_get_alignment_by_var(
        data = data,
        dt_boxhead_get_var_stub(data = data)
      )
      alignments <- c(alignments, row_alignment)
    }
    alignments
  }
  dt_boxhead_get_var_by_type <- function(data, type) {
    boxhead <- dt_boxhead_get(data = data)
    boxhead$var[boxhead$type == type]
  }
  dt_boxhead_get_vars_labels_default <- function(data) {
    boxhead <- dt_boxhead_get(data = data)
    unlist(boxhead$column_label[boxhead$type == "default"])
  }
  dt_boxhead_get_vars_align_default <- function(data) {
    boxhead <- dt_boxhead_get(data = data)
    boxhead$column_align[boxhead$type == "default"]
  }
  dt_boxhead_get_alignment_by_var <- function(data, var) {
    boxhead <- dt_boxhead_get(data = data)
    boxhead$column_align[boxhead$var == var]
  }
  check_names_dt_boxhead_expr <- function(expr) {
    
    if (!all(names(expr) %in% c(
      "type", "column_label", "column_units", "column_pattern",
      "column_align", "column_width", "hidden_px"
    ))) {
      cli::cli_abort("Expressions must use names available in `dt_boxhead`.")
    }
  }
  check_vars_dt_boxhead <- function(var, dt_boxhead) {
    
    if (!(var %in% dt_boxhead$var)) {
      cli::cli_abort("The `var` value must be value in `dt_boxhead$var`.")
    }
  }
  dt_boxhead_build <- function(data, context) {
    
    boxh <- dt_boxhead_get(data = data)
    
    boxh$column_label <-
      lapply(boxh$column_label, function(label) process_text(label, context))
    
    # Merge column units into column labels
    if (!all(is.na(boxh$column_units))) {
      
      for (i in seq_along(boxh$column_label)) {
        
        if (is.na(boxh[["column_units"]][i])) next
        
        column_label <- unlist(boxh[["column_label"]][i])
        
        units <- boxh[["column_units"]][i]
        column_pattern <- boxh[["column_pattern"]][i]
        
        units_built <-
          render_units(
            define_units(units_notation = units),
            context = context
          )
        
        # rstudio/gt#1733
        if (
          !is.na(column_pattern) &&
          column_pattern == "" &&
          grepl(units, column_label, fixed = TRUE)
        ) {
          
          # With `column_pattern` equal to `""`, we can surmise that this was
          # set automatically by `cols_label()`; the mechanism now is to replace
          # the units text in the label with the 'built' units text
          
          column_label <- gsub(units, units_built, column_label, fixed = TRUE)
          
        } else {
          
          if (is.na(column_pattern)) {
            
            # Obtain the default `column_pattern` (which that is settable in the
            # `column_labels.units_pattern` option of `tab_options()`
            column_pattern <-
              dt_options_get_value(
                data = data,
                option = "column_labels_units_pattern"
              )
          }
          
          column_pattern <- gsub("{1}", column_label, column_pattern, fixed = TRUE)
          column_pattern <- gsub("{2}", units_built, column_pattern, fixed = TRUE)
          column_label <- column_pattern
        }
        
        boxh$column_label[i] <- list(column_label)
      }
    }
    
    dt_boxhead_set(data = data, boxh = boxh)
  }
  dt_boxhead_set_var_order <- function(data, vars) {
    
    boxh <- dt_boxhead_get(data = data)
    
    if (
      length(vars) != nrow(boxh) ||
      length(unique(vars)) != nrow(boxh) ||
      !all(vars %in% boxh$var)
    ) {
      cli::cli_abort("The length of `vars` must equal the row count of `_boxh`.")
    }
    
    order_vars <- vapply(vars, function(x) {which(boxh$var == x)}, numeric(1))
    
    boxh <- boxh[order_vars, ]
    
    dt_boxhead_set(data = data, boxh = boxh)
  }
  dt__get <- function(data, key) {
    data[[key, exact = TRUE]]
  }
  dt__set <- function(data, key, value) {
    data[[key]] <- value
    data
  }
  
  
  var_sym <- rlang::enquo(column)
  var_bare <- rlang::as_label(var_sym)
  
  all_vals <- gt_index(gt_object, {{ column }}) %>%
    lapply(X = ., FUN = sum, na.rm = TRUE) %>%
    unlist()
  
  if (length(all_vals) == 0) {
    return(gt_object)
  }
  
  total_rng <- max(all_vals, na.rm = TRUE)
  
  tab_out <- text_transform(
    gt_object,
    locations = cells_body({{ column }}),
    fn = function(x) {
      bar_fx <- function(x_val) {
        if (x_val %in% c("NA", "NULL")) {
          return("<div></div>")
        }
        
        col_pal <- palette
        
        vals <- strsplit(x_val, split = ", ") %>%
          unlist() %>%
          as.double()
        
        n_val <- length(vals)
        
        col_fill <- c(1:4)
        
        df_in <- dplyr::tibble(
          x = vals,
          y = rep(1, n_val),
          fill = col_pal[col_fill]
        )
        
        plot_out <- df_in %>%
          ggplot(
            aes(
              x = .data$x,
              y = factor(.data$y),
              fill = I(.data$fill),
              group = .data$y
            )
          ) +
          geom_col(position = position, color = "#e6e6e6", width = 1) +
          scale_x_continuous(
            expand = if (position == "stack") {
              expansion(mult = c(0, 0.1))
            } else {
              c(0, 0)
            },
            limits = if (position == "stack") {
              c(0, total_rng)
            } else {
              NULL
            }
          ) +
          scale_y_discrete(expand = c(0, 0)) +
          coord_cartesian(clip = "off") +
          theme_void() +
          theme(legend.position = "none", plot.margin = margin(0, 0, 0, 0, "pt"))
        
        out_name <- file.path(tempfile(
          pattern = "file",
          tmpdir = tempdir(),
          fileext = ".svg"
        ))
        
        ggsave(
          out_name,
          plot = plot_out,
          dpi = 25.4,
          height = 5,
          width = width,
          units = "mm",
          device = "svg"
        )
        
        img_plot <- readLines(out_name) %>%
          paste0(collapse = "") %>%
          gt::html()
        
        on.exit(file.remove(out_name), add = TRUE)
        
        img_plot
      }
      
      tab_built <- lapply(X = x, FUN = bar_fx)
    }
  )
  
  label_built <- if (FALSE) {
    } else {
    lab_pal1 <- palette[1]
    lab_pal2 <- palette[2]
    lab_pal3 <- palette[3]
    lab_pal4 <- palette[4]
    
    lab1 <- labels[1]
    lab2 <- labels[2]
    lab3 <- labels[3]
    lab4 <- labels[4]
    
    glue::glue(
      "<span>{lab2}</span>"
    ) %>% gt::html()
  }
  
  
  
  # Get the columns supplied in `columns` as a character vector
  tab_out <-
    dt_boxhead_edit_column_label(
      data = tab_out,
      var = var_bare,
      column_label = label_built
    )
  suppressWarnings(tab_out)
}

setwd("~/Fantasy/WOFFL")

# Import Season Info
szn = 2024

AllGames <- read.csv("WOFFL_stats_portal/AllGames.csv")

SeasonGames <- AllGames |>
  filter(
    season == szn,
    !is.na(score),
    team != "Ghost of Dakota Frantum"
  ) |>
  arrange(
    score, team
  ) |>
  mutate(
    ov_wins = row_number() - 1,
    ov_losses = 10 - ov_wins,
    .by = ov_wk
  ) |>
  arrange(ov_wk)

week = SeasonGames %>% 
  filter(
    !is.na(score)
  ) %>% 
  select(week) %>% 
  unique() %>% 
  nrow() %>% 
  as.numeric()

# Import Simulation Results
sim_res <- read.csv("WOFFL_stats_portal/sim_results/sim_results_2024.csv")

cur_wk_sim_res <- sim_res |>
  filter(
    wk == week
  ) |>
  arrange(
    desc(champion), desc(fifth)
  ) |>
  mutate(
    rank = row_number() 
  ) |>
  arrange(
    team
  )

last_wk_sim_res <- sim_res |>
  filter(
    wk == week - 1
  ) |>
  arrange(
    desc(champion), desc(fifth)
  ) |>
  mutate(
    rank = row_number() 
  ) |>
  arrange(
    team
  )

# Calculate over-the-week change in sim results
delta_sim_res <- cbind(
    cur_wk_sim_res[, 1], 
    cur_wk_sim_res[,-1] - last_wk_sim_res[,-1]
  ) |>
  rename(
    team = 1
  ) |>
  select(-wk) |>
  rename_with(
    ~ paste0("delta_", .x)
  ) |>
  mutate(
    delta_rank = delta_rank * -1
  )

# Next week simulation preds
nxt_wk_sim <- sim_res |>
  filter(
    wk == week
  ) |>
  select(
    team, win_nxt_game, win_po, lose_po, win_frb, lose_frb
  ) |>
  left_join(
    AllGames |>
      filter(
        season == szn,
        week == .env$week + 1
      ) |>
      select(
        team, opponent
      ),
    by = "team"
  ) |>
  mutate(
    opponent = paste0("vs ", stringr::word(opponent, 1))
  )

# Find and combine columns that we want in our final table
SG_stats <- SeasonGames |>
  filter(
    !is.na(score)
  ) |>
  mutate(
    win = ifelse(score > opponent_score, 1, 0),
    loss = ifelse(score < opponent_score, 1, 0)
  ) |>
  summarise(
    wins = sum(win),
    losses = sum(loss),
    PPG = mean(score),
    scores = list(score),
    ov_rec = paste0(sum(ov_wins), "-", sum(ov_losses)),
    results = list(win),
    .by = team
  ) |>
  rowwise() |>
  mutate(
    results = list(c(results, rep(.5, 13-week)))
  )

espn_teams <- fflr::league_teams(
    leagueId = "313259"
  ) |>
  mutate(team = case_when(
      teamId == 1 ~ "Jeremy Patak",
      teamId == 2 ~ "Austin Iske",
      teamId == 3 ~ "Brody Morgan",
      teamId == 4 ~ "Dax Davis",
      teamId == 5 ~ "Landry Sheridan",
      teamId == 6 ~ "Stone Palmer",
      teamId == 7 ~ "Seth Lassiter",
      teamId == 8 ~ "Nick McFarland",
      teamId == 9 ~ "Nike Simmons",
      teamId == 10 ~ "Daniel Potichko",
      teamId == 11 ~ "Cade Palmer",
      .default = "Error"
    )
  ) |>
  select(
    team, name, abbrev, logo
  )

final <- SG_stats |>
  left_join(
    cur_wk_sim_res |>
      select(rank, team, wins_proj = wins, PF, 
             playoffs, frb, champion, fifth, eleventh),
    by = "team"
  ) |>
  left_join(
    delta_sim_res |>
      select(delta_team, delta_wins_proj = delta_wins, delta_PF, delta_playoffs, 
             delta_frb, delta_champion, delta_fifth, delta_eleventh, delta_rank),
    by = c("team" = "delta_team")
  ) |>
  left_join(
    espn_teams,
    by = "team"
  ) |>
  left_join(
    nxt_wk_sim,
    by = "team"
  ) |>
  mutate(
    wins_proj = round(wins_proj, 1),
    PPG = round(PPG, 1),
    po_curr = playoffs,
    playoffs = paste0(
      round(playoffs*100, 0),
      "%"
    ),
    delta_playoffs = ifelse(
      delta_playoffs > 0,
      paste0("+", round(delta_playoffs*100, 1)),
      round(delta_playoffs*100, 1) |> as.character()
    ),
    frb = paste0(
      round(frb*100, 0),
      "%"
    ),
    delta_frb = ifelse(
      delta_frb > 0,
      paste0("+", round(delta_frb*100, 1)),
      round(delta_frb*100, 1) |> as.character()
    ),
    win_nxt_game = paste0(round(win_nxt_game*100, 1), "%")
  ) |>
  arrange(
    rank
  )

# Create ggplots
make_ggplot_po <- function(x){
  return(
  final |>
    arrange(rank) |>
    filter(
      rank == x
    ) |>
    ggplot(
      aes(
        y = factor(team),
        group = team
      )
    ) +
    geom_col(aes(x = 1), position = "stack", color = "#e6e6e6", fill = "#e6e6e6", width = 1) +
    geom_col(aes(x = win_po), position = "stack", color = "#1b7837", fill = "#1b7837", width = 1) +
    geom_col(aes(x = po_curr), position = "stack", color = "darkred", fill = "darkred", width = 1) +
    geom_col(aes(x = lose_po), position = "stack", color = "#e6e6e6", fill = "#e6e6e6", width = 1) +
    scale_x_continuous(
      expand = expansion(mult = c(0, 0)),
      limits = c(-.4, 1.4)
    ) +
    scale_y_discrete(expand = c(0, 0)) +
    coord_cartesian(clip = "off") +
    theme_void() +
    theme(legend.position = "none", plot.margin = margin(0, 0, 0, 0, "pt")) +
    annotate(
      "text", 
      x = 1 + .2, 
      y = final$team[[x]], 
      label = paste0(round(final$win_po[[x]]*100 - final$po_curr[[x]]*100, 0), "%"),
      size = 55,
      fontface = 2,
      color = "#1b7837"
    ) +
    annotate(
      "text", 
      x = 0 - .2, 
      y = final$team[[x]], 
      label = paste0(round(final$po_curr[[x]]*100 - final$lose_po[[x]]*100, 0), "%"),
      size = 55,
      fontface = 2, 
      color = "darkred"
    )
  )
}

rk1gg <- make_ggplot_po(1)
rk2gg <- make_ggplot_po(2)
rk3gg <- make_ggplot_po(3)
rk4gg <- make_ggplot_po(4)
rk5gg <- make_ggplot_po(5)
rk6gg <- make_ggplot_po(6)
rk7gg <- make_ggplot_po(7)
rk8gg <- make_ggplot_po(8)
rk9gg <- make_ggplot_po(9)
rk10gg <- make_ggplot_po(10)
rk11gg <- make_ggplot_po(11)

# Format Table
tab <- final |>
  arrange(
    desc(champion), desc(fifth)
  ) |>
  mutate(
    po_odds_change = NA
  ) |>
  select(
    rank, delta_rank, logo, name, team, 
    PPG, results, scores, ov_rec,
    champion, fifth, eleventh,
    playoffs, delta_playoffs, frb, delta_frb,
    win_nxt_game, opponent, po_odds_change
  ) |>
  gt() |>
  tab_style(
    style = list(cell_text(weight = "bold")),
    locations = cells_body(
      columns = rank,
    )
  ) |>
  gt_fa_rank_change(
    delta_rank, 
    palette = c("#1b7837", "#ffffff", "darkred"),
    show_text = FALSE, 
    fa_type = "caret"
  ) |>
  gt_img_rows(logo, height = 50) |>
  gt_merge_stack(name, team) |>
  gt_merge_stack_with_density(
    PPG, scores,
    width = 15,
    font_size = "15px"
  ) |>
  gt_merge_stack_with_wl_pills(
    ov_rec, results,
    max_wins = 13, width = 15) |>
  tab_spanner(
    "Performance",
    columns = c(PPG, ov_rec)
  ) |>
  gt_color_pills(
    champion,
    digits = 1,
    format_type = "percent"
  ) |>
  gt_color_pills(
    fifth,
    digits = 1,
    format_type = "percent"
  ) |>
  gt_color_pills(
    eleventh,
    digits = 1,
    format_type = "percent"
  ) |>
  tab_spanner(
    "Liklihood of Finishing:",
    columns = c(champion, fifth, eleventh)
  ) |>
  gt_merge_stack_delta(playoffs, delta_playoffs) |>
  gt_merge_stack_delta(frb, delta_frb) |>
  tab_spanner(
    "Probability of Earning:",
    columns = c(playoffs, frb)
  ) |>
  gt_merge_stack(win_nxt_game, opponent) |>
  text_transform(
    locations = cells_body(columns = po_odds_change, rows = rank==1),
    fn = function(x){
      rk1gg |>
        ggplot_image(height = px(22), aspect_ratio = 5)
    }
  ) |>
  text_transform(
    locations = cells_body(columns = po_odds_change, rows = rank==2),
    fn = function(x){
      rk2gg |>
        ggplot_image(height = px(22), aspect_ratio = 5)
    }
  ) |>
  text_transform(
    locations = cells_body(columns = po_odds_change, rows = rank==3),
    fn = function(x){
      rk3gg |>
        ggplot_image(height = px(22), aspect_ratio = 5)
    }
  ) |>
  text_transform(
    locations = cells_body(columns = po_odds_change, rows = rank==4),
    fn = function(x){
      rk4gg |>
        ggplot_image(height = px(22), aspect_ratio = 5)
    }
  ) |>
  text_transform(
    locations = cells_body(columns = po_odds_change, rows = rank==5),
    fn = function(x){
      rk5gg |>
        ggplot_image(height = px(22), aspect_ratio = 5)
    }
  ) |>
  text_transform(
    locations = cells_body(columns = po_odds_change, rows = rank==6),
    fn = function(x){
      rk6gg |>
        ggplot_image(height = px(22), aspect_ratio = 5)
    }
  ) |>
  text_transform(
    locations = cells_body(columns = po_odds_change, rows = rank==7),
    fn = function(x){
      rk7gg |>
        ggplot_image(height = px(22), aspect_ratio = 5)
    }
  ) |>
  text_transform(
    locations = cells_body(columns = po_odds_change, rows = rank==8),
    fn = function(x){
      rk8gg |>
        ggplot_image(height = px(22), aspect_ratio = 5)
    }
  ) |>
  text_transform(
    locations = cells_body(columns = po_odds_change, rows = rank==9),
    fn = function(x){
      rk9gg |>
        ggplot_image(height = px(22), aspect_ratio = 5)
    }
  ) |>
  text_transform(
    locations = cells_body(columns = po_odds_change, rows = rank==10),
    fn = function(x){
      rk10gg |>
        ggplot_image(height = px(22), aspect_ratio = 5)
    }
  ) |>
  text_transform(
    locations = cells_body(columns = po_odds_change, rows = rank==11),
    fn = function(x){
      rk11gg |>
        ggplot_image(height = px(22), aspect_ratio = 5)
    }
  ) |>
  tab_spanner(
    paste0("Week ", week+1, " Impact"),
    columns = c(win_nxt_game, po_odds_change)
  ) |>
  gt_theme_pl() |>
  tab_style(
    style = list(cell_fill(color = "#f5f5f5")),
    locations = cells_body(
      columns = c(PPG, ov_rec, playoffs, frb)
    )
  ) |>
  tab_style(
    style = cell_text(size = px(11)),
    locations = cells_column_labels(c(ov_rec, playoffs, po_odds_change))
  ) |>
  tab_style(
    style = cell_text(size = px(9)),
    locations = cells_column_labels(c(fifth, win_nxt_game))
  ) |>
  tab_style(
    style = cell_text(align = "center"),
    locations = cells_column_labels(
      c(PPG, ov_rec, champion, fifth, eleventh, playoffs, frb, win_nxt_game)
    )
  ) |>
  cols_align(
    align = c("center"),
    columns = c(logo, playoffs, frb, win_nxt_game, po_odds_change)
  ) |>
  cols_width(
    rank ~ px(20),
    delta_rank ~ px(20),
    logo ~ px(50),
    name ~ px(115),
    PPG ~ px(80),
    ov_rec ~ px(85),
    champion ~ px(70),
    fifth ~ px(70),
    eleventh ~ px(70),
    playoffs ~ px(50),
    frb ~ px(50),
    win_nxt_game ~ px(60),
    po_odds_change ~ px(115)
  ) |>
  cols_label(
    rank ~ "",
    delta_rank ~ "",
    logo ~ "",
    name ~ "Team",
    scores ~ "PPG",
    ov_rec ~ '"Overall" Wins',
    champion ~ "Champ",
    fifth ~ "Consolidation Champ",
    eleventh ~ "Last",
    playoffs ~ "Playoff Spot",
    frb ~ "BYE",
    win_nxt_game ~ "Win Probability",
    po_odds_change ~ "PO Odds"
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
    subtitle = paste0("Week ", week, " Power Rankings"),
    title = md("<div><img src='https://i.ibb.co/fSTLLVQ/BlackPNG.png' style='height:100px;'></div>")
  ) |>
  tab_options(
    heading.align = "center",
    heading.subtitle.font.size = px(30)
  )


setwd("~/Fantasy/2024/power_rankings")

gtsave(
    tab,
    paste0("pr_wk", week, ".png"),
    "",
    expand = 30
  )

setwd("~/Fantasy/WOFFL")
