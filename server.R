# @author Scott Dobbins
# @version 0.9.9.3
# @date 2017-11-07 19:30


# ### initialize plotly ###
# 
# Sys.setenv("plotly_username"="sdobbins")
# Sys.setenv("plotly_api_key"="ElZwoGYrCyhDGcauIpUQ")


### Constants ---------------------------------------------------------------

change_token <- "change_token"


### Server Component --------------------------------------------------------

shinyServer(function(input, output, session) {
  
### Session variables -------------------------------------------------------

  previous_wars_selection <- c()
  previous_dropdown_selection <- rep(list("All"), length(dropdown_tags))
  re_name(previous_dropdown_selection, dropdown_tags)
  
  overview_proxy <- leafletProxy("overview_map")
  civilian_proxy <- leafletProxy("civilian_map")
  overview_proxy_fast <- leafletProxy("overview_map", deferUntilFlush = FALSE)
  civilian_proxy_fast <- leafletProxy("civilian_map", deferUntilFlush = FALSE)
  
  previous_start_date <- earliest_date
  previous_end_date <- latest_date
  animation_end_date <- latest_date
  

### War Selections ----------------------------------------------------------

  selected <- function(war_label) {
    reactive(war_label %c% input$which_war)
  }
  war_selected <- lapply(war_labels, selected)
  
  selection <- function(war_tag) {
    reactive({
      if (all(length(input$dateRange) == 2L, 
              length(input$regions) >= 1L, 
              length(input$targets) >= 1L, 
              length(input$countries) >= 1L, 
              length(input$aircrafts) >= 1L, 
              length(input$weapons) >= 1L)) {
        filter_selection(war_tag    = war_tag, 
                         start_date = input$dateRange[[1]], 
                         end_date   = input$dateRange[[2]], 
                         regions    = input$regions, 
                         targets    = input$targets, 
                         countries  = input$countries, 
                         aircrafts  = input$aircrafts, 
                         weapons    = input$weapons)
      } else {
        war_data[[war_tag]]
      }
    })
  }
  war_selection <- lapply(war_tags, selection)
  
  sample_war <- function(war_tag) {
    reactive({
      if ((war_missions_reactive[[war_tag]])() < input$sample_num) {
        (war_selection[[war_tag]])()
      } else {
        war_dt <- (war_selection[[war_tag]])()
        war_dt[sample(seq_len(nrow(war_dt)), size = input$sample_num, replace = FALSE)]
      }
    })
  }
  war_sample <- lapply(war_tags, sample_war)
  
  walk(list(war_selected, 
            war_selection, 
            war_sample), 
       ~re_name(., war_tags))
  

### InfoBox Reactives -------------------------------------------------------
  
  missions_reactive <- function(war_tag) {
    reactive({
      if ((war_selected[[war_tag]])()) {
        (war_selection[[war_tag]])()[, .N]
      } else 0
    })
  }
  war_missions_reactive <- lapply(war_tags, missions_reactive)
  
  flights_reactive <- function(war_tag) {
    reactive({
      if((war_selected[[war_tag]])()) {
        (war_selection[[war_tag]])()[, sum(Aircraft_Attacking_Num,  na.rm = TRUE)]
      } else 0
    })
  }
  war_flights_reactive <- lapply(war_tags, flights_reactive)

  bombs_reactive <- function(war_tag) {
    reactive({
      if((war_selected[[war_tag]])()) {
        (war_selection[[war_tag]])()[, sum(Weapon_Expended_Num,  na.rm = TRUE)]
      } else 0
    })
  }
  war_bombs_reactive <- lapply(war_tags, bombs_reactive)
  
  weight_reactive <- function(war_tag) {
    reactive({
      if((war_selected[[war_tag]])()) {
        (war_selection[[war_tag]])()[, sum(as.numeric(Weapon_Weight_Pounds),  na.rm = TRUE)]
      } else 0
    })
  }
  war_weight_reactive <- lapply(war_tags, weight_reactive)
  
  walk(list(war_missions_reactive, 
            war_flights_reactive, 
            war_bombs_reactive, 
            war_weight_reactive), 
       ~re_name(., war_tags))
  
  war_reactive <- list(missions = war_missions_reactive, 
                       flights  = war_flights_reactive, 
                       bombs    = war_bombs_reactive, 
                       weight   = war_weight_reactive)
  

### Infobox Outputs ---------------------------------------------------------
  
  get_total <- function(type) {
    function() return(sum(map_dbl(war_reactive[[type]], function(f) (f)())))
  }
  get_total_missions <- get_total('missions')
  get_total_flights  <- get_total('flights')
  get_total_bombs    <- get_total('bombs')
  get_total_weight   <- get_total('weight')
  
  # number of missions
  output$num_missions <- renderInfoBox({
    infoBox(title = "Missions", 
            value = add_commas(get_total_missions()), 
            icon = icon('chevron-up', lib = 'font-awesome'))
  })
  
  # number of aircraft
  output$num_aircraft <- renderInfoBox({
    infoBox(title = "Flights", 
            value = add_commas(get_total_flights()), 
            icon = icon('fighter-jet', lib = 'font-awesome'))
  })
  
  # number of bombs
  output$num_bombs <- renderInfoBox({
    infoBox(title = "Bombs", 
            value = add_commas(get_total_bombs()), 
            icon = icon('bomb', lib = 'font-awesome'))
  })
  
  # weight of bombs
  output$total_weight <- renderInfoBox({
    infoBox(title = "TNT Equivalent (lbs)", 
            value = add_commas(get_total_weight()), 
            icon = icon('fire', lib = 'font-awesome'))
  })
  
  
### Overview Map ------------------------------------------------------------
  
  # initialize overview leaflet map
  output$overview_map <- renderLeaflet({
    overview <- leaflet()
    overview
  })
  
  output$overview_text <- renderText({"<i>Hints for use:</i><br>
    <b>Color</b> map: best aesthetics<br>
    <b>Plain</b> map: visualize individual points<br>
    <b>Terrain</b> map: visualize terrain<br>
    <b>Street</b> map: visualize civil infrastructure<br>
    <b>Satellite</b> map: visualize current-day city features"
  })
  

### Animations --------------------------------------------------------------
  
  animation_delta <- eventReactive(eventExpr = input$map_animate_delta, 
                                   valueExpr = {
                                     switch(input$map_animate_delta, 
                                            "year"  = years(1L), 
                                            "month" = months(1L), 
                                            "week"  = weeks(1L))
                                   }, 
                                   ignoreInit = FALSE, 
                                   ignoreNULL = TRUE)
  
  animation_toggle <- reactiveVal(value = FALSE)
  
  observeEvent(input$map_animate_button, {
    debug_message("animating map")
    wars_selected <- war_tags[map(war_selected, ~(.)()) == TRUE]
    if (input$tabs %c% war_tags || !is_empty(wars_selected)) {
      previous_start_date <<- input$dateRange[1]
      previous_end_date   <<- input$dateRange[2]
      if (input$tabs %c% war_tags) {
        dates <- dates_from_selection(input$tabs)
        relevant_first_mission <- dates[["first"]]
        relevant_last_mission <- dates[["last"]]
      } else {
        dates_first <- dates_from_selection(first(wars_selected))
        dates_last <- dates_from_selection(last(wars_selected))
        relevant_first_mission  <- dates_first[["first"]]
        relevant_last_mission <- dates_last[["last"]]
      }
      current_start_date  <- floor_date(relevant_first_mission, input$map_animate_delta)
      current_end_date    <- current_start_date + animation_delta() - days(1L)
      animation_end_date <<- ceiling_date(relevant_last_mission, input$map_animate_delta) - days(1L)
      animation_toggle(TRUE)
      updateDateRangeInput(session, 
                           inputId = "dateRange", 
                           start = current_start_date, 
                           end = current_end_date)
    }
  })
  
  animation_stoppage_listener <- reactive({
    list(input$tabs, 
         input$which_war, 
         input$regions, 
         input$targets, 
         input$countries, 
         input$aircrafts, 
         input$weapons)
  })
  
  animation_stopper <- observeEvent(eventExpr = animation_stoppage_listener(), handlerExpr = {
    if (animation_toggle()) {
      animation_toggle(FALSE)
      updateDateRangeInput(session, 
                           inputId = "dateRange", 
                           start = previous_start_date, 
                           end = previous_end_date)
    }
  })
  

### Pilot Map ---------------------------------------------------------------
  
  pilot_text_output <- function(war_tag) {
    force(war_tag)
    renderText(war_video_description_phrases[[war_tag]])
  }
  for (tag in war_tags) {
    output[[pilot_text_id[[tag]]]] <- pilot_text_output(tag)
  }
  
  pilot_video_output <- function(war_tag) {
    force(war_tag)
    renderUI({
      tags$iframe(src = youtube_embed(war_videos[[war_tag]]), width = video_width, height = video_height)
    })
  }
  for (tag in war_tags) {
    output[[pilot_video_id[[tag]]]] <- pilot_video_output(tag)
  }
  

### Commander Map -----------------------------------------------------------
  
  commander_text_output <- function(war_tag) {
    force(war_tag)
    renderText(war_maps_description_phrases[[war_tag]])
  }
  for (tag in war_tags) {
    output[[commander_text_id[[tag]]]] <- commander_text_output(tag)
  }
  
  commander_maps_output <- function(war_tag, num) {
    force(war_tag)
    force(num)
    renderUI({
      img(src = war_maps[[war_tag]][[num]], width = image_width, height = image_height)
    })
  }
  for (tag in war_tags) {
    for (i in seq_along(commander_maps_ids[[tag]])) {
      output[[commander_maps_ids[[tag]][[i]]]] <- commander_maps_output(tag, i)
    }
  }
  

### Civilian Map ------------------------------------------------------------
  
  output$civilian_title <- renderText({
    "Where is the bombing the worst?"
  })
  
  # initialize civilian leaflet map
  output$civilian_map <- renderLeaflet({
    civilian <- leaflet() %>% addProviderTiles("CartoDB.Positron", layerId = "civilian_base")
    civilian
  })

    
### DataTable ---------------------------------------------------------------
  
  datatable_output <- function(war_tag) {
    reactive({
      datatable(data = (war_selection[[war_tag]])()[, war_datatable_columns[[war_tag]], with = FALSE], 
                rownames = FALSE, 
                colnames = war_datatable_colnames[[war_tag]]) %>%
        formatStyle(columns = war_datatable_columns[[war_tag]], 
                    background = war_background[[war_tag]], 
                    fontWeight = font_weight)
    })
  }
  war_datatable_output <- lapply(war_tags, datatable_output)
  re_name(war_datatable_output, war_tags)
  
  output$table <- DT::renderDataTable({
    wars_selected <- war_tags[map(war_selected, ~(.)()) == TRUE]
    if (is_empty(wars_selected)) {
      datatable(data = data.table(Example = list("Pick a war"), Data = list("to see its data")), 
                rownames = FALSE) %>%
        formatStyle(columns = 1:2, 
                    background = example_background, 
                    fontWeight = font_weight)
    } else {
      (war_datatable_output[[wars_selected[[1]]]])()
    }
  })
  
  
### Data Histograms ---------------------------------------------------------
  
  histogram_output <- function(war_tag) {
    force(war_tag)
    renderPlot({
      war_dt <- (war_selection[[war_tag]])()
      group_input <- input[[war_sandbox_group_ids[[war_tag]]]]
      ver_trans_input <- input[[war_transformation_ver_ids[[war_tag]]]]
      if (group_input == "None") {
        hist_plot <- ggplot(mapping = aes(x = war_dt[["Mission_Date"]])) + 
          geom_histogram(bins = input[[war_hist_slider_ids[[war_tag]]]])
      } else {
        group_category <- war_categorical[[war_tag]][[group_input]]
        hist_plot <- ggplot(mapping = aes(x     = war_dt[["Mission_Date"]], 
                                          color = war_dt[[group_category]])) + 
          geom_freqpoly(bins = input[[war_hist_slider_ids[[war_tag]]]]) + 
          guides(color = guide_legend(title = group_input))
      }
      hist_plot <- hist_plot + 
        ggtitle(war_histogram_title[[war_tag]]) + 
        xlab("Date") + 
        ylab("Number of Missions") + 
        theme_bw()
      if (ver_trans_input == "None") {
        hist_plot
      } else {
        hist_plot + 
          scale_y_log10()
      }
    })
  }
  for (tag in war_tags) {
    output[[war_hist_ids[[tag]]]] <- histogram_output(tag)
  }
  
  
### Data Sandboxes ----------------------------------------------------------
  
  sandbox_output <- function(war_tag) {
    force(war_tag)
    renderPlot({
      ind_input <- input[[war_sandbox_ind_ids[[war_tag]]]]
      dep_input <- input[[war_sandbox_dep_ids[[war_tag]]]]
      group_input <- input[[war_sandbox_group_ids[[war_tag]]]]
      transformation_horizontal <- input[[war_transformation_hor_ids[[war_tag]]]]
      transformation_vertical <- input[[war_transformation_ver_ids[[war_tag]]]]
      plot_dep <- war_continuous[[war_tag]][[dep_input]]
      if (ind_input == "None (All Data)") {
        make_stats_plot <- TRUE
        make_points_plot <- FALSE
        if (group_input == "None") {
          graph_dt <- copy((war_selection[[war_tag]])()[, c(dep = plot_dep), with = FALSE])
          setnames(graph_dt, c("dep"))
          sandbox_plot <- ggplot(data = graph_dt, mapping = aes(x = "", y = dep))
        } else {
          plot_group <- war_categorical[[war_tag]][[group_input]]
          graph_dt <- copy((war_selection[[war_tag]])()[, c(dep = plot_dep, grp = plot_group), with = FALSE])
          setnames(graph_dt, c("dep", "grp"))
          graph_dt[["grp"]] %>% 
            drop_levels(drop = empty_text) %>% 
            otherize_levels_count(cutoff = count_cutoff, other = "others")
          if (calculate_plotted_items(graph_dt, "grp") > subset_graph_threshold) {
            graph_dt[["grp"]] %>%
              otherize_levels_prop(cutoff = prop_cutoff, other = "others")
            if (calculate_plotted_items(graph_dt, "grp") > subset_graph_threshold) {
              graph_dt[["grp"]] %>% 
                otherize_levels_rank(cutoff = rank_cutoff, other = "others", include_ties = FALSE)
            }
          }
          sandbox_plot <- ggplot(data = graph_dt, mapping = aes(x = "", y = dep, fill = grp)) + 
            guides(fill = guide_legend(title = group_input))
        }
        sandbox_plot <- sandbox_plot + geom_violin()
      } else {
        if (ind_input %c% war_continuous_choices[[war_tag]]) {
          make_points_plot <- TRUE
          make_stats_plot <- FALSE
          plot_ind <- war_continuous[[war_tag]][[ind_input]]
        } else {
          make_stats_plot <- TRUE
          make_points_plot <- FALSE
          plot_ind <- war_categorical[[war_tag]][[ind_input]]
        }
        if (group_input == "None") {
          grouped_plot <- FALSE
          graph_dt <- copy((war_selection[[war_tag]])()[, c(ind = plot_ind, dep = plot_dep), with = FALSE])
          setnames(graph_dt, c("ind", "dep"))
        } else {
          grouped_plot <- TRUE
          plot_group <- war_categorical[[war_tag]][[group_input]]
          graph_dt <- copy((war_selection[[war_tag]])()[, c(ind = plot_ind, dep = plot_dep, grp = plot_group), with = FALSE])
          setnames(graph_dt, c("ind", "dep", "grp"))
        }
        graph_dt <- graph_dt[!is.na(dep), ]
        if (make_stats_plot) {
          graph_dt[["ind"]] %>% 
            drop_levels(drop = empty_text) %>% 
            otherize_levels_count(cutoff = count_cutoff, other = "others")
          if (grouped_plot) {
            graph_dt %>% otherize_groups_count("ind", "grp", cutoff = count_cutoff, other = "others")
            if (calculate_plotted_items(graph_dt, "ind", "grp") > subset_graph_threshold) {
              graph_dt %>% otherize_groups_prop("ind", "grp", cutoff = prop_cutoff, other = "others")
              if (calculate_plotted_items(graph_dt, "ind", "grp") > subset_graph_threshold) {
                graph_dt %>% otherize_groups_rank("ind", "grp", cutoff = rank_cutoff, other = "others",  include_ties = FALSE)
              }
            }
          } else {
            if (calculate_plotted_items(graph_dt, "ind", "grp") > subset_graph_threshold) {
              graph_dt[["ind"]] %>%
                otherize_levels_prop(cutoff = prop_cutoff, other = "others")
              if (calculate_plotted_items(graph_dt, "ind", "grp") > subset_graph_threshold) {
                graph_dt[["ind"]] %>% 
                  otherize_levels_rank(cutoff = rank_cutoff, other = "others", include_ties = FALSE)
              }
            }
          }
          needs_reordering <- plot_ind != "Year" && plot_ind != "Month"
          if (needs_reordering) {
            graph_order <- graph_dt[, .(func = (function(x) median(x) + mean(x)/10)(dep)), by = ind][order(func)][["ind"]]
            graph_dt[, ind := ordered(ind, levels = graph_order)]
          }
        } else {
          graph_dt <- graph_dt[!is.na(ind), ]
        }
        sandbox_plot <- ggplot(data = graph_dt, mapping = aes(x = ind, y = dep))
        if (grouped_plot) {
          if (make_points_plot) {
            sandbox_plot <- sandbox_plot + aes(color = grp) + 
              guides(color = guide_legend(title = group_input))
          } else {
            sandbox_plot <- sandbox_plot + aes(fill = grp) + 
              guides(fill = guide_legend(title = group_input))
          }
        }
      }
      if (make_points_plot) {
        sandbox_plot <- sandbox_plot + 
          geom_point() + 
          geom_smooth(method = 'lm')
        if (transformation_horizontal == "Logarithm") {
          sandbox_plot <- sandbox_plot + scale_x_log10()
        }
        if (transformation_vertical == "Logarithm") {
          sandbox_plot <- sandbox_plot + scale_y_log10()
        }
      } else if (make_stats_plot) {
        if (transformation_vertical == "Logarithm") {
          sandbox_plot <- sandbox_plot + 
            geom_violin(draw_quantiles = c(0.25, 0.50, 0.75), trim = FALSE) + 
            stat_summary(fun.y = mean, geom = 'point', shape = 23, size = 2, position = position_dodge(width = 0.9)) + 
            scale_y_log10()
        } else {
          sandbox_plot <- sandbox_plot + 
            geom_violin(draw_quantiles = c(0.25, 0.50, 0.75), trim = FALSE) + 
            stat_summary(fun.y = mean, geom = 'point', shape = 23, size = 2, position = position_dodge(width = 0.9))
        }
        if (ind_input != "None (All Data)") {
          if (calculate_plotted_items(graph_dt, "ind", "grp") > coord_flip_threshold) {
            sandbox_plot <- sandbox_plot + coord_flip()
          }
        }
      }
      sandbox_plot + 
        ggtitle(war_sandbox_title[[war_tag]]) + 
        xlab(ind_input) + 
        ylab(dep_input) + 
        theme_bw()
    })
  }
  for (tag in war_tags) {
    output[[war_sandbox_ids[[tag]]]] <- sandbox_output(tag)
  }
  
  calculate_plotted_items <- function(dt, var_colname, group_colname = NULL) {
    assert_that(!is.null(var_colname), 
                msg = "include the name of the column you're looking to plot (cannot be NULL)")
    if (is.null(group_colname) || group_colname %!c% colnames(dt)) {
      return (uniqueN(dt[[var_colname]]))
    } else {
      return (dt[, .(uniqueN = uniqueN(.SD)), .SDcols = var_colname, by = group_colname][, sum(uniqueN)])
    }
  }
  

### Observers ---------------------------------------------------------------


### Map observers -----------------------------------------------------------
  
  # hanlder for changes in map type
  observeEvent(eventExpr = input$pick_map, handlerExpr = {
    debug_message("map altered")
    # remove other tiles and add designated map
    fix_map_base(map_type = input$pick_map)
    # gotta redraw the map labels if the underlying map has changed
    fix_map_labels(borders = "Borders" %c% input$pick_labels, 
                   text = "Text" %c% input$pick_labels)
  })
  
  # handler for changes in map labels
  observeEvent(eventExpr = input$pick_labels, ignoreNULL = FALSE, handlerExpr = {
    debug_message("labels altered")
    fix_map_labels(borders = "Borders" %c% input$pick_labels, 
                   text = "Text" %c% input$pick_labels)
  })
  
  # handler for changes in map zoom
  observeEvent(eventExpr = input$overview_map_zoom, handlerExpr = {
    debug_message("map zoomed")
    redraw_overview()
  })
  

### War observer ------------------------------------------------------------

  # handler for war selection
  observeEvent(eventExpr = input$which_war, ignoreNULL = FALSE, ignoreInit = TRUE, handlerExpr = {
    debug_message("wars selected")
    diff_war <- war_tags[which(war_labels == previous_wars_selection %dd% input$which_war)]
    deselected <- length(previous_wars_selection) > length(input$which_war)
    if (is_scalar(diff_war)) {
      if (deselected) {
        debug_message0(diff_war, "deselected")
        (war_clear[[diff_war]])()
        hide_video(diff_war)
        hide_map(diff_war)
      } else {
        debug_message0(diff_war, "selected")
        (war_draw[[diff_war]])()
        show_video(diff_war)
        show_map(diff_war)
      }
    } else {
      if (deselected) {
        debug_message("all wars deselected")
        for (tag in diff_war) {
          (war_clear[[tag]])()
          hide_video(tag)
          hide_map(tag)
        }
      } else {
        debug_message("impossible?")
      }
    }
    update_selectize_inputs()
    previous_wars_selection <<- input$which_war
  })
  

### Country observer --------------------------------------------------------
  
  # general observer maker
  dropdown_observer <- function(type) {
    observeEvent(eventExpr = input[[type]], ignoreNULL = FALSE, ignoreInit = TRUE, handlerExpr = {
      debug_message0(type, "selected")
      if (change_token %c% previous_dropdown_selection[[type]]) {
        previous_dropdown_selection[[type]] <<- previous_dropdown_selection[[type]] %d% change_token
        redraw_tab(input$tabs)
        update_other_selectize_inputs(type)
      } else {
        this_input <- input[[type]]
        if (is_empty(this_input)) {
          previous_dropdown_selection[[type]] <<- c("All", change_token)
          updateSelectizeInput(session, inputId = type, selected = "All")
        } else {
          difference <- previous_dropdown_selection[[type]] %dd% this_input
          selected <- length(this_input) > length(previous_dropdown_selection[[type]])
          if ("All" %c% previous_dropdown_selection[[type]]) {
            previous_dropdown_selection[[type]] <<- c(difference, change_token)
            updateSelectizeInput(session, inputId = type, selected = difference)
          } else {
            if ("All" %e% difference) {
              previous_dropdown_selection[[type]] <<- c("All", change_token)
              updateSelectizeInput(session, inputId = type, selected = "All")
            } else {
              previous_dropdown_selection[[type]] <<- this_input
              redraw_tab(input$tabs)
              update_other_selectize_inputs(type)
            }
          }
        }
      }
    })
  }
  dropdown_observers <- lapply(dropdown_tags, dropdown_observer)
  re_name(dropdown_observers, dropdown_tags)
  

### Other observers ---------------------------------------------------------

  # tab selection (redraw after switching back to a map tab)
  observeEvent(eventExpr = input$tabs, ignoreNULL = TRUE, ignoreInit = TRUE, handlerExpr = {
    redraw_tab(input$tabs)
  })
  
  # handler for sample size refresh
  observeEvent(eventExpr = input$sample_num, ignoreNULL = TRUE, ignoreInit = TRUE, handlerExpr = {
    debug_message("sample size changed")
    redraw_overview()
  })
  
  # handler for date range refresh
  observeEvent(eventExpr = input$dateRange, ignoreNULL = TRUE, ignoreInit = TRUE, handlerExpr = {
    debug_message("date range changed")
    redraw_tab(input$tabs)
    if (animation_toggle()) {
      if (input$dateRange[1] + animation_delta() < animation_end_date) {
        Sys.sleep(animation_delays[[input$map_animate_delta]])
        updateDateRangeInput(session, 
                             inputId = "dateRange", 
                             start = input$dateRange[1] + animation_delta(), 
                             end = input$dateRange[2] + animation_delta())
      } else {
        Sys.sleep(animation_delays[[input$map_animate_delta]])
        animation_toggle(FALSE)
        updateDateRangeInput(session, 
                             inputId = "dateRange", 
                             start = previous_start_date, 
                             end = previous_end_date)
      }
    }
  })
  
  
### War Map Drawers ---------------------------------------------------------

  draw_overview_war <- function(war_tag) {
    function() {
      opacity <- calculate_opacity(min((war_missions_reactive[[war_tag]])(), input$sample_num), input$overview_map_zoom)
      overview_proxy_fast %>% addCircles(data = (war_sample[[war_tag]])(),
                                         lat = ~Target_Latitude,
                                         lng = ~Target_Longitude,
                                         color = war_color[[war_tag]],
                                         weight = point_weight, 
                                         radius = {temp <- (war_sample[[war_tag]])()[["Damage_Radius"]]; 
                                                   pmax(temp, temp * 1e3 * 2 ** -input$overview_map_zoom)}, 
                                         opacity = opacity,
                                         fill = point_fill,
                                         fillColor = war_color[[war_tag]],
                                         fillOpacity = opacity,
                                         popup = ~tooltip,
                                         group = war_overview[[war_tag]])
    }
  }
  war_draw_overview <- lapply(war_tags, draw_overview_war)
  
  clear_overview_war <- function(war_tag) {
    function() {
      overview_proxy_fast %>% clearGroup(group = war_overview[[war_tag]])
    }
  }
  war_clear_overview <- lapply(war_tags, clear_overview_war)
  
  draw_civilian_war <- function(war_tag) {
    function() {
      civilian_proxy_fast %>% addHeatmap(data = (war_selection[[war_tag]])(), 
                                         lng = ~Target_Longitude, 
                                         lat = ~Target_Latitude, 
                                         blur = civilian_blur, 
                                         max = civilian_max, 
                                         radius = civilian_radius, 
                                         group = war_civilian[[war_tag]])
    }
  }
  war_draw_civilian <- lapply(war_tags, draw_civilian_war)
  
  clear_civilian_war <- function(war_tag) {
    function() {
      civilian_proxy_fast %>% clearGroup(group = war_civilian[[war_tag]])
    }
  }
  war_clear_civilian <- lapply(war_tags, clear_civilian_war)
  
  walk(list(war_draw_overview, 
            war_clear_overview, 
            war_draw_civilian, 
            war_clear_civilian), 
       ~re_name(., war_tags))
  

### War Map Composite Drawers -----------------------------------------------
  
  redraw_overview_war <- function(war_tag) {
    function() {
      (war_clear_overview[[war_tag]])()
      (war_draw_overview[[war_tag]])()
    }
  }
  war_redraw_overview <- lapply(war_tags, redraw_overview_war)
  
  redraw_civilian_war <- function(war_tag) {
    function() {
      (war_clear_civilian[[war_tag]])()
      (war_draw_civilian[[war_tag]])()
    }
  }
  war_redraw_civilian <- lapply(war_tags, redraw_civilian_war)
  
  draw_war <- function(war_tag) {
    function() {
      (war_draw_overview[[war_tag]])()
      (war_draw_civilian[[war_tag]])()
    }
  }
  war_draw <- lapply(war_tags, draw_war)
  
  clear_war <- function(war_tag) {
    function() {
      (war_clear_overview[[war_tag]])()
      (war_clear_civilian[[war_tag]])()
    }
  }
  war_clear <- lapply(war_tags, clear_war)
  
  redraw_war <- function(war_tag) {
    function() {
      (war_redraw_overview[[war_tag]])()
      (war_redraw_civilian[[war_tag]])()
    }
  }
  war_redraw <- lapply(war_tags, redraw_war)
  
  walk(list(war_redraw_overview, 
            war_redraw_civilian, 
            war_draw, 
            war_clear, 
            war_redraw), 
       ~re_name(., war_tags))
  

### Total War Map Drawers ---------------------------------------------------

  redraw_overview <- function() {
    for (tag in war_tags) {
      if ((war_selected[[tag]])()) (war_redraw_overview[[tag]])()
    }
  }
  
  redraw_civilian <- function() {
    for (tag in war_tags) {
      if ((war_selected[[tag]])()) (war_redraw_civilian[[tag]])()
    }
  }
  
  redraw <- function() {
    debug_message("redrew")
    for (tag in war_tags) {
      if ((war_selected[[tag]])()) (war_redraw[[tag]])()
    }
  }
  
  redraw_tab <- function(tab) {
    if (tab == "overview") {
      redraw_overview()
    } else if (tab == "civilian") {
      redraw_civilian()
    }
  }
  

### Map Drawers -------------------------------------------------------------

  swap_map_base <- function(type, options = NULL) {
    overview_proxy %>% clearTiles() %>% addProviderTiles(provider = type, layerId = "overview_base", options = options)
  }
  
  fix_map_base <- function(map_type) {
    switch(map_type, 
           "Color Map"     = swap_map_base(type = "Stamen.Watercolor"), 
           "Plain Map"     = swap_map_base(type = "CartoDB.PositronNoLabels"), 
           "Terrain Map"   = swap_map_base(type = "Stamen.TerrainBackground"), 
           "Street Map"    = swap_map_base(type = "HERE.basicMap", options = providerTileOptions(app_id = HERE_id, app_code = HERE_code)), 
           "Satellite Map" = swap_map_base(type = "Esri.WorldImagery"))
  }
  
  swap_map_labels <- function(type) {
    overview_proxy %>% removeTiles(layerId = "overview_labels")
    if (type != "none") {
      overview_proxy %>% addProviderTiles(type, layerId = "overview_labels")
    }
  }
  
  fix_map_labels <- function(borders, text) {
    if (borders) {
      if (text) {
        debug_message("Both borders and text")
        swap_map_labels(type = "Stamen.TonerHybrid")
      } else {
        debug_message("Just borders; no text")
        swap_map_labels(type = "Stamen.TonerLines")
      }
    } else {
      if (text) {
        debug_message("Just text; no borders")
        swap_map_labels(type = "Stamen.TonerLabels")
      } else {
        debug_message("Neither text nor borders")
        swap_map_labels(type = "none")
      }
    }
  }
  
  
### Video and Map Updaters --------------------------------------------------
  
  show_video <- function(war_tag) {
    shinyjs::hide(pilot_text_box_id[[war_tag]])
    shinyjs::show(pilot_video_box_id[[war_tag]])
  }
  
  hide_video <- function(war_tag) {
    shinyjs::hide(pilot_video_box_id[[war_tag]])
    shinyjs::show(pilot_text_box_id[[war_tag]])
  }
  
  show_map <- function(war_tag) {
    shinyjs::hide(commander_text_box_id[[war_tag]])
    shinyjs::show(commander_maps_box_id[[war_tag]])
  }
  
  hide_map <- function(war_tag) {
    shinyjs::hide(commander_maps_box_id[[war_tag]])
    shinyjs::show(commander_text_box_id[[war_tag]])
  }
  
  
### Dropdown Updaters -------------------------------------------------------
  
  update_dropdown <- function(type) {
    function() {
      debug_message0(type, "choices updated")
      choices <- c("All", possible_selectize_choices(dropdowns[[type]]))
      this_input <- input[[type]]
      updateSelectizeInput(session, 
                           inputId = type, 
                           choices = choices, 
                           selected = this_input %whichin% choices %OR% "All")
    }
  }
  update_dropdowns <- lapply(dropdown_tags, update_dropdown)
  
  update_selectize_inputs <- function() {
    walk(update_dropdowns,
         ~(.)())
  }
  
  update_other_selectize_inputs <- function(changed) {
    walk(update_dropdowns[dropdown_tags != changed],
         ~(.)())
  }
  

### Filtering Functions -----------------------------------------------------

  selected_war_levels <- function(column) {
    possible_levels <- c()
    for (tag in war_tags) {
      if ((war_selected[[tag]])()) {
        possible_levels <- c(possible_levels, levels(war_data[[tag]][[column]]))
      }
    }
    return (possible_levels)
  }
  
  possible_selectize_choices <- function(column) {
    start_date <- input$dateRange[1]
    end_date   <- input$dateRange[2]
    regions    <- input$regions %whichin% c("All", selected_war_levels("Target_Country")) %OR% "All"
    targets    <- input$targets %whichin% c("All", selected_war_levels("Target_Category")) %OR% "All"
    countries  <- input$countries %whichin% c("All", selected_war_levels("Unit_Country")) %OR% "All"
    aircrafts  <- input$aircrafts %whichin% c("All", selected_war_levels("Aircraft_Type")) %OR% "All"
    weapons    <- input$weapons %whichin% c("All", selected_war_levels("Weapon_Type")) %OR% "All"
    switch(column, 
           "Target_Country"  = {regions   <- "All"}, 
           "Target_Category" = {targets   <- "All"}, 
           "Unit_Country"    = {countries <- "All"}, 
           "Aircraft_Type"   = {aircrafts <- "All"}, 
           "Weapon_Type"     = {weapons   <- "All"})
    multi_selectors <- list(regions, targets, countries, aircrafts, weapons)
    selectors <- c(list(start_date, end_date), multi_selectors)
    re_name(selectors, c("start_date", "end_date", dropdown_tags))
    if (all(multi_selectors %in% "All")) {
      result <- selected_war_levels(column)
    } else {
      result <- c()
      for (tag in war_tags) {
        if ((war_selected[[tag]])()) {
          arg_list <- list("war_tag" = tag, "column" = column, "criteria" = selectors)
          result <- c(result, do.call(unique_from_filter, arg_list))
        }
      }
    }
    if (!is_empty(result)) {
      result <- sort(unique(result))
      if (empty_text %c% result) {
        result <- c(result %[!=]% empty_text, empty_text)
      }
    }
    result
  }
  
  unique_from_filter_slow <- function(war_tag, column, criteria) {
    arg_list <- c(list("war_tag" = war_tag), criteria)
    return (levels(do.call(filter_selection, arg_list)[[column]][, drop = TRUE]))
  }
  
  unique_from_filter <- memoise(unique_from_filter_slow)
  
  dates_from_selection <- function(war_tag) {
    dates <- (war_selection[[war_tag]])()[["Mission_Date"]]
    return (list("first" = first(dates), "last" = last(dates)))
  }
  
  filter_selection <- function(war_tag, start_date = earliest_date, end_date = latest_date, regions = "All", targets = "All", countries = "All", aircrafts = "All", weapons = "All") {
    if ("All" %c% regions) {
      if ("All" %c% targets) {
        if ("All" %c% countries) {
          if ("All" %c% aircrafts) {
            if ("All" %c% weapons) {
              war_dt <- war_data[[war_tag]]
            } else {
              war_dt <- war_data[[war_tag]][.(weapons), on = .(Weapon_Type), nomatch = 0L]
            }
          } else {
            if ("All" %c% weapons) {
              war_dt <- war_data[[war_tag]][.(aircrafts), on = .(Aircraft_Type), nomatch = 0L]
            } else {
              war_dt <- war_data[[war_tag]][.(aircrafts, weapons), on = .(Aircraft_Type, Weapon_Type), nomatch = 0L]
            }
          }
        } else {
          if ("All" %c% aircrafts) {
            if ("All" %c% weapons) {
              war_dt <- war_data[[war_tag]][.(countries), on = .(Unit_Country), nomatch = 0L]
            } else {
              war_dt <- war_data[[war_tag]][.(countries, weapons), on = .(Unit_Country, Weapon_Type), nomatch = 0L]
            }
          } else {
            if ("All" %c% weapons) {
              war_dt <- war_data[[war_tag]][.(countries, aircrafts), on = .(Unit_Country, Aircraft_Type), nomatch = 0L]
            } else {
              war_dt <- war_data[[war_tag]][.(countries, aircrafts, weapons), on = .(Unit_Country, Aircraft_Type, Weapon_Type), nomatch = 0L]
            }
          }
        }
      } else {
        if ("All" %c% countries) {
          if ("All" %c% aircrafts) {
            if ("All" %c% weapons) {
              war_dt <- war_data[[war_tag]][.(targets), on = .(Target_Category), nomatch = 0L]
            } else {
              war_dt <- war_data[[war_tag]][.(targets, weapons), on = .(Target_Category, Weapon_Type), nomatch = 0L]
            }
          } else {
            if ("All" %c% weapons) {
              war_dt <- war_data[[war_tag]][.(targets, aircrafts), on = .(Target_Category, Aircraft_Type), nomatch = 0L]
            } else {
              war_dt <- war_data[[war_tag]][.(targets, aircrafts, weapons), on = .(Target_Category, Aircraft_Type, Weapon_Type), nomatch = 0L]
            }
          }
        } else {
          if ("All" %c% aircrafts) {
            if ("All" %c% weapons) {
              war_dt <- war_data[[war_tag]][.(targets, countries), on = .(Target_Category, Unit_Country), nomatch = 0L]
            } else {
              war_dt <- war_data[[war_tag]][.(targets, countries, weapons), on = .(Target_Category, Unit_Country, Weapon_Type), nomatch = 0L]
            }
          } else {
            if ("All" %c% weapons) {
              war_dt <- war_data[[war_tag]][.(targets, countries, aircrafts), on = .(Target_Category, Unit_Country, Aircraft_Type), nomatch = 0L]
            } else {
              war_dt <- war_data[[war_tag]][.(targets, countries, aircrafts, weapons), on = .(Target_Category, Unit_Country, Aircraft_Type, Weapon_Type), nomatch = 0L]
            }
          }
        }
      }
    } else {
      if ("All" %c% targets) {
        if ("All" %c% countries) {
          if ("All" %c% aircrafts) {
            if ("All" %c% weapons) {
              war_dt <- war_data[[war_tag]][.(regions), on = .(Target_Country), nomatch = 0L]
            } else {
              war_dt <- war_data[[war_tag]][.(regions, weapons), on = .(Target_Country, Weapon_Type), nomatch = 0L]
            }
          } else {
            if ("All" %c% weapons) {
              war_dt <- war_data[[war_tag]][.(regions, aircrafts), on = .(Target_Country, Aircraft_Type), nomatch = 0L]
            } else {
              war_dt <- war_data[[war_tag]][.(regions, aircrafts, weapons), on = .(Target_Country, Aircraft_Type, Weapon_Type), nomatch = 0L]
            }
          }
        } else {
          if ("All" %c% aircrafts) {
            if ("All" %c% weapons) {
              war_dt <- war_data[[war_tag]][.(regions, countries), on = .(Target_Country, Unit_Country), nomatch = 0L]
            } else {
              war_dt <- war_data[[war_tag]][.(regions, countries, weapons), on = .(Target_Country, Unit_Country, Weapon_Type), nomatch = 0L]
            }
          } else {
            if ("All" %c% weapons) {
              war_dt <- war_data[[war_tag]][.(regions, countries, aircrafts), on = .(Target_Country, Unit_Country, Aircraft_Type), nomatch = 0L]
            } else {
              war_dt <- war_data[[war_tag]][.(regions, countries, aircrafts, weapons), on = .(Target_Country, Unit_Country, Aircraft_Type, Weapon_Type), nomatch = 0L]
            }
          }
        }
      } else {
        if ("All" %c% countries) {
          if ("All" %c% aircrafts) {
            if ("All" %c% weapons) {
              war_dt <- war_data[[war_tag]][.(regions, targets), on = .(Target_Country, Target_Category), nomatch = 0L]
            } else {
              war_dt <- war_data[[war_tag]][.(regions, targets, weapons), on = .(Target_Country, Target_Category, Weapon_Type), nomatch = 0L]
            }
          } else {
            if ("All" %c% weapons) {
              war_dt <- war_data[[war_tag]][.(regions, targets, aircrafts), on = .(Target_Country, Target_Category, Aircraft_Type), nomatch = 0L]
            } else {
              war_dt <- war_data[[war_tag]][.(regions, targets, aircrafts, weapons), on = .(Target_Country, Target_Category, Aircraft_Type, Weapon_Type), nomatch = 0L]
            }
          }
        } else {
          if ("All" %c% aircrafts) {
            if ("All" %c% weapons) {
              war_dt <- war_data[[war_tag]][.(regions, targets, countries), on = .(Target_Country, Target_Category, Unit_Country), nomatch = 0L]
            } else {
              war_dt <- war_data[[war_tag]][.(regions, targets, countries, weapons), on = .(Target_Country, Target_Category, Unit_Country, Weapon_Type), nomatch = 0L]
            }
          } else {
            if ("All" %c% weapons) {
              war_dt <- war_data[[war_tag]][.(regions, targets, countries, aircrafts), on = .(Target_Country, Target_Category, Unit_Country, Aircraft_Type), nomatch = 0L]
            } else {
              war_dt <- war_data[[war_tag]][.(regions, targets, countries, aircrafts, weapons), on = .(Target_Country, Target_Category, Unit_Country, Aircraft_Type, Weapon_Type), nomatch = 0L]
            }
          }
        }
      }
    }
    
    if (start_date > war_first_missions[[war_tag]]) {
      if (end_date < war_last_missions[[war_tag]]) {
        war_dt[Mission_Date >= start_date & Mission_Date <= end_date, nomatch = 0L]
      } else {
        war_dt[Mission_Date >= start_date, nomatch = 0L]
      }
    } else {
      if (end_date < war_last_missions[[war_tag]]) {
        war_dt[Mission_Date <= end_date, nomatch = 0L]
      } else {
        war_dt
      }
    }
  }
  

### Init --------------------------------------------------------------------

  for (tag in war_tags) {
    hide_video(tag)
    hide_map(tag)
  }
  
  
})
