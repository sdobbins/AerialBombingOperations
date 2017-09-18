# @author Scott Dobbins
# @version 0.9.9
# @date 2017-09-17 18:30


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
    infoBox(title = "Aircraft Flights", 
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
  

### Pilot Map ---------------------------------------------------------------
  
  pilot_text_output <- function(war_tag) {
    renderText(war_video_description_phrases[[war_tag]])
  }
  output$pilot_text_WW1     <- pilot_text_output(WW1)
  output$pilot_text_WW2     <- pilot_text_output(WW2)
  output$pilot_text_Korea   <- pilot_text_output(Korea)
  output$pilot_text_Vietnam <- pilot_text_output(Vietnam)
  
  pilot_video_output <- function(war_tag) {
    renderUI({
      tags$iframe(src = youtube_embed(war_videos[[war_tag]]), width = video_width, height = video_height)
    })
  }
  output$pilot_video_WW1     <- pilot_video_output(WW1)
  output$pilot_video_WW2     <- pilot_video_output(WW2)
  output$pilot_video_Korea   <- pilot_video_output(Korea)
  output$pilot_video_Vietnam <- pilot_video_output(Vietnam)
  

### Commander Map -----------------------------------------------------------
  
  commander_text_output <- function(war_tag) {
    renderText(war_maps_description_phrases[[war_tag]])
  }
  output$commander_text_WW1     <- commander_text_output(WW1)
  output$commander_text_WW2     <- commander_text_output(WW2)
  output$commander_text_Korea   <- commander_text_output(Korea)
  output$commander_text_Vietnam <- commander_text_output(Vietnam)
  
  commander_maps_output <- function(war_tag, num) {
    renderUI({
      img(src = war_maps[[war_tag]][[num]], width = image_width, height = image_height)
    })
  }
  output$commander_map_WW1_1     <- commander_maps_output(WW1, 1)
  output$commander_map_WW1_2     <- commander_maps_output(WW1, 2)
  output$commander_map_WW2_1     <- commander_maps_output(WW2, 1)
  output$commander_map_WW2_2     <- commander_maps_output(WW2, 2)
  output$commander_map_Korea_1   <- commander_maps_output(Korea, 1)
  output$commander_map_Korea_2   <- commander_maps_output(Korea, 2)
  output$commander_map_Vietnam_1 <- commander_maps_output(Vietnam, 1)
  output$commander_map_Vietnam_2 <- commander_maps_output(Vietnam, 2)
  

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
  
  
### Data Graph Inputs -------------------------------------------------------
  
  war_sandbox_group_input <- function(war_tag) {
    switch(war_tag, 
           WW1     = input$WW1_sandbox_group, 
           WW2     = input$WW2_sandbox_group, 
           Korea   = input$Korea_sandbox_group, 
           Vietnam = input$Vietnam_sandbox_group)
  }
  
  war_sandbox_ind_input <- function(war_tag) {
    switch(war_tag, 
           WW1     = input$WW1_sandbox_ind, 
           WW2     = input$WW2_sandbox_ind, 
           Korea   = input$Korea_sandbox_ind, 
           Vietnam = input$Vietnam_sandbox_ind)
  }
  
  war_sandbox_dep_input <- function(war_tag) {
    switch(war_tag, 
           WW1     = input$WW1_sandbox_dep, 
           WW2     = input$WW2_sandbox_dep, 
           Korea   = input$Korea_sandbox_dep, 
           Vietnam = input$Vietnam_sandbox_dep)
  }
  
  war_hist_slider_input <- function(war_tag) {
    switch(war_tag, 
           WW1     = input$WW1_hist_slider, 
           WW2     = input$WW2_hist_slider, 
           Korea   = input$Korea_hist_slider, 
           Vietnam = input$Vietnam_hist_slider)
  }
  
  war_hor_trans_input <- function(war_tag) {
    switch(war_tag, 
           WW1     = input$WW1_transformation_hor, 
           WW2     = input$WW2_transformation_hor, 
           Korea   = input$Korea_transformation_hor, 
           Vietnam = input$Vietnam_transformation_hor)
  }
  
  war_ver_trans_input <- function(war_tag) {
    switch(war_tag, 
           WW1     = input$WW1_transformation_ver, 
           WW2     = input$WW2_transformation_ver, 
           Korea   = input$Korea_transformation_ver, 
           Vietnam = input$Vietnam_transformation_ver)
  }
  

### Dropdown Inputs ---------------------------------------------------------

  dropdown_input <- function(type) {
    input[[type]]
    # switch(type, 
    #        country = input$country, 
    #        aircraft = input$aircraft, 
    #        weapon = input$weapon)
  }
  
  
### Data Histograms ---------------------------------------------------------
  
  histogram_output <- function(war_tag) {
    renderPlot({
      war_dt <- (war_selection[[war_tag]])()
      group_input <- war_sandbox_group_input(war_tag)
      ver_trans_input <- war_ver_trans_input(war_tag)
      if (group_input == "None") {
        hist_plot <- ggplot(mapping = aes(x = war_dt[["Mission_Date"]])) + 
          geom_histogram(bins = war_hist_slider_input(war_tag))
      } else {
        group_category <- war_categorical[[war_tag]][[group_input]]
        hist_plot <- ggplot(mapping = aes(x     = war_dt[["Mission_Date"]], 
                                          color = war_dt[[group_category]])) + 
          geom_freqpoly(bins = war_hist_slider_input(war_tag)) + 
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
  output$WW1_hist     <- histogram_output(WW1)
  output$WW2_hist     <- histogram_output(WW2)
  output$Korea_hist   <- histogram_output(Korea)
  output$Vietnam_hist <- histogram_output(Vietnam)
  
  
### Data Sandboxes ----------------------------------------------------------
  
  sandbox_output <- function(war_tag) {
    renderPlot({
      ind_input <- war_sandbox_ind_input(war_tag)
      dep_input <- war_sandbox_dep_input(war_tag)
      group_input <- war_sandbox_group_input(war_tag)
      war_dt <- (war_selection[[war_tag]])()
      plot_dep <- war_continuous[[war_tag]][[dep_input]]
      plot_group <- war_categorical[[war_tag]][[group_input]]
      if (ind_input %c% war_continuous_choices[[war_tag]]) {
        plot_ind <- war_continuous[[war_tag]][[ind_input]]
        sandbox_plot <- ggplot(data = war_dt, 
                               mapping = aes_string(x     = plot_ind, 
                                                    y     = plot_dep, 
                                                    color = plot_group)) + 
          guides(color = guide_legend(title = group_input)) + 
          geom_point() + 
          geom_smooth(method = 'lm')
        if (war_hor_trans_input(war_tag) == "Logarithm") {
          sandbox_plot <- sandbox_plot + scale_x_log10()
        }
      } else {
        plot_ind <- war_categorical[[war_tag]][[ind_input]]
        if (ind_input == "None (All Data)") {
          if (group_input == "None") {
            sandbox_plot <- ggplot(mapping = aes(x = "", 
                                                 y = war_dt[[plot_dep]]))
          } else {
            sandbox_plot <- ggplot(mapping = aes(x    = "", 
                                                 y    = war_dt[[plot_dep]], 
                                                 fill = war_dt[[plot_group]])) + 
              guides(fill = guide_legend(title = group_input))
          }
        } else {
          sandbox_plot <- ggplot(data = war_dt, 
                                 mapping = aes_string(x    = plot_ind, 
                                                      y    = plot_dep, 
                                                      fill = plot_group)) + 
            guides(fill = guide_legend(title = group_input))
        }
        sandbox_plot <- sandbox_plot + 
          geom_violin(draw_quantiles = c(0.25, 0.50, 0.75))
      }
      if (war_ver_trans_input(war_tag) == "Logarithm") {
        sandbox_plot <- sandbox_plot + scale_y_log10()
      }
      sandbox_plot + 
        ggtitle(war_sandbox_title[[war_tag]]) + 
        xlab(ind_input) + 
        ylab(dep_input) + 
        theme_bw()
    })
  }
  output$WW1_sandbox     <- sandbox_output(WW1)
  output$WW2_sandbox     <- sandbox_output(WW2)
  output$Korea_sandbox   <- sandbox_output(Korea)
  output$Vietnam_sandbox <- sandbox_output(Vietnam)
  

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
    observeEvent(eventExpr = dropdown_input(type), ignoreNULL = FALSE, ignoreInit = TRUE, handlerExpr = {
      debug_message0(type, "selected")
      if (change_token %c% previous_dropdown_selection[[type]]) {
        previous_dropdown_selection[[type]] <<- previous_dropdown_selection[[type]] %d% change_token
        redraw()
        update_other_selectize_inputs(type)
      } else {
        input <- dropdown_input(type)
        if (is_empty(input)) {
          previous_dropdown_selection[[type]] <<- c("All", change_token)
          updateSelectizeInput(session, inputId = type, selected = "All")
        } else {
          difference <- previous_dropdown_selection[[type]] %dd% input
          selected <- length(input) > length(previous_dropdown_selection[[type]])
          if ("All" %c% previous_dropdown_selection[[type]]) {
            previous_dropdown_selection[[type]] <<- c(difference, change_token)
            updateSelectizeInput(session, inputId = type, selected = difference)
          } else {
            if ("All" %e% difference) {
              previous_dropdown_selection[[type]] <<- c("All", change_token)
              updateSelectizeInput(session, inputId = type, selected = "All")
            } else {
              previous_dropdown_selection[[type]] <<- input
              redraw()
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

  # handler for sample size refresh
  observeEvent(eventExpr = input$sample_num, ignoreNULL = TRUE, ignoreInit = TRUE, handlerExpr = {
    debug_message("sample size changed")
    redraw_overview()
  })
  
  # handler for date range refresh
  observeEvent(eventExpr = input$dateRange, ignoreNULL = TRUE, ignoreInit = TRUE, handlerExpr = {
    debug_message("date range changed")
    redraw()
  })
  
  
### War Map Drawers ---------------------------------------------------------

  draw_overview_war <- function(war_tag) {
    function() {
      opacity <- calculate_opacity(min((war_missions_reactive[[war_tag]])(), input$sample_num), input$overview_map_zoom)
      overview_proxy %>% addCircles(data = (war_sample[[war_tag]])(),
                                    lat = ~Target_Latitude,
                                    lng = ~Target_Longitude,
                                    color = war_color[[war_tag]],
                                    weight = point_weight, 
                                    radius = {temp <- (war_sample[[war_tag]])()[["Damage_Radius"]]; pmax(temp, temp * 1e3 * 2 ** -input$overview_map_zoom)}, 
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
      overview_proxy %>% clearGroup(group = war_overview[[war_tag]])
    }
  }
  war_clear_overview <- lapply(war_tags, clear_overview_war)
  
  draw_civilian_war <- function(war_tag) {
    function() {
      civilian_proxy %>% addHeatmap(data = (war_selection[[war_tag]])(), 
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
      civilian_proxy %>% clearGroup(group = war_civilian[[war_tag]])
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
    for (tag in war_tags) {
      if ((war_selected[[tag]])()) (war_redraw[[tag]])()
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
      input <- dropdown_input(type)
      matches <- input %in% choices
      if (any(matches)) {
        selected <- input[matches]
      } else {
        selected <- "All"
      }
      updateSelectizeInput(session, 
                           inputId = type, 
                           choices = choices, 
                           selected = selected)
    }
  }
  update_regions <- update_dropdown('regions')
  update_targets <- update_dropdown('targets')
  update_countries <- update_dropdown('countries')
  update_aircrafts <- update_dropdown('aircrafts')
  update_weapons   <- update_dropdown('weapons')
  # update_dropdowns <- lapply(dropdown_tags, update_dropdown)
  
  update_selectize_inputs <- function() {
    update_regions()
    update_targets()
    update_countries()
    update_aircrafts()
    update_weapons()
    # walk(update_dropdowns, 
    #      ~(.)())
  }
  
  update_other_selectize_inputs <- function(changed) {
    switch(changed, 
           regions   = {update_targets()
                        update_countries()
                        update_aircrafts()
                        update_weapons()}, 
           targets   = {update_regions()
                        update_countries()
                        update_aircrafts()
                        update_weapons()}, 
           countries = {update_regions()
                        update_targets()
                        update_aircrafts()
                        update_weapons()},
           aircrafts = {update_regions()
                        update_targets()
                        update_countries()
                        update_weapons()},
           weapons   = {update_regions()
                        update_targets()
                        update_countries()
                        update_aircrafts()})
    # walk(update_dropdowns[dropdown_tags != changed], 
    #      ~(.)())
  }
  

### Filtering Functions -----------------------------------------------------

  possible_selectize_choices <- function(column) {
    start_date <- input$dateRange[1]
    end_date   <- input$dateRange[2]
    regions    <- input$regions
    targets    <- input$targets
    countries  <- input$countries
    aircrafts  <- input$aircrafts
    weapons    <- input$weapons
    switch(column, 
           "Target_Country"  = {regions   <- "All"}, 
           "Target_Category" = {targets   <- "All"}, 
           "Unit_Country"    = {countries <- "All"}, 
           "Aircraft_Type"   = {aircrafts <- "All"}, 
           "Weapon_Type"     = {weapons   <- "All"})
    multi_selectors <- list(regions, targets, countries, aircrafts, weapons)
    selectors <- c(list(start_date, end_date), multi_selectors)
    re_name(selectors, c("start_date", "end_date", dropdown_tags))
    result <- c()
    if (all(multi_selectors == "All")) {
      for (tag in war_tags) {
        if ((war_selected[[tag]])()) {
          result <- append(result, levels(war_data[[tag]][[column]]))
        }
      }
    } else {
      for (tag in war_tags) {
        if ((war_selected[[tag]])()) {
          arg_list <- list("war_tag" = tag, "column" = column, "criteria" = selectors)
          result <- append(result, do.call(unique_from_filter, arg_list))
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
  
  filter_selection <- function(war_tag, start_date = earliest_date, end_date = latest_date, regions = "All", targets = "All", countries = "All", aircrafts = "All", weapons = "All") {
    if ("All" %c% regions) {
      if ("All" %c% targets) {
        if ("All" %c% countries) {
          if ("All" %c% aircrafts) {
            if ("All" %c% weapons) {
              war_dt <- war_data[[war_tag]]
            } else {
              war_dt <- war_data[[war_tag]][.(weapons), on = .(Weapon_Type)]
            }
          } else {
            if ("All" %c% weapons) {
              war_dt <- war_data[[war_tag]][.(aircrafts), on = .(Aircraft_Type)]
            } else {
              war_dt <- war_data[[war_tag]][.(aircrafts, weapons), on = .(Aircraft_Type, Weapon_Type)]
            }
          }
        } else {
          if ("All" %c% aircrafts) {
            if ("All" %c% weapons) {
              war_dt <- war_data[[war_tag]][.(countries), on = .(Unit_Country)]
            } else {
              war_dt <- war_data[[war_tag]][.(countries, weapons), on = .(Unit_Country, Weapon_Type)]
            }
          } else {
            if ("All" %c% weapons) {
              war_dt <- war_data[[war_tag]][.(countries, aircrafts), on = .(Unit_Country, Aircraft_Type)]
            } else {
              war_dt <- war_data[[war_tag]][.(countries, aircrafts, weapons), on = .(Unit_Country, Aircraft_Type, Weapon_Type)]
            }
          }
        }
      } else {
        if ("All" %c% countries) {
          if ("All" %c% aircrafts) {
            if ("All" %c% weapons) {
              war_dt <- war_data[[war_tag]][.(targets), on = .(Target_Category)]
            } else {
              war_dt <- war_data[[war_tag]][.(targets, weapons), on = .(Target_Category, Weapon_Type)]
            }
          } else {
            if ("All" %c% weapons) {
              war_dt <- war_data[[war_tag]][.(targets, aircrafts), on = .(Target_Category, Aircraft_Type)]
            } else {
              war_dt <- war_data[[war_tag]][.(targets, aircrafts, weapons), on = .(Target_Category, Aircraft_Type, Weapon_Type)]
            }
          }
        } else {
          if ("All" %c% aircrafts) {
            if ("All" %c% weapons) {
              war_dt <- war_data[[war_tag]][.(targets, countries), on = .(Target_Category, Unit_Country)]
            } else {
              war_dt <- war_data[[war_tag]][.(targets, countries, weapons), on = .(Target_Category, Unit_Country, Weapon_Type)]
            }
          } else {
            if ("All" %c% weapons) {
              war_dt <- war_data[[war_tag]][.(targets, countries, aircrafts), on = .(Target_Category, Unit_Country, Aircraft_Type)]
            } else {
              war_dt <- war_data[[war_tag]][.(targets, countries, aircrafts, weapons), on = .(Target_Category, Unit_Country, Aircraft_Type, Weapon_Type)]
            }
          }
        }
      }
    } else {
      if ("All" %c% targets) {
        if ("All" %c% countries) {
          if ("All" %c% aircrafts) {
            if ("All" %c% weapons) {
              war_dt <- war_data[[war_tag]][.(regions), on = .(Target_Country)]
            } else {
              war_dt <- war_data[[war_tag]][.(regions, weapons), on = .(Target_Country, Weapon_Type)]
            }
          } else {
            if ("All" %c% weapons) {
              war_dt <- war_data[[war_tag]][.(regions, aircrafts), on = .(Target_Country, Aircraft_Type)]
            } else {
              war_dt <- war_data[[war_tag]][.(regions, aircrafts, weapons), on = .(Target_Country, Aircraft_Type, Weapon_Type)]
            }
          }
        } else {
          if ("All" %c% aircrafts) {
            if ("All" %c% weapons) {
              war_dt <- war_data[[war_tag]][.(regions, countries), on = .(Target_Country, Unit_Country)]
            } else {
              war_dt <- war_data[[war_tag]][.(regions, countries, weapons), on = .(Target_Country, Unit_Country, Weapon_Type)]
            }
          } else {
            if ("All" %c% weapons) {
              war_dt <- war_data[[war_tag]][.(regions, countries, aircrafts), on = .(Target_Country, Unit_Country, Aircraft_Type)]
            } else {
              war_dt <- war_data[[war_tag]][.(regions, countries, aircrafts, weapons), on = .(Target_Country, Unit_Country, Aircraft_Type, Weapon_Type)]
            }
          }
        }
      } else {
        if ("All" %c% countries) {
          if ("All" %c% aircrafts) {
            if ("All" %c% weapons) {
              war_dt <- war_data[[war_tag]][.(regions, targets), on = .(Target_Country, Target_Category)]
            } else {
              war_dt <- war_data[[war_tag]][.(regions, targets, weapons), on = .(Target_Country, Target_Category, Weapon_Type)]
            }
          } else {
            if ("All" %c% weapons) {
              war_dt <- war_data[[war_tag]][.(regions, targets, aircrafts), on = .(Target_Country, Target_Category, Aircraft_Type)]
            } else {
              war_dt <- war_data[[war_tag]][.(regions, targets, aircrafts, weapons), on = .(Target_Country, Target_Category, Aircraft_Type, Weapon_Type)]
            }
          }
        } else {
          if ("All" %c% aircrafts) {
            if ("All" %c% weapons) {
              war_dt <- war_data[[war_tag]][.(regions, targets, countries), on = .(Target_Country, Target_Category, Unit_Country)]
            } else {
              war_dt <- war_data[[war_tag]][.(regions, targets, countries, weapons), on = .(Target_Country, Target_Category, Unit_Country, Weapon_Type)]
            }
          } else {
            if ("All" %c% weapons) {
              war_dt <- war_data[[war_tag]][.(regions, targets, countries, aircrafts), on = .(Target_Country, Target_Category, Unit_Country, Aircraft_Type)]
            } else {
              war_dt <- war_data[[war_tag]][.(regions, targets, countries, aircrafts, weapons), on = .(Target_Country, Target_Category, Unit_Country, Aircraft_Type, Weapon_Type)]
            }
          }
        }
      }
    }
    
    if (start_date > war_first_missions[[war_tag]]) {
      if (end_date < war_last_missions[[war_tag]]) {
        war_dt[Mission_Date >= start_date & Mission_Date <= end_date]
      } else {
        war_dt[Mission_Date >= start_date]
      }
    } else {
      if (end_date < war_last_missions[[war_tag]]) {
        war_dt[Mission_Date <= end_date]
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
