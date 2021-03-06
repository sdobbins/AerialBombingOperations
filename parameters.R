# @author Scott Dobbins
# @version 0.9.9.7
# @date 2018-01-13 21:00


### Developer Control -------------------------------------------------------

if (!for_publication) {
  # debug control
  debug_mode_on <- TRUE
  
  # data refresh
  refresh_data <- TRUE
  full_write <- TRUE
} else {
  # publication settings
  debug_mode_on <- FALSE
  refresh_data <- FALSE
  full_write <- FALSE
}

# JIT compiler settings
use_compiler <- TRUE

# parallel settings
use_parallel <- TRUE


### cleaning data
coord_buffer <- 0.1
coord_buffer_tight <- 0.001
lat_long_error_threshold_tight <- round(sqrt(2) / 20 - 0.00005, digits = 4L)
lat_long_error_threshold <- 0.5


### writing data
downsample <- TRUE
downsample_size <- 2e5


### unit testing
max_string_length <- 4096L


### processing data
near_tolerance <- 0.1
empty_text <- "unspecified"


### Appearance Modifiers ----------------------------------------------------

# sizes
sidebar_width <- 240
title_width <- 360
map_height <- 640
map_width <- 1024
graph_height <- "600px"
video_height <- 480
video_width <- 640
image_height <- 768
image_width <- 1024

# colors (on map)
WW1_color <- 'darkblue'
WW2_color <- 'darkred'
Korea_color <- 'yellow'
Vietnam_color <- 'darkgreen'

# background colors (in DataTable)
WW1_background <- 'skyblue'
WW2_background <- 'indianred'
Korea_background <- 'khaki'
Vietnam_background <- 'olivedrab'
example_background <- 'snow'
font_weight <- 'bold'

# map parameters
point_weight <- 5
point_fill <- TRUE
civilian_blur <- 20
civilian_max <- 0.05
civilian_radius <- 15

# graph parameters
min_breaks <- 6L
max_breaks <- 12L
coord_flip_threshold <- 12L
subset_graph_threshold <- 48L
stat_count_cutoff <- 3L
stat_prop_cutoff <- 0.001
stat_rank_cutoff <- subset_graph_threshold
mosaic_prop_cutoff <- 0.01
mosaic_offset <- 0.005
# high_cutoff <- 5L
# low_cutoff <- 5L
# boxplot_width <- 0.2

# animation parameters
animation_delays <- c("year" = 1.5, "month" = 1, "week" = 0.5)

# usage keys
HERE_id <- '5LPi1Hu7Aomn8Nv4If6c'
HERE_code <- 'mrmfvq4OREjya6Vbjmw6Gw'

# images
sidebar_image <- "https://yt3.ggpht.com/-04uuTMHfDz4/AAAAAAAAAAI/AAAAAAAAAAA/Kjeupp-eNNg/s100-c-k-no-rj-c0xffffff/photo.jpg"

# sampling parameters
init_sample_size <- 1024
min_sample_size <- 1
max_sample_size <- 4096

# bin size parameters
WW1_min_bins <- 4
WW1_init_bins <- 30
WW1_max_bins <- 48
WW2_min_bins <- 7
WW2_init_bins <- 30
WW2_max_bins <- 84
Korea_min_bins <- 4
Korea_init_bins <- 30
Korea_max_bins <- 48
Vietnam_min_bins <- 4
Vietnam_init_bins <- 30
Vietnam_max_bins <- 240


### Dates -------------------------------------------------------------------

# historical
WW1_start_date <- as.Date("1914-07-28")
WW1_end_date   <- as.Date("1918-11-11")

WW2_start_date <- as.Date("1939-09-01")
WW2_end_date   <- as.Date("1945-09-02")

Korea_start_date <- as.Date("1950-06-25")
Korea_end_date   <- as.Date("1953-07-27")

Vietnam_start_date <- as.Date("1955-11-01")
Vietnam_end_date   <- as.Date("1975-04-30")

# records
WW1_first_mission <- as.Date("1915-05-26")
WW1_last_mission  <- as.Date("1918-11-10")

WW2_first_mission <- as.Date("1939-09-03")
WW2_last_mission  <- as.Date("1945-12-31")

Korea_first_mission <- as.Date("1950-06-26")
Korea_last_mission  <- as.Date("1952-12-31")

Vietnam_first_mission <- as.Date("1965-06-01")
Vietnam_last_mission  <- as.Date("1975-06-30")

# for app
earliest_date <- floor_date(min(WW1_start_date, WW1_first_mission), "year")
latest_date <- ceiling_date(max(Vietnam_end_date, Vietnam_last_mission), "year")


### Historical Data ---------------------------------------------------------

WW1_altitude_max_feet <- 20000L
WW2_altitude_max_feet <- 40000L
Korea_altitude_max_feet <- 40000L
Vietnam_altitude_max_feet <- 70000L
