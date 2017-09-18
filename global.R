# @author Scott Dobbins
# @version 0.9.9
# @date 2017-09-17 18:30


### Import Packages ---------------------------------------------------------

library(shiny)          # app formation
library(shinydashboard) # web display
library(shinyjs)        # dynamic UI display
library(leaflet)        # map source
library(leaflet.extras) # map extras
library(ggplot2)        # plots and graphs
library(assertthat)     # assertions for errors
library(memoise)        # caching
library(dplyr)          # data processing
library(purrr)          # data processing
library(lubridate)      # time processing
library(data.table)     # data processing
# library(plotly)         # pretty interactive graphs
# library(maps)           # also helps with maps
# library(htmltools)      # helps with tooltips
library(DT)             # web tables
# library(rgdal)          # map reading


### Custom Utilities --------------------------------------------------------

# standard personal functions
source('utils.R')


### Global Values -----------------------------------------------------------

# app behavior parameters
source('parameters.R')
if (debug_mode_on) {
  library(testthat)     # unit testing
  library(beepr)        # sound alert
}

# file locations
source('filepaths.R')

# labels for drop-down menus
source('labels.R')


### Global Functions --------------------------------------------------------

# specific helper functions
source('helper.R')

# for embedded videos
youtube_embed <- function(video_code) {
  return (paste0("https://www.youtube.com/embed/", video_code))
}

# for plotting points on overview map
calculate_opacity <- function(sample_number, map_zoom) {
  return (0.1 * bounded(map_zoom + 10 - bounded(log2(sample_number), 0, 10), 1, 10))
}

# for necessity of loading or generating app data
has_bombs_data <- function() {
  return (exists("WW1_bombs") &&
            exists("WW2_bombs") &&
            exists("Korea_bombs2") &&
            exists("Vietnam_bombs"))
}
has_clean_data <- function() {
  return (exists("WW1_clean") &&
            exists("WW2_clean") &&
            exists("Korea_clean2") &&
            exists("Vietnam_clean"))
}


### Parallel ----------------------------------------------------------------

if (use_parallel) {
  library(parallel)
  all_cores <- detectCores(logical = TRUE)
  real_cores_only <- detectCores(logical = FALSE)
  cores <- real_cores_only
  max_useful_cores <- function(col_names, virtual = FALSE) {
    if (virtual) {
      return (min(length(col_names), all_cores))
    } else {
      return (min(length(col_names), virtual_cores_only))
    }
  }
} else {
  cores <- 1L
}
setDTthreads(cores)


### Get Data ----------------------------------------------------------------

if (!has_clean_data()) {
  if (use_compiler) {
    library(compiler)
    enableJIT(3)
  }
  
  if (refresh_data) {
    started.at <- proc.time()
    source('reader.R')
    debug_message(paste0("Read in ", timetaken(started.at)))
    started.at <- proc.time()
    source('cleaner.R')
    debug_message(paste0("Cleaned in ", timetaken(started.at)))
    if (debug_mode_on) {
      started.at <- proc.time()
      source('cleaner_test.R')
      debug_message(paste0("Tested cleaned data in ", timetaken(started.at)))
    }
    started.at <- proc.time()
    source('processor.R')
    debug_message(paste0("Processed in ", timetaken(started.at)))
    if (debug_mode_on) {
      started.at <- proc.time()
      source('processor_test.R')
      debug_message(paste0("Tested processed data in ", timetaken(started.at)))
    }
    
    if (full_write) {
      started.at <- proc.time()
      source('saver.R')
      debug_message(paste0("Saved data in ", timetaken(started.at)))
    }
  } else {
    started.at <- proc.time()
    load(most_recent_save_filepath)
    debug_message(paste0("Loaded in ", timetaken(started.at)))
  }
  
  if (use_compiler) {
    enableJIT(0)
  }
  if (debug_mode_on) beep()
}


### Set Keys ----------------------------------------------------------------

keys <- c("Mission_Date", "Target_Country", "Target_Category", "Unit_Country", "Aircraft_Type", "Weapon_Type")
walk(list(WW1_clean, WW2_clean, Korea_clean1, Korea_clean2, Vietnam_clean), 
     ~setkeyv(., cols = keys))


### Create Samples ----------------------------------------------------------

if (debug_mode_on) {
  WW1_sample <-     sample_n(WW1_clean,     debug_sample_size)
  WW2_sample <-     sample_n(WW2_clean,     debug_sample_size)
  Korea_sample1 <-  sample_n(Korea_clean1,  debug_sample_size)
  Korea_sample2 <-  sample_n(Korea_clean2,  debug_sample_size)
  Vietnam_sample <- sample_n(Vietnam_clean, debug_sample_size)
}


### More Globals ------------------------------------------------------------

# for iteration
war_data <- list(WW1_clean, WW2_clean, Korea_clean2, Vietnam_clean)
re_name(war_data, war_tags)

