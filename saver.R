# @author Scott Dobbins
# @version 0.9.9.7
# @date 2018-01-13 21:00


### Save Data ---------------------------------------------------------------

debug_message("saving cleaned bomb data")
for (war_data_tag in war_data_tags) {
  fwrite(bomb_data[[war_data_tag]],
         file = war_bombs_filepath[[war_data_tag]],
         quote = TRUE)
}

debug_message("saving processed clean data")
for (war_data_tag in war_data_tags) {
  fwrite(clean_data[[war_data_tag]], 
         file = war_clean_filepath[[war_data_tag]], 
         quote = TRUE)
}


### Adjustments -------------------------------------------------------------

debug_message("saving workspace")
save.image(file = full_save_filepath)


### Sample and Resave -------------------------------------------------------

if (downsample) {
  source('downsampler.R')
  # pipeR::`%>>%`, which is set by utils_general.R to have the name `%>%`, 
  # doesn't work on shinyapps.io, so it should be removed before the workspace is saved
  rm(`%>%`)
  debug_message("saving workspace with downsamples")
  save.image(file = save_filepath)
}
