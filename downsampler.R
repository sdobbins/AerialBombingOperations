# @author Scott Dobbins
# @version 0.9.9.7
# @date 2018-01-13 15:00


if (nrow(WW1_clean) > downsample_size) {
  debug_message("downsampling WW1")
  WW1_clean <- sample_n(WW1_clean, downsample_size, replace = FALSE)
}
if (nrow(WW2_clean) > downsample_size) {
  debug_message("downsampling WW2")
  WW2_clean <- sample_n(WW2_clean, downsample_size, replace = FALSE)
}
if (nrow(Korea_clean2) > downsample_size) {
  debug_message("downsampling Korea")
  Korea_clean2 <- sample_n(Korea_clean2, downsample_size, replace = FALSE)
}
if (nrow(Vietnam_clean) > downsample_size) {
  debug_message("downsampling Vietnam")
  Vietnam_clean <- sample_n(Vietnam_clean, downsample_size, replace = FALSE)
}
