# @author Scott Dobbins
# @version 0.9.9.7
# @date 2018-01-11 21:00


if (nrow(WW1_unique) > downsample_size) {
  debug_message("downsampling WW1")
  WW1_original <- copy(WW1_unique)
  WW1_unique <- sample_n(WW1_unique, downsample_size, replace = FALSE)
}
if (nrow(WW2_unique) > downsample_size) {
  debug_message("downsampling WW2")
  WW2_original <- copy(WW2_unique)
  WW2_unique <- sample_n(WW2_unique, downsample_size, replace = FALSE)
}
if (nrow(Korea_unique2) > downsample_size) {
  debug_message("downsampling Korea")
  Korea_original2 <- copy(Korea_unique2)
  Korea_unique2 <- sample_n(Korea_unique2, downsample_size, replace = FALSE)
}
if (nrow(Vietnam_unique) > downsample_size) {
  debug_message("downsampling Vietnam")
  Vietnam_original <- copy(Vietnam_unique)
  Vietnam_unique <- sample_n(Vietnam_unique, downsample_size, replace = FALSE)
}
