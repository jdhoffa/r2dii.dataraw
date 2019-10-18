# Source: @jdhoffa https://github.com/2DegreesInvesting/r2dii.dataraw/pull/8
name_reductions <-
  readr::read_csv(here::here("data-raw", "name_reductions.csv"))
use_data(name_reductions, overwrite = TRUE)
