library(glue)
library(rlang)
library(readr)

data_dictionary <- tibble::tribble(
  ~dataset,          ~column,      ~definition,
  "data_dictionary", "dataset",    "The name of a dataset",
  "data_dictionary", "column",     "The name of a dataset-column",
  "data_dictionary", "definition", "The definition of a dataset-column"
)

#  Edited manually
path <- here::here("data-raw", "data_dictionary_loanbook.csv")
all_chr <- cols(
  dataset = col_character(),
  column = col_character(),
  definition = col_character()
)
dic_loanbook_demo <- read_csv(path, col_types = all_chr)

data_dictionary <- dplyr::bind_rows(data_dictionary, dic_loanbook_demo)
use_data(data_dictionary, overwrite = TRUE)
