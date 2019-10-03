library(glue)
library(rlang)
library(readr)

enframe_dictionary <- function(dataset, package = NULL) {
  package <- package %||% fs::path_file(usethis::proj_get())
  stopifnot(is.character(dataset), is.character(package))

  dataset_ <- get(dataset, envir = as.environment(glue("package:{package}")))
  tibble::tibble(
    dataset = dataset,
    column = names(dataset_),
    definition = NA_character_
  )
}

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
