# Source withr::with_options
with_options <- function (new, code) {
  old <- set_options(new_options = new)
  on.exit(set_options(old))
  force(code)
}

# Source withr::with_options
set_options <- function (new_options) {
  do.call(options, as.list(new_options))
}

#' Ready csv fieles in a way that is compatible with legacy code
#'
#' TODO? See https://github.com/2DegreesInvesting/Reference/issues/36
#' @noRd
read_csv_ <- function(x, ...) {
  check_path(x)

  out <- utils::read.csv(x, strip.white = TRUE, stringsAsFactors = FALSE, ...)
  as_tibble(out)
}

check_path <- function(path) {
  if (!fs::file_exists(path)) {
    abort(glue(
      "Can't find {ui_path(path)}.
      There is no such file or directory."
    ))
  }

  invisible(path)
}

read_datastore <- function(path) {
  read_csv_(DATA.STORE.PATH(path))
}

#' A checklist to add a new dataset
#'
#' @return Printed output
#'
#' @examples
#' use_newdata_checklist()
#' @noRd
use_newdata_checklist <- function() {
  cat(glue("Checklist to add a new dataset called {ui_code('newdata')}:\n\n"))

  ui_todo(glue("Add {ui_path('data-raw/newdata.csv')}"))

  ui_todo(glue("Add {ui_path('inst/extdata/data_dictionary_newdata.csv')}"))

  ui_todo("In {ui_path('data-raw/newdata.R')}, do something like this:")
  cat(glue("

      # Source: @<contributor> <URL to issue or pull request>
      newdata <- readr::read_csv(here::here('data-raw', 'newdata.csv'))
      use_data(newdata, overwrite = TRUE)


  "))

  ui_todo("In {ui_path('R/newdata.R')}, document {ui_code('newdata')}")

  ui_todo(
    "In {ui_path('test/testthat/test-newdata.R')}, add a regression test for \\
    {ui_code('newdata')}"
  )

  ui_todo(
    "In {ui_path('R/data_dictionary.R')}, \\
    extend {ui_code('data_dictionary()')} to use {ui_code('newdata')}"
  )

  ui_todo(
    "In {ui_path('test/testthat/test-data_dictionary.R')}, \\
    add at least one test for {ui_code('newdata')}"
  )

  ui_todo(
    "In {ui_path('test/testthat/test-data_dictionary.R')}, \\
    update {ui_code('expect_known_value()')}"
  )
}

