#' Ready csv fieles in a way that is compatible with legacy code
#'
#' TODO? See https://github.com/2DegreesInvesting/Reference/issues/36
#' @noRd
read_csv_ <- function(x, ...) {
  check_path(x)

  out <- utils::read.csv(x, strip.white = TRUE, stringsAsFactors = FALSE, ...)
  tibble::as_tibble(out)
}

check_path <- function(path) {
  if(!fs::file_exists(path)) {
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

#' Create a coverage object
#'
#' This function is a kind of hack to report coverage. The usual approach with
#' `covr::package_coverage()` didn't work as expected; it ignored all tests and
#' returned 0% coverage. Similar problems has been reported before and
#' eventually I may find a solution. Until then, this is a workaround works
#' well. It's buggy and won't pass tests but I don't bother for now because it's
#' only for developers and for interactive use.
#'
#' @param exclude String giving patterns to match files to exclude.
#'
#' @return An object of class coverage.
#'
#' @examples
#' # Takes a few seconds to run
#' \dontrun{
#' result <- pkg_coverage(exclude = c("dataset[.]R", "dataset-large[.]R"))
#' result <- pkg_coverage(exclude = "dataset.*")
#' covr::percent_coverage(result)
#' }
#' @noRd
pkg_coverage <- function(exclude = "dataset") {
  source_ <- sort(pick_files(exclude))
  tests <- as_test(source_)
  covr::file_coverage(source_, tests)
}

pick_files <- function(exclude = NULL) {
  files <- find_source_files()

  if (!is.null(exclude)) {
    exclude_positions <- unlist(purrr::map(exclude, ~ grep(.x, files)))
    files <- files[-exclude_positions]
  }

  if (length(files) < 1) {
    rlang::warn("Nothing to exclude.")
  }

  files
}

find_files_with_tests <- function() {
  in_tests <- fs::dir_ls(testthat::test_path(), regexp = "test-.*[.]R$")
  in_tests %>%
    stringr::str_replace("tests/testthat", "R") %>%
    stringr::str_replace("test-", "")
}

find_source_files <- function() {
  in_r <- fs::dir_ls("R", regexp = "[.]R$")
  in_r[in_r %in% find_files_with_tests()]
}

as_test <- function(x) {
  file_ <- glue::glue("test-{fs::path_file(x)}")
  testthat::test_path(fs::path_file(file_))
}

#' Move html files from docs/ to inst/docs then remove docs/
#'
#' This function is useful to add small changes to the site in inst by running a
#' pkgdown function (e.g. `build_news()`) then moving the result from docs/ to
#' inst/docs/.
#' @noRd
move_html_to_inst <- function() {
  files <- fs::dir_ls("docs", recurse = TRUE, regexp = "[.]html$")
  destfile <- fs::path("inst", files)
  fs::file_move(files, destfile)
  fs::dir_delete("docs")
}

