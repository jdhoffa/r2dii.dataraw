# TODO: DRY with r2dii.utils::check_crucial_names()
check_crucial_names <- function(x, expected_names) {
  stopifnot(rlang::is_named(x))
  stopifnot(is.character(expected_names))

  ok <- all(expected_names %in% names(x))
  if (ok) {
    return(invisible(x))
  }

  rlang::abort(glue::glue(
    "The data must have all expected names:
    Actual: {usethis::ui_field(sort(names(x)))}
    Expected: {usethis::ui_field(sort(expected_names))}"
  ))
}

test_that("all classification data has minimim expected names", {

  exported_data <- function(package) {
    utils::data(package = package)$results[, "Item"]
  }

  ends_with_classification <- grep(
    pattern = "_classification$",
    x = exported_data("r2dii.dataraw"),
    value = TRUE
  )

  classification_list <- ends_with_classification %>%
    purrr::map(~get(.x, envir = as.environment("package:r2dii.dataraw"))) %>%
    purrr::set_names(ends_with_classification)

  # https://github.com/2DegreesInvesting/r2dii.match/issues/7
  crucial <- c("code", "sector", "borderline")
  expect_error(
    purrr::walk(classification_list, ~check_crucial_names(.x, crucial)),
    NA
  )
})
