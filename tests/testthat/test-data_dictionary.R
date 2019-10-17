test_that("data_dictionary hasn't changed", {
  expect_known_value(
    data_dictionary(), "ref-data_dictionary",
    update = FALSE
  )
})

test_that("data_dictionary has the expected names", {
  expect_named(
    data_dictionary(),
    c("dataset", "column", "typeof", "definition")
  )
})

test_that("data_dictionary defines all its names", {
  dd_definitions <- data_dictionary() %>%
    dplyr::filter(.data$dataset == "data_dictionary")

  expect_equal(nrow(dd_definitions), 4L)

  dd_columns <- sort(dd_definitions$column)
  expect_equal(dd_columns, sort(names(data_dictionary())))
})
