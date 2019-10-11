library(tibble)

test_that("data_dictionary hasn't changed", {
  expect_known_value(
    data_dictionary, "ref-data_dictionary",
    update = FALSE
  )
})

test_that("create_data_dictionary outputs a tibble", {
  out <- create_data_dictionary("data_dictionary", package = "r2dii.dataraw")
  expect_is(out, "tbl_df")
  expect_named(out, names(data_dictionary))
})
