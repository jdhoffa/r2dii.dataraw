library(tibble)

test_that("data_dictionary hasn't changed", {
  expect_known_value(
    data_dictionary, "ref-data_dictionary",
    update = FALSE
  )
})
