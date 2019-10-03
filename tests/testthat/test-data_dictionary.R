library(tibble)

test_that("data_dictionary hasn't changed", {
  expect_known_output(
    data_dictionary, "ref-data_dictionary",
    print = TRUE,
    update = FALSE
  )
})
