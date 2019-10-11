library(tibble)

test_that("data_dictionary hasn't changed", {
  expect_known_value(
    data_dictionary, "ref-data_dictionary",
    update = FALSE
  )
})

test_that("create_data_dictionary outupts the expected tibble", {
  new_data <- tibble::tibble(x = 1, y = "a")
  out <- create_data_dictionary(new_data)

  reference <- tibble::tribble(
    ~dataset,   ~column,   ~definition,
    "new_data",     "x", NA_character_,
    "new_data",     "y", NA_character_
  )
  expect_equal(out, reference)
})
