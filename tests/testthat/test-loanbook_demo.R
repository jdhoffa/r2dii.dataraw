library(tibble)

test_that("loanbook_demo hasn't changed", {
  expect_known_output(
    loanbook_demo, "ref-loanbook_demo",
    print = TRUE,
    update = FALSE
  )
})
