.do_skip <- TRUE
.update <- FALSE

skip_if_do_skip <- function(msg = "Skipping dataset for speed.",
                            do_skip = .do_skip) {
  if (do_skip) {
    testthat::skip(msg)
  }
}

expect_known_output <- purrr::partial(
  update = .update,
  print = TRUE,
  testthat::expect_known_output
)

# For devtools::check() to be consistent with devtools::test()
library(tibble)

library(r2dii.utils)

test_that("DebtMarketClimate is sensitive to config file set globally", {
  skip_if_not(dropbox_exists(), "2dii's dropbox folder doesn't exist.")

  default <- withr::with_options(list(r2dii_config = default_config()), {
    DebtMarketClimate()
  })
  config <- example_config("config-toy.yml")
  example <- withr::with_options(list(r2dii_config = config), {
    DebtMarketClimate()
  })

  expect_is(example, "data.frame")
  expect_false(identical(default, example))
})

test_that("LoanMarketClimate errs nicely if the relevant file doesn't exist", {
  bad_config <- list(
    r2dii_config = example_config("config-toy.yml")
  )

  expect_error(
    withr::with_options(bad_config, LoanMarketClimate()),
    "Can't find"
  )
})

test_that("LoanMarketClimate output is as expected", {
  skip_if_do_skip()
  skip_if_not(dropbox_exists(), "2dii's dropbox folder doesn't exist.")

  # Quarter 2, 2019 contains the relevant data
  options_2019Q2 <- list(
    r2dii_config = example_config("config_2019Q2.yml")
  )
  out <- withr::with_options(options_2019Q2, LoanMarketClimate())

  expect_is(out, "data.frame")
  expect_known_output(out, "ref-LoanMarketClimate")
})

test_that("LoanMarket output is as expected", {
  skip_if_do_skip()
  skip_if_not(dropbox_exists(), "2dii's dropbox folder doesn't exist.")

  # Quarter 2, 2019 contains the relevant data
  options_2019Q2 <- list(
    r2dii_config = example_config("config_2019Q2.yml")
  )
  out <- withr::with_options(options_2019Q2, LoanMarket())

  expect_is(out, "data.frame")
  expect_known_output(out, "ref-LoanMarket")
})

test_that("SEC.TYPE.BONDS output is as expected", {
  skip_if_do_skip()
  skip_if_not(dropbox_exists(), "2dii's dropbox folder doesn't exist.")
  expect_is(SEC.TYPE.BONDS(), "character")
})

test_that("TYPE.BONDS output is as expected", {
  skip_if_do_skip()
  skip_if_not(dropbox_exists(), "2dii's dropbox folder doesn't exist.")
  expect_is(TYPE.BONDS(), "character")
})

test_that("ALD.BV output is as expected", {
  skip_if_do_skip()
  skip_if_not(dropbox_exists(), "2dii's dropbox folder doesn't exist.")
  out <- ALD.BV()
  expect_known_output(out, "ref-ALD.BV")
})

test_that("BALANCE.SHEET.DATA output is as expected", {
  skip_if_do_skip()
  skip_if_not(dropbox_exists(), "2dii's dropbox folder doesn't exist.")
  out <- BALANCE.SHEET.DATA()
  expect_known_output(out, "ref-BALANCE.SHEET.DATA")
})

test_that("EQMarket.Size output is as expected", {
  skip_if_do_skip()
  skip_if_not(dropbox_exists(), "2dii's dropbox folder doesn't exist.")
  out <- EQMarket.Size()
  expect_known_output(out, "ref-EQMarket.Size")
})

test_that("DebtMarketClimate output is as expected", {
  skip_if_do_skip()
  skip_if_not(dropbox_exists(), "2dii's dropbox folder doesn't exist.")
  out <- DebtMarketClimate()
  expect_known_output(out, "ref-DebtMarketClimate")
})

test_that("DebtMarket output is as expected", {
  skip_if_do_skip()
  skip_if_not(dropbox_exists(), "2dii's dropbox folder doesn't exist.")
  out <- DebtMarket()
  expect_known_output(out, "ref-DebtMarket")
})

test_that("SCENLong output is as expected", {
  skip_if_do_skip()
  skip_if_not(dropbox_exists(), "2dii's dropbox folder doesn't exist.")
  out <- SCENLong()
  expect_known_output(out, "ref-SCENLong")
})

test_that("SCEN output is as expected", {
  skip_if_do_skip()
  skip_if_not(dropbox_exists(), "2dii's dropbox folder doesn't exist.")
  out <- SCEN()
  expect_known_output(out, "ref-SCEN")
})

test_that("SEC.TYPE.BONDS output is as expected", {
  skip_if_do_skip()
  skip_if_not(dropbox_exists(), "2dii's dropbox folder doesn't exist.")
  out <- SEC.TYPE.BONDS()
  expect_known_output(out, "ref-SEC.TYPE.BONDS")
})

test_that("TYPE.BONDS output is as expected", {
  skip_if_do_skip()
  skip_if_not(dropbox_exists(), "2dii's dropbox folder doesn't exist.")
  out <- TYPE.BONDS()
  expect_known_output(out, "ref-TYPE.BONDS")
})

test_that("TYPE.RECEIPTS output is as expected", {
  skip_if_do_skip()
  skip_if_not(dropbox_exists(), "2dii's dropbox folder doesn't exist.")
  out <- TYPE.RECEIPTS()
  expect_known_output(out, "ref-TYPE.RECEIPTS")
})

test_that("Receipts output is as expected", {
  skip_if_do_skip()
  skip_if_not(dropbox_exists(), "2dii's dropbox folder doesn't exist.")
  out <- Receipts()
  expect_known_output(out, "ref-Receipts")
})

test_that("CB_OG output is as expected", {
  skip_if_do_skip()
  skip_if_not(dropbox_exists(), "2dii's dropbox folder doesn't exist.")
  out <- CB_OG()
  expect_known_output(out, "ref-CB_OG")
})

test_that("EQ_OG output is as expected", {
  skip_if_do_skip()
  skip_if_not(dropbox_exists(), "2dii's dropbox folder doesn't exist.")
  out <- EQ_OG()
  expect_known_output(out, "ref-EQ_OG")
})

test_that("Indices output is as expected", {
  skip_if_do_skip()
  skip_if_not(dropbox_exists(), "2dii's dropbox folder doesn't exist.")
  out <- Indices()
  expect_known_output(out, "ref-Indices")
})


