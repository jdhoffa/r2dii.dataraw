# Avoid warning from using toy default_config()
restore <- getOption("r2dii_config")
setup(options(r2dii_config = suppressWarnings(default_config())))
teardown(options(restore))

.do_skip <- TRUE
.update <- FALSE

skip_if_do_skip <- function(msg = "Skipping dataset for speed.",
                            do_skip = .do_skip) {
  if (do_skip) {
    testthat::skip(msg)
  }
}

# For devtools::check() to be consistent with devtools::test()
library(tibble)

library(r2dii.utils)

test_that("DebtMarketClimate is sensitive to config file set globally", {
  skip_if_not(dropbox_exists(), "2dii's dropbox folder doesn't exist.")

  default_config <- suppressWarnings(default_config())
  default <- withr::with_options(list(r2dii_config = default_config), {
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
  expect_known_value(out, "ref-LoanMarketClimate", update = .update)
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
  expect_known_value(out, "ref-LoanMarket", update = .update)
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
  expect_known_value(out, "ref-ALD.BV", update = .update)
})

test_that("BALANCE.SHEET.DATA output is as expected", {
  skip_if_do_skip()
  skip_if_not(dropbox_exists(), "2dii's dropbox folder doesn't exist.")
  out <- BALANCE.SHEET.DATA()
  expect_known_value(out, "ref-BALANCE.SHEET.DATA", update = .update)
})

test_that("EQMarket.Size output is as expected", {
  skip_if_do_skip()
  skip_if_not(dropbox_exists(), "2dii's dropbox folder doesn't exist.")
  out <- EQMarket.Size()
  expect_known_value(out, "ref-EQMarket.Size", update = .update)
})

test_that("DebtMarketClimate output is as expected", {
  skip_if_do_skip()
  skip_if_not(dropbox_exists(), "2dii's dropbox folder doesn't exist.")
  out <- DebtMarketClimate()
  expect_known_value(out, "ref-DebtMarketClimate", update = .update)
})

test_that("DebtMarket output is as expected", {
  skip_if_do_skip()
  skip_if_not(dropbox_exists(), "2dii's dropbox folder doesn't exist.")
  out <- DebtMarket()
  expect_known_value(out, "ref-DebtMarket", update = .update)
})

test_that("SCENLong output is as expected", {
  skip_if_do_skip()
  skip_if_not(dropbox_exists(), "2dii's dropbox folder doesn't exist.")
  out <- SCENLong()
  expect_known_value(out, "ref-SCENLong", update = .update)
})

test_that("SCEN output is as expected", {
  skip_if_do_skip()
  skip_if_not(dropbox_exists(), "2dii's dropbox folder doesn't exist.")
  out <- SCEN()
  expect_known_value(out, "ref-SCEN", update = .update)
})

test_that("SEC.TYPE.BONDS output is as expected", {
  skip_if_do_skip()
  skip_if_not(dropbox_exists(), "2dii's dropbox folder doesn't exist.")
  out <- SEC.TYPE.BONDS()
  expect_known_value(out, "ref-SEC.TYPE.BONDS", update = .update)
})

test_that("TYPE.BONDS output is as expected", {
  skip_if_do_skip()
  skip_if_not(dropbox_exists(), "2dii's dropbox folder doesn't exist.")
  out <- TYPE.BONDS()
  expect_known_value(out, "ref-TYPE.BONDS", update = .update)
})

test_that("TYPE.RECEIPTS output is as expected", {
  skip_if_do_skip()
  skip_if_not(dropbox_exists(), "2dii's dropbox folder doesn't exist.")
  out <- TYPE.RECEIPTS()
  expect_known_value(out, "ref-TYPE.RECEIPTS", update = .update)
})

test_that("Receipts output is as expected", {
  skip_if_do_skip()
  skip_if_not(dropbox_exists(), "2dii's dropbox folder doesn't exist.")
  out <- Receipts()
  expect_known_value(out, "ref-Receipts", update = .update)
})

test_that("CB_OG output is as expected", {
  skip_if_do_skip()
  skip_if_not(dropbox_exists(), "2dii's dropbox folder doesn't exist.")
  out <- CB_OG()
  expect_known_value(out, "ref-CB_OG", update = .update)
})

test_that("EQ_OG output is as expected", {
  skip_if_do_skip()
  skip_if_not(dropbox_exists(), "2dii's dropbox folder doesn't exist.")
  out <- EQ_OG()
  expect_known_value(out, "ref-EQ_OG", update = .update)
})

test_that("Indices output is as expected", {
  skip_if_do_skip()
  skip_if_not(dropbox_exists(), "2dii's dropbox folder doesn't exist.")

  expect_known_value(Indices(), "ref-Indices", update = .update)
})
