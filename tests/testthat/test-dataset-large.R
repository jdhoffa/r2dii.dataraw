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

test_that("PHYSICAL.RISK.EQ has the expected structure", {
  skip_if_not(dropbox_exists(), "2dii's dropbox folder doesn't exist.")
  skip_if_do_skip()
  out <- PHYSICAL.RISK.EQ()
  expect_known_output(out, "ref-PHYSICAL.RISK.EQ")
})

test_that("PHYSICAL.RISK.CB has the expected structure", {
  skip_if_not(dropbox_exists(), "2dii's dropbox folder doesn't exist.")
  skip_if_do_skip()
  out <- PHYSICAL.RISK.CB()
  expect_known_output(out, "ref-PHYSICAL.RISK.CB")
})

test_that("FIN.DATA output is as expected", {
  skip_if_not(dropbox_exists(), "2dii's dropbox folder doesn't exist.")
  skip_if_do_skip()
  out <- FIN.DATA()
  expect_known_output(out, "ref-FIN.DATA")
})

test_that("ALD.SPV output is as expected", {
  skip_if_not(dropbox_exists(), "2dii's dropbox folder doesn't exist.")
  skip_if_do_skip()
  out <- ALD.SPV()
  expect_known_output(out, "ref-ALD.SPV")
})

test_that("Fund.Data output is as expected", {
  skip_if_not(dropbox_exists(), "2dii's dropbox folder doesn't exist.")
  skip_if_do_skip()
  out <- Fund.Data()
  expect_known_output(out, "ref-Fund.Data")
})

test_that("ALD.Company output is as expected", {
  skip_if_not(dropbox_exists(), "2dii's dropbox folder doesn't exist.")
  skip_if_do_skip()
  out <- ALD.Company()

  expect_known_output(out, "ref-ALD.Company")
})

test_that("ALD.EQ output is as expected", {
  skip_if_not(dropbox_exists(), "2dii's dropbox folder doesn't exist.")
  skip_if_do_skip()
  out <- ALD.EQ()
  expect_known_output(out, "ref-ALD.EQ")
})

test_that("ALD.CC output is as expected", {
  skip_if_not(dropbox_exists(), "2dii's dropbox folder doesn't exist.")
  skip_if_do_skip()
  out <- ALD.CC()
  expect_known_output(out, "ref-ALD.CC")
})

test_that("FundsTrusts output is as expected", {
  skip_if_not(dropbox_exists(), "2dii's dropbox folder doesn't exist.")
  skip_if_do_skip()
  out <- FundsTrusts()
  expect_known_output(out, "ref-FundsTrusts")
})

test_that("ALD.CB output is as expected", {
  skip_if_not(dropbox_exists(), "2dii's dropbox folder doesn't exist.")
  skip_if_do_skip()
  out <- ALD.CB()
  expect_known_output(out, "ref-ALD.CB")
})

