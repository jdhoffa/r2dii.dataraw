.do_skip <- TRUE
.update <- FALSE

skip_if_do_skip <- function(msg = "Skipping dataset for speed.",
                            do_skip = .do_skip) {
  if (do_skip) {
    testthat::skip(msg)
  }
}

test_that("PHYSICAL.RISK.EQ has the expected structure", {
  skip_if_not(dropbox_exists(), "2dii's dropbox folder doesn't exist.")
  skip_if_do_skip()
  out <- PHYSICAL.RISK.EQ()
  expect_known_value(out, "ref-PHYSICAL.RISK.EQ", update = .update)
})

test_that("PHYSICAL.RISK.CB has the expected structure", {
  skip_if_not(dropbox_exists(), "2dii's dropbox folder doesn't exist.")
  skip_if_do_skip()
  out <- PHYSICAL.RISK.CB()
  expect_known_value(out, "ref-PHYSICAL.RISK.CB", update = .update)
})

test_that("FIN.DATA output is as expected", {
  skip_if_not(dropbox_exists(), "2dii's dropbox folder doesn't exist.")
  skip_if_do_skip()
  out <- FIN.DATA()
  expect_known_value(out, "ref-FIN.DATA", update = .update)
})

test_that("ALD.SPV output is as expected", {
  skip_if_not(dropbox_exists(), "2dii's dropbox folder doesn't exist.")
  skip_if_do_skip()
  out <- ALD.SPV()
  expect_known_value(out, "ref-ALD.SPV", update = .update)
})

test_that("Fund.Data output is as expected", {
  skip_if_not(dropbox_exists(), "2dii's dropbox folder doesn't exist.")
  skip_if_do_skip()
  out <- Fund.Data()
  expect_known_value(out, "ref-Fund.Data", update = .update)
})

test_that("ALD.EQ output is as expected", {
  skip_if_not(dropbox_exists(), "2dii's dropbox folder doesn't exist.")
  skip_if_do_skip()
  out <- ALD.EQ()
  expect_known_value(out, "ref-ALD.EQ", update = .update)
})

test_that("ALD.CC output is as expected", {
  skip_if_not(dropbox_exists(), "2dii's dropbox folder doesn't exist.")
  skip_if_do_skip()
  out <- ALD.CC()
  expect_known_value(out, "ref-ALD.CC", update = .update)
})

test_that("FundsTrusts output is as expected", {
  skip_if_not(dropbox_exists(), "2dii's dropbox folder doesn't exist.")
  skip_if_do_skip()
  out <- FundsTrusts()
  expect_known_value(out, "ref-FundsTrusts", update = .update)
})

test_that("ALD.CB output is as expected", {
  skip_if_not(dropbox_exists(), "2dii's dropbox folder doesn't exist.")
  skip_if_do_skip()
  out <- ALD.CB()
  expect_known_value(out, "ref-ALD.CB", update = .update)
})
