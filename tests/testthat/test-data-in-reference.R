library(dplyr)

.update <- FALSE

test_that("BENCH.REGIONS has no name `BenchmarkRegions`", {
  skip_if_not(dropbox_exists(), "2dii's dropbox folder doesn't exist.")

  # https://github.com/2DegreesInvesting/Reference/issues/39 (@2diiKlaus)
  expect_false(rlang::has_name(BENCH.REGIONS(), "BenchmarkRegions"))
})

test_that("BENCH.REGIONS has names `ScenarioGeography` and `reg.count`", {
  skip_if_not(dropbox_exists(), "2dii's dropbox folder doesn't exist.")

  # https://github.com/2DegreesInvesting/Reference/issues/39 (@2diiKlaus)
  expect_true(rlang::has_name(BENCH.REGIONS(), "ScenarioGeography"))
  expect_true(rlang::has_name(BENCH.REGIONS(), "reg.count"))
})

test_that("BENCH.REGIONS output is as expected", {
  skip_if_not(dropbox_exists(), "2dii's dropbox folder doesn't exist.")

  out <- BENCH.REGIONS()
  expect_is(out, "tbl_df")
  expect_known_value(out, "ref-BENCH.REGIONS", update = .update)
})

test_that("BENCH.REGIONS at `CountryISO` has no missing values", {
  skip_if_not(dropbox_exists(), "2dii's dropbox folder doesn't exist.")

  out <- BENCH.REGIONS()
  na <- dplyr::filter(out, is.na(CountryISO))
  expect_equal(nrow(na), 0L)
})

test_that("BENCH.REGIONS has no missing values in the `Country` column", {
  skip_if_not(dropbox_exists(), "2dii's dropbox folder doesn't exist.")

  n_na <- BENCH.REGIONS() %>%
    filter(is.na(Country)) %>%
    nrow()

  expect_equal(n_na, 0L)
})

test_that("INDEX.REGIONS output is as expected", {
  skip_if_not(dropbox_exists(), "2dii's dropbox folder doesn't exist.")

  out <- INDEX.REGIONS()
  expect_is(out, "tbl_df")
  expect_known_value(out, "ref-INDEX.REGIONS", update = .update)
})

test_that("INDEX.REGIONS has no missing values in the `Country` column", {
  skip_if_not(dropbox_exists(), "2dii's dropbox folder doesn't exist.")

  n_na <- INDEX.REGIONS() %>%
    filter(is.na(Country)) %>%
    nrow()

  expect_equal(n_na, 0L)
})

test_that("sector.bridge output is as expected", {
  skip_if_not(dropbox_exists(), "2dii's dropbox folder doesn't exist.")

  out <- sector.bridge()
  expect_is(out, "tbl_df")
  expect_known_value(out, "ref-sector.bridge", update = .update)
})

test_that("SectorBridge output is as expected", {
  skip_if_not(dropbox_exists(), "2dii's dropbox folder doesn't exist.")

  out <- SectorBridge()
  expect_is(out, "tbl_df")
  expect_known_value(out, "ref-SectorBridge", update = .update)
})

test_that("BicsSectorBridge output is as expected", {
  skip_if_not(dropbox_exists(), "2dii's dropbox folder doesn't exist.")

  out <- BicsSectorBridge()
  expect_is(out, "tbl_df")
  expect_known_value(out, "ref-BicsSectorBridge", update = .update)
})

test_that("RevenueSplit output is as expected", {
  skip_if_not(dropbox_exists(), "2dii's dropbox folder doesn't exist.")

  out <- RevenueSplit()
  expect_is(out, "tbl_df")
  expect_known_value(out, "ref-RevenueSplit", update = .update)
})

test_that("path_reference outputs an existing directory", {
  skip_if_not(dropbox_exists(), "2dii's dropbox folder doesn't exist.")
  expect_true(fs::dir_exists(path_reference()))
})

test_that("TYPE.EQUITY output is as expected", {
  out <- TYPE.EQUITY()
  expect_is(out, "character")
  expect_known_value(out, "ref-TYPE.EQUITY", update = .update)
})

test_that("GROUPS.GOVT output is as expected", {
  out <- GROUPS.GOVT()
  expect_is(out, "character")
  expect_known_value(out, "ref-GROUPS.GOVT", update = .update)
})

test_that("TYPE.OTHERS output is as expected", {
  expect_is(TYPE.OTHERS(), "character")
})
