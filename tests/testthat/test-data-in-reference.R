library(dplyr)

.update <- FALSE
expect_known_value <- purrr::partial(
  update = .update,
  testthat::expect_known_value
)

test_that("BENCH.REGIONS has no name `BenchmarkRegions`", {
  # https://github.com/2DegreesInvesting/Reference/issues/39 (@2diiKlaus)
  expect_false(rlang::has_name(BENCH.REGIONS(), "BenchmarkRegions"))
})

test_that("BENCH.REGIONS has names `ScenarioGeography` and `reg.count`", {
  # https://github.com/2DegreesInvesting/Reference/issues/39 (@2diiKlaus)
  expect_true(rlang::has_name(BENCH.REGIONS(), "ScenarioGeography"))
  expect_true(rlang::has_name(BENCH.REGIONS(), "reg.count"))
})

test_that("BENCH.REGIONS output is as expected", {
  out <- BENCH.REGIONS()
  expect_is(out, "tbl_df")
  expect_known_value(out, "ref-BENCH.REGIONS")
})

test_that("BENCH.REGIONS at `CountryISO` has no missing values", {
  out <- BENCH.REGIONS()
  na <- dplyr::filter(out, is.na(CountryISO))
  expect_equal(nrow(na), 0L)
})

test_that("BENCH.REGIONS has no missing values in the `Country` column", {
  n_na <- BENCH.REGIONS() %>%
    filter(is.na(Country)) %>%
    nrow()

  expect_equal(n_na, 0L)
})

test_that("INDEX.REGIONS output is as expected", {
  out <- INDEX.REGIONS()
  expect_is(out, "tbl_df")
  expect_known_value(out, "ref-INDEX.REGIONS")
})

test_that("INDEX.REGIONS has no missing values in the `Country` column", {
  n_na <- INDEX.REGIONS() %>%
    filter(is.na(Country)) %>%
    nrow()

  expect_equal(n_na, 0L)
})

test_that("sector.bridge output is as expected", {
  out <- sector.bridge()
  expect_is(out, "tbl_df")
  expect_known_value(out, "ref-sector.bridge")
})

test_that("SectorBridge output is as expected", {
  out <- SectorBridge()
  expect_is(out, "tbl_df")
  expect_known_value(out, "ref-SectorBridge")
})

test_that("BicsSectorBridge output is as expected", {
  out <- BicsSectorBridge()
  expect_is(out, "tbl_df")
  expect_known_value(out, "ref-BicsSectorBridge")
})

test_that("RevenueSplit output is as expected", {
  out <- RevenueSplit()
  expect_is(out, "tbl_df")
  expect_known_value(out, "ref-RevenueSplit")
})

test_that("path_reference outputs an existing directory", {
  expect_true(fs::dir_exists(path_reference()))
})

test_that("TYPE.EQUITY output is as expected", {
  out <- TYPE.EQUITY()
  expect_is(out, "character")
  expect_known_value(out, "ref-TYPE.EQUITY")
})

test_that("GROUPS.GOVT output is as expected", {
  out <- GROUPS.GOVT()
  expect_is(out, "character")
  expect_known_value(out, "ref-GROUPS.GOVT")
})

test_that("TYPE.OTHERS output is as expected", {
  expect_is(TYPE.OTHERS(), "character")
})
