config_demo <- function() {
  testthat::test_path("config_demo.yml")
}

test_that("take_snapshot saves BENCH.REGIONS as a tibble", {
  skip_if_not(dropbox_exists(), "2dii's dropbox folder doesn't exist.")

  # FIXME: Should use config_demo() instead, but if I do so, config
  # fails to copy. Not sure what's going on
  withr::local_options(
    list(r2dii_config = r2dii.utils::example_config()[[2]])
  )

  temp_root <- fs::path(tempdir(), "newdir")
  fs::dir_create(temp_root)
  withr::local_dir(temp_root)

  take_snapshot("BENCH.REGIONS")
  path <- fs::path(temp_root, "BENCH.REGIONS.csv")
  expect_is(readr::read_csv(path), "tbl_df")
})

test_that("take_snapshot defaults to write snapshots in working directory", {
  skip_if_not(dropbox_exists(), "2dii's dropbox folder doesn't exist.")

  # FIXME: Should use config_demo() instead, but if I do so, config
  # fails to copy. Not sure what's going on
  withr::local_options(
    list(r2dii_config = r2dii.utils::example_config()[[2]])
  )

  temp_root <- fs::path(tempdir(), "newdir")
  fs::dir_create(temp_root)
  withr::local_dir(temp_root)

  take_snapshot("BENCH.REGIONS")

  snapshots_written_in_root <- fs::dir_ls(temp_root, regexp = "BENCH.REGIONS")
  expect_length(snapshots_written_in_root, 1L)
})

test_that("take_snapshot creates a destdir if it doesn't exist", {
  skip_if_not(dropbox_exists(), "2dii's dropbox folder doesn't exist.")

  destdir <- fs::path(tempdir(), "newdir")
  take_snapshot(
    "BENCH.REGIONS",
    destdir = destdir,
    config = suppressWarnings(get_config())
  )
  expect_true(fs::dir_exists(destdir))
  out <- fs::dir_ls(destdir, regexp = "BENCH.REGIONS")
  expect_length(out, 1L)
})

test_that("take_snapshot saves an exported dataset to a new `destdir`", {
  skip_if_not(dropbox_exists(), "2dii's dropbox folder doesn't exist.")

  destdir <- fs::path(tempdir(), "newdir")
  take_snapshot(
    "BENCH.REGIONS",
    destdir = destdir,
    config = suppressWarnings(get_config())
  )
  expect_true(fs::dir_exists(destdir))
  out <- fs::dir_ls(destdir, regexp = "BENCH.REGIONS")
  expect_length(out, 1L)
})

test_that("take_snapshot saves an exported dataset to a new `destdir`", {
  skip_if_not(dropbox_exists(), "2dii's dropbox folder doesn't exist.")

  destdir <- fs::path(tempdir(), "newdir")
  expect_warning(
    take_snapshot(
      "SEC.TYPE.BONDS",
      destdir = destdir,
      # This config points to a directory containing the relevant data
      config = config_demo()
    ),
    NA
  )
  paths <- fs::dir_ls(destdir)
  csv_txt <- any(grepl(".csv.txt", paths, fixed = TRUE))
  expect_false(csv_txt)
})

test_that("take_snapshot is sensitive to `config` set locally", {
  skip_if_not(dropbox_exists(), "2dii's dropbox folder doesn't exist.")

  withr::with_options(list(r2dii_config = "ignore-global-config.yml"), {
    local_config <- example_config("config_demo.yml")

    expect_error(
      take_snapshot(
        "DebtMarketClimate",
        destdir = tempdir(), overwrite = TRUE,
        config = local_config
      ),
      NA
    )
  })
})

test_that("snapshot data is sensitive to config set globally", {
  skip_if_not(dropbox_exists(), "2dii's dropbox folder doesn't exist.")

  expect_error(
    # Using a braking config just to prove take_snapshot() to uses it.
    withr::with_options(list(r2dii_config = "this-will-error.yml"), {
      take_snapshot("DebtMarketClimate", destdir = tempdir(), overwrite = TRUE)
    }),
    "no such file",
    class = "ENOENT"
  )
})

test_that("take_snapshot with overwrite = FALSE skips an existing dataset.", {
  path <- fs::path(tempdir(), "TYPE.OTHERS.txt")
  suppressWarnings(take_snapshot("TYPE.OTHERS", destdir = tempdir()))
  expect_true(fs::file_exists(path))

  # Skip it now via `overwrite = FALSE`
  expect_message(
    suppressWarnings(
      take_snapshot("TYPE.OTHERS", destdir = tempdir(), overwrite = FALSE)
    ),
    "Skipping.*TYPE.OTHERS"
  )
})

test_that("take_snapshot with overwrite = FALSE skips an existing config file", {
  path <- fs::path(tempdir(), "config_demo.yml")
  suppressWarnings(take_snapshot("whatever", destdir = tempdir()))
  expect_true(fs::file_exists(path))

  # Skip it now via `overwrite = FALSE`
  expect_message(
    suppressWarnings(
      take_snapshot("whatever", destdir = tempdir(), overwrite = FALSE)
    ),
    "Skipping.*config"
  )
})

test_that("take_snapshot writes the configuration file whatever happens next.", {
  path <- fs::path(tempdir(), "config_demo.yml")
  expect_warning(
    take_snapshot("whatever", destdir = tempdir(), overwrite = TRUE),
    "Can't write.*whatever"
  )

  expect_true(fs::file_exists(path))
})

test_that("take_snapshot writes a data.frame from a data-function", {
  skip_if_not(dropbox_exists(), "2dii's dropbox folder doesn't exist.")

  path <- fs::path(tempdir(), "EQMarket.Size.csv")
  # I can't figure out why this warning happens
  suppressWarnings(
    take_snapshot("EQMarket.Size", destdir = tempdir(), overwrite = TRUE)
  )
  expect_is(vroom::vroom(path), "data.frame")
})

test_that("take_snapshot writes a data.frame from an exported dataset", {
  skip_if_not(dropbox_exists(), "2dii's dropbox folder doesn't exist.")

  path <- fs::path(tempdir(), "RevenueSplit.csv")
  # I can't figure out why this warning happens
  suppressWarnings(
    take_snapshot("RevenueSplit", destdir = tempdir(), overwrite = TRUE)
  )
  expect_is(vroom::vroom(path), "data.frame")
})

test_that("take_snapshot writes a character from a data-function", {
  skip_if_not(dropbox_exists(), "2dii's dropbox folder doesn't exist.")

  path <- fs::path(tempdir(), "TYPE.BONDS.txt")
  take_snapshot(
    "TYPE.BONDS",
    destdir = tempdir(),
    overwrite = TRUE,
    # This config points to a directory containing the relevant data
    config = config_demo()
  )

  expect_is(readr::read_lines(path), "character")
})

test_that("take_snapshot writes a character form an exported dataset", {
  path <- fs::path(tempdir(), "TYPE.OTHERS.txt")
  # I can't figure out why this warning happens
  suppressWarnings(
    take_snapshot("TYPE.OTHERS", destdir = tempdir(), overwrite = TRUE)
  )
  expect_is(readr::read_lines(path), "character")
})

test_that("take_snapshot warns if can't write a dataset", {
  expect_warning(
    take_snapshot("bad", destdir = tempdir(), overwrite = TRUE),
    "Can't write.*bad"
  )
})

test_that("possible_snapshots hasn't changed", {
  ref <- c(
    "ALD.BV",
    "ALD.CB",
    "ALD.CC",
    "ALD.EQ",
    "ALD.SPV",
    "BALANCE.SHEET.DATA",
    "BENCH.REGIONS",
    "BicsSectorBridge",
    "CB_OG",
    "DebtMarket",
    "DebtMarketClimate",
    "EQ_OG",
    "EQMarket.Size",
    "FIN.DATA",
    "Fund.Data",
    "FundsTrusts",
    "GROUPS.GOVT",
    "INDEX.REGIONS",
    "Indices",
    "LoanMarket",
    "LoanMarketClimate",
    "PHYSICAL.RISK.CB",
    "PHYSICAL.RISK.EQ",
    "Receipts",
    "RevenueSplit",
    "SCEN",
    "SCENLong",
    "SEC.TYPE.BONDS",
    "sector.bridge",
    "SectorBridge",
    "TYPE.BONDS",
    "TYPE.EQUITY",
    "TYPE.OTHERS",
    "TYPE.RECEIPTS"
  )

  expect_equal(sort(possible_snapshots()), sort(ref))
})

test_that("take_snapshot errs if r2dii.dataraw is not attached", {
  if (any(grepl("package:r2dii.dataraw", search()))) {
    detach("package:r2dii.dataraw")
  }

  expect_error(
    take_snapshot("BENCH.REGIONS", destdir = tempdir()),
    "must be attached"
  )

  library(r2dii.dataraw)
})
