test_that("dropbox_exists is true if this test is not skipped", {
  skip_if_not(dropbox_exists(), "2dii's dropbox folder doesn't exist.")

  expect_true(dropbox_exists())
})

test_that("dropbox_exists is true in Mauro's computer", {
  if (identical(r2dii.utils::USER.NAME(), "Mauro")) {
    expect_true(
      dropbox_exists()
    )
  } else {
    message("Not in Mauro's PC.")
  }
})
