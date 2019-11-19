test_that("overwrite_demo has the expected names", {
  expect_named(
    overwrite_demo,
    c("level", "id", "name", "sector", "source")
  )
})
