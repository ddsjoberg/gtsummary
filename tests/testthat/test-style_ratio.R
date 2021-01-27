context("test-style_ratio")
testthat::skip_on_cran()

test_that("correct rounding near one", {
  expect_equal(
    style_ratio(c(0.99, 0.999, 1.1)),
    c("0.99", "1.00", "1.10")
  )

  expect_false(
    style_ratio(0.99) == "1.0"
  )
})
