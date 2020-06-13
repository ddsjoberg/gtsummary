context("test-continuous_digits_guess")
testthat::skip_on_cran()

test_that("guess digits: mtcars$drat", {
  expect_equal(
    continuous_digits_guess(
      data = mtcars,
      variable = "drat",
      summary_type = "continuous",
      class = "numeric",
      digits = NULL
    )[[1]],
    2
  )
})

test_that("guess digits: mtcars$vs", {
  expect_true(
    is.na(continuous_digits_guess(
      data = mtcars,
      variable = "vs",
      summary_type = "dichotomous",
      class = "numeric",
      digits = NULL
    ))
  )
})
