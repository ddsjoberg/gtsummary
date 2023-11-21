skip_on_cran()

continuous_digits_guess(
  data = mtcars,
  variable = "drat",
  summary_type = "continuous"
)

test_that("guess digits: mtcars$drat", {
  expect_equal(
    continuous_digits_guess(
      data = mtcars,
      variable = "drat",
      summary_type = "continuous"
    ),
    2
  )
})

test_that("guess digits: mtcars$vs", {
  expect_true(
    is.na(continuous_digits_guess(
      data = mtcars,
      variable = "vs",
      summary_type = "dichotomous"
    ))
  )
})
