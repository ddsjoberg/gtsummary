context("test-add_p_test_safe")

test_that("testing statistical tests", {

  # t.test
  expect_error(
    add_p_test_safe(
      data = trial, variable = "age", by = trt,
      test = "t.test", type = "continuous", group = NULL
    ),
    NA
  )
  expect_warning(
    add_p_test_safe(
      data = trial, variable = "age", by = trt,
      test = "t.test", type = "continuous", group = NULL
    ),
    NA
  )

  # random effects model
  expect_error(
    add_p_test_safe(
      data = trial, variable = "age", by = trt,
      test = "lme4", type = "continuous", group = "stage"
    ),
    NA
  )
  expect_warning(
    add_p_test_safe(
      data = trial, variable = "age", by = trt,
      test = "lme4", type = "continuous", group = "stage"
    ),
    NA
  )
  expect_error(
    add_p_test_safe(
      data = trial, variable = "response", by = trt,
      test = "lme4", type = "categorical", group = "stage"
    ),
    NA
  )
  expect_warning(
    add_p_test_safe(
      data = trial, variable = "response", by = trt,
      test = "lme4", type = "categorical", group = "stage"
    ),
    NA
  )
})
