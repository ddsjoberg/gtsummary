context("test-utils-calculate_pvalue_one")


test_that("testing statistical tests", {

  # t.test
  expect_error(
    calculate_pvalue_one(
      data = trial, variable = "age", by = trt,
      test = "t.test", type = "continuous", group = NULL
    ),
    NA
  )
  expect_warning(
    calculate_pvalue_one(
      data = trial, variable = "age", by = trt,
      test = "t.test", type = "continuous", group = NULL
    ),
    NA
  )

  # random effects model
  expect_error(
    calculate_pvalue_one(
      data = trial, variable = "age", by = trt,
      test = "lme4", type = "continuous", group = "stage"
    ),
    NA
  )
  expect_warning(
    calculate_pvalue_one(
      data = trial, variable = "age", by = trt,
      test = "lme4", type = "continuous", group = "stage"
    ),
    NA
  )
  expect_error(
    calculate_pvalue_one(
      data = trial, variable = "response", by = trt,
      test = "lme4", type = "categorical", group = "stage"
    ),
    NA
  )
  expect_warning(
    calculate_pvalue_one(
      data = trial, variable = "response", by = trt,
      test = "lme4", type = "categorical", group = "stage"
    ),
    NA
  )
})
