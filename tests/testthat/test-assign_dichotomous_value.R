skip_on_cran()

test_df <-
  tibble::tibble(
    v1 = factor(c("no")),
    v2 = factor(c("yes", "no")),
    v3 = factor(c("YeS", "No"), levels = c("YeS", "No"))
  )

test_that("assign_dichotomous_value_one for yes/no factors", {
  expect_equal(
    assign_dichotomous_value_one(
      data = test_df, variable = "v2",
      summary_type = "dichotomous",
      value = NULL
    ),
    "yes"
  )
})

test_that("input checks", {
  expect_error(
    assign_dichotomous_value(
      data = trial, variable = "stage",
      summary_type = "dichotomous",
      value = NULL
    )
  )
})
