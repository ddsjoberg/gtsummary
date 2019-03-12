context("test-assign_dichotomous_value")


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
      class = "factor",
      value = NULL
    ),
    "yes"
  )
})
