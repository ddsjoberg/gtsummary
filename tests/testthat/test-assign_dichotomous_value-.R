context("test-assign_dichotomous_value-")

test_that("input checks", {
  expect_error(
    assign_dichotomous_value(data = trial, variable = "stage",
                             summary_type = "dichotomous",
                             class = "factor", value = NULL),
    "'stage' is dichotomous, but I was unable to determine the level.*"
  )
})




