context("test-tbl_summary_input_checks")

test_that("input check", {
  expect_message(
    tbl_summary(trial, type = list(ag5555e = "continuous")),
    "The following names from 'type' are not found in 'data' and were ignored.*"
  )
  expect_message(
    tbl_summary(trial, label = list(ag5555e = "Age")),
    "The following names from 'label' are not found in 'data' and were ignored.*"
  )
  expect_message(
    tbl_summary(trial, statistic = list(conti5555nuous = "{median}")),
    "Expecting list names 'continuous' and 'categorical'.*"
  )
  expect_error(
    tbl_summary(trial, type = list(age = "cont555inuous")),
    "'type' values must be 'continuous', 'categorical', or 'dichotomous'.*"
  )
  expect_error(
    tbl_summary(trial, value = list(stage = "t555555")),
    "'t555555' not a level of the variable 'stage'"
  )
})
