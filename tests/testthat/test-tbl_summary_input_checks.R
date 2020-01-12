context("test-tbl_summary_input_checks")

test_that("input check", {
  expect_error(
    tbl_summary(trial, type = list("age" ~ "cont555inuous")),
    "*"
  )
  expect_error(
    tbl_summary(trial, type = list("cont555inuous")),
    "*"
  )
  expect_error(
    tbl_summary(trial, value = list("Drug")),
    "*"
  )
  expect_error(
    tbl_summary(trial, value = all_continuous() ~ TRUE),
    "*"
  )
  expect_error(
    tbl_summary(trial, label = list(TRUE)),
    "*"
  )
  expect_error(
    tbl_summary(trial, label = list(vars(age) ~ 7)),
    "*"
  )
  expect_error(
    tbl_summary(trial, statistic = all_continuous() ~ c("{median}", "{mode}")),
    "*"
  )
  expect_error(
    tbl_summary(trial, statistic = list("{median}")),
    "*"
  )
  expect_error(
    tbl_summary(trial, sort = list("frequency")),
    "*"
  )
  expect_error(
    tbl_summary(trial, sort = TRUE),
    "*"
  )
  expect_error(
    tbl_summary(trial %>% select(-everything())),
    "*"
  )
})
