test_that("select helpers", {
  expect_error(
    tbl_summary(trial, type = all_character() ~ "categorical"), NA
  )

  expect_error(
    tbl_summary(trial, type = all_double() ~ "continuous"), NA
  )

  expect_error(
    tbl_summary(trial, type = all_factor() ~ "categorical"), NA
  )

  expect_error(
    tbl_summary(trial, type = all_integer() ~ "categorical"), NA
  )

  expect_error(
    tbl_summary(trial, statistic = all_continuous() ~ "t.test"), NA
  )

  expect_error(
    tbl_summary(trial, statistic = all_categorical() ~ "fisher.test"), NA
  )

  expect_error(
    tbl_summary(trial, statistic = all_dichotomous() ~ "fisher.test"), NA
  )

  expect_error(
    tbl_summary(trial, statistic = all_categorical(dichotomous = FALSE) ~ "fisher.test"), NA
  )
})
