skip_on_cran()

test_that("input check", {
  expect_error(
    tbl_summary(trial, type = list("age" ~ "cont555inuous")),
    NULL
  )
  expect_error(
    tbl_summary(trial, type = list("cont555inuous")),
    NULL
  )
  expect_error(
    tbl_summary(trial, value = list("Drug")),
    NULL
  )
  expect_error(
    tbl_summary(trial, value = all_continuous() ~ TRUE),
    NULL
  )
  expect_error(
    tbl_summary(trial, label = list(TRUE)),
    NULL
  )
  expect_error(
    tbl_summary(trial, label = list(vars(age) ~ 7)),
    NULL
  )
  expect_error(
    tbl_summary(trial, statistic = list("{median}")),
    NULL
  )
  expect_error(
    tbl_summary(trial, sort = list("frequency")),
    NULL
  )
  expect_error(
    tbl_summary(trial, sort = TRUE),
    NULL
  )
  expect_error(
    tbl_summary(trial %>% select(-everything())),
    NULL
  )
  expect_error(
    tbl_summary(trial, type = is.character),
    NULL
  )

  expect_error(
    tbl_summary(trial, digits = list(is.character)),
    NULL
  )
})
