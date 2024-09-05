skip_on_cran()

test_that("select_helpers work", {
  tbl_sum <- trial |> tbl_summary(type = age ~ "continuous2")
  tbl_reg <-
    lm(age ~ marker + trt * grade, trial) |>
    tbl_regression(intercept = TRUE)

  expect_equal(
    scope_table_body(tbl_sum$table_body) |>
      dplyr::select(all_continuous()) |>
      names(),
    c("age", "marker", "ttdeath")
  )

  expect_equal(
    scope_table_body(tbl_sum$table_body) |>
      dplyr::select(all_continuous(continuous2 = FALSE)) |>
      names(),
    c("marker", "ttdeath")
  )

  expect_equal(
    scope_table_body(tbl_sum$table_body) |>
      dplyr::select(all_continuous2()) |>
      names(),
    "age"
  )

  expect_equal(
    scope_table_body(tbl_sum$table_body) |>
      dplyr::select(all_categorical()) |>
      names(),
    c("trt", "stage", "grade", "response", "death")
  )

  expect_equal(
    scope_table_body(tbl_sum$table_body) |>
      dplyr::select(all_categorical(dichotomous = FALSE)) |>
      names(),
    c("trt", "stage", "grade")
  )

  expect_equal(
    scope_table_body(tbl_sum$table_body) |>
      dplyr::select(all_dichotomous()) |>
      names(),
    c("response", "death")
  )

  expect_equal(
    tbl_summary(trial, by = trt, include = c(age, grade)) |>
      add_p() |>
      getElement("table_body") |>
      scope_table_body() |>
      dplyr::select(all_tests("wilcox.test")) |>
      names(),
    "age"
  )

  expect_equal(
    scope_table_body(tbl_reg$table_body) |>
      dplyr::select(all_intercepts()) |>
      names(),
    "(Intercept)"
  )

  expect_equal(
    scope_table_body(tbl_reg$table_body) |>
      dplyr::select(all_interaction()) |>
      names(),
    "trt:grade"
  )

  expect_equal(
    scope_table_body(tbl_reg$table_body) |>
      dplyr::select(all_contrasts()) |>
      names(),
    c("trt", "grade")
  )

  expect_equal(
    tbl_merge(list(tbl_sum, tbl_sum))$table_body |>
      dplyr::select(all_stat_cols()) |>
      names(),
    c("stat_0_1", "stat_0_2")
  )
})
