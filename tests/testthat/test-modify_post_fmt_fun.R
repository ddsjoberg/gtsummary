test_that("modify_post_fmt_fun() works", {
  expect_silent(
    tbl <- data.frame(x = FALSE) |>
      tbl_summary(type = x ~ "categorical") |>
      modify_post_fmt_fun(
        fmt_fun = ~ifelse(. == "0 (0%)", "0", .),
        columns = all_stat_cols()
      )
  )

  expect_true(
    nrow(tbl$table_styling$post_fmt_fun) == 1L
  )

  # test no errors when no columns are selected
  expect_equal(
    data.frame(x = FALSE) |>
      tbl_summary(type = x ~ "categorical") |>
      modify_post_fmt_fun(
        fmt_fun = ~ifelse(. == "0 (0%)", "0", .),
        columns = starts_with("xxxxxx")
      ) |>
      getElement("table_styling") |>
      getElement("post_fmt_fun"),
    data.frame(x = FALSE) |>
      tbl_summary(type = x ~ "categorical") |>
      getElement("table_styling") |>
      getElement("post_fmt_fun")
  )


  # no errors after tables with modify_post_fmt_fun() are stacked
  expect_silent(
    tbl_strata_nested_stack(
      trial,
      strata = trt,
      .tbl_fun = ~ .x |>
        tbl_summary(include = response,) |>
        modify_header(stat_0 = "**Overall**") |>
        modify_post_fmt_fun(
          fmt_fun = ~ ifelse(. == "0 (0.0%)", "0", .),
          columns = all_stat_cols()
        )
    )
  )
})
