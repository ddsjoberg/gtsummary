test_that("modify_fmt_fun() works", {
  expect_error(
    tbl <- lm(age ~ marker + grade, trial) |>
      tbl_regression() %>%
      modify_fmt_fun(
        p.value = label_style_pvalue(digits = 3),
        c(estimate, conf.low, conf.high) ~ label_style_sigfig(digits = 4),
        rows = variable == "grade"
      ),
    NA
  )

  expect_equal(
    tbl |>
      getElement("table_styling") |>
      getElement("fmt_fun") |>
      dplyr::filter(column %in% "p.value") |>
      dplyr::slice_tail(n = 1, by = "column") |>
      dplyr::pull(fmt_fun) |>
      unique(),
    list(label_style_pvalue(digits = 3))
  )
  expect_equal(
    tbl |>
      getElement("table_styling") |>
      getElement("fmt_fun") |>
      dplyr::filter(column %in% "p.value") |>
      dplyr::slice_tail(n = 1, by = "column") |>
      dplyr::pull(rows) |>
      unique() |>
      getElement(1L) |>
      quo_get_expr(),
    expr(variable == "grade")
  )

  expect_equal(
    tbl |>
      getElement("table_styling") |>
      getElement("fmt_fun") |>
      dplyr::filter(column %in% c("estimate", "conf.low", "conf.high")) |>
      dplyr::slice_tail(n = 1, by = "column") |>
      dplyr::pull(fmt_fun) |>
      unique(),
    list(label_style_sigfig(digits = 4))
  )
  expect_equal(
    tbl |>
      getElement("table_styling") |>
      getElement("fmt_fun") |>
      dplyr::filter(column %in% c("estimate", "conf.low", "conf.high")) |>
      dplyr::slice_tail(n = 1, by = "column") |>
      dplyr::pull(rows) |>
      unique() |>
      getElement(1L) |>
      quo_get_expr(),
    expr(variable == "grade")
  )
})
