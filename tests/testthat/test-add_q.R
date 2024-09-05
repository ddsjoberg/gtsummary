skip_on_cran()

test_that("add_q() works after add_p()", {
  table1 <- trial %>%
    tbl_summary(by = trt) %>%
    add_p(pvalue_fun = label_style_pvalue(digits = 3))

  # check the adjusted pvalues are correct
  expect_equal(
    table1 |>
      add_q(method = "holm") |>
      getElement("table_body") |>
      dplyr::pull(q.value),
    p.adjust(table1$table_body$p.value)
  )

  # check the pvalue function is correctly inherited
  expect_equal(
    table1 |>
      add_q(method = "holm") |>
      as.data.frame(col_label = FALSE) |>
      dplyr::pull(q.value),
    p.adjust(table1$table_body$p.value) |>
      style_pvalue(digits = 3)
  )
})

test_that("add_q() errors with no p.value column", {
  table1 <- tbl_summary(trial, by = trt)

  expect_snapshot(
    error = TRUE,
    add_q(table1)
  )
})

