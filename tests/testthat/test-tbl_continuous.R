skip_on_cran()

test_that("tbl_continuous() works", {
  expect_error(
    tbl1 <-
      tbl_continuous(
        data = trial,
        variable = age,
        by = trt,
        include = c(grade, stage)
      ) %>%
      modify_header(all_stat_cols() ~ "{level}, N = {N}") %>%
      as_tibble(),
    NA
  )
  expect_snapshot(tbl1)

  expect_snapshot(
    tbl_continuous(
      data = trial,
      variable = age,
      by = trt,
      include = c(grade, stage)
    ) %>%
      add_overall() %>%
      as.data.frame()
  )

  expect_equal(
    names(tbl1),
    c("**Characteristic**", "Drug A, N = 200", "Drug B, N = 200")
  )

  expect_equal(
    tbl_continuous(
      data = trial[c("age", "grade")],
      variable = age,
      include = grade
    ) %>%
      `[`(c("table_body", "table_styling")),
    trial %>%
      select(age, grade) %>%
      tbl_continuous(variable = age) %>%
      `[`(c("table_body", "table_styling"))
  )

  expect_error(
    tbl2 <-
      tbl_continuous(
        data = trial,
        variable = age,
        include = c(grade, stage),
        statistic = everything() ~ "{mean}"
      ) %>%
      as_tibble(col_labels = FALSE),
    NA
  )
  expect_snapshot(tbl2)

  expect_equal(
    tbl2$stat_0,
    c(NA, "46", "48", "48", NA, "47", "48", "49", "45")
  )
})
