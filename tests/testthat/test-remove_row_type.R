skip_on_cran()

test_that("no errors/warnings with standard use", {
  tbl_sum <- trial %>%
    select(trt, age, grade, stage) %>%
    tbl_summary(by = trt)
  tbl_reg <- lm(age ~ grade + stage, trial) %>% tbl_regression()

  expect_error(tbl1 <- remove_row_type(tbl_sum, type = "header"), NA)
  expect_warning(remove_row_type(tbl_sum, type = "header"), NA)
  expect_snapshot(tbl1 %>% as.data.frame())

  expect_equal(
    tbl1$table_body$label,
    c("Age", "Unknown", "I", "II", "III", "T1", "T2", "T3", "T4")
  )

  expect_error(tbl2 <- remove_row_type(tbl_sum, type = "missing"), NA)

  expect_equal(
    tbl2$table_body$label,
    c("Age", "Grade", "I", "II", "III", "T Stage", "T1", "T2", "T3", "T4")
  )

  expect_error(tbl3 <- remove_row_type(tbl_reg, type = "reference"), NA)
  expect_snapshot(tbl3 %>% as.data.frame())
  expect_equal(
    as.vector(tbl3$table_body$label),
    c("Grade", "II", "III", "T Stage", "T2", "T3", "T4")
  )

  expect_error(remove_row_type(tbl_sum, type = "reference"))


  expect_equal(
    trial %>%
      tbl_summary(include = c(age, grade)) %>%
      remove_row_type(variable = grade, type = "all") %>%
      purrr::pluck("table_body"),
    trial %>%
      tbl_summary(include = age) %>%
      remove_row_type(variable = grade, type = "all") %>%
      purrr::pluck("table_body")
  )

  expect_equal(
    trial %>%
      tbl_summary(include = grade) %>%
      remove_row_type(variable = grade, type = "level", level_value = "I") %>%
      purrr::pluck("table_body", "label"),
    c("Grade", "II", "III")
  )

  expect_equal(
    trial %>%
      tbl_summary(include = grade) %>%
      remove_row_type(variable = grade, type = "level") %>%
      purrr::pluck("table_body", "label"),
    "Grade"
  )

  expect_error(
    trial %>%
      tbl_summary(include = grade) %>%
      remove_row_type(variable = grade, type = "level", level_value = 5)
  )

  expect_message(
    trial %>%
      tbl_summary(include = grade) %>%
      remove_row_type(variable = grade, type = "header", level_value = "I")
  )

  # previously there was a bug where header rows for cont2 variables was not removed
  expect_snapshot(
    data.frame(x = 1:100, y = c(rep("A", 50), rep("B", 50))) %>%
      tbl_summary(
        by = y,
        type = x ~ "continuous2",
        statistic = x ~ c("{mean}", "{min}", "{max}")
      ) %>%
      remove_row_type(
        x,
        type = "header"
      ) %>%
      as.data.frame()
  )
})
