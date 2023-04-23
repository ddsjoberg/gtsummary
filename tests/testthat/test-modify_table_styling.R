skip_on_cran()

test_that("checks for rows arg", {
  expect_snapshot(
    tbl_summary(trial[c("trt", "age")]) %>%
      modify_table_styling(
        columns = label,
        footnote = "test footnote",
        rows = variable == "age"
      ) %>%
      as.data.frame()
  )

  footnote_variable <- "age"
  expect_snapshot(
    tbl_summary(trial[c("trt", "age")]) %>%
      modify_table_styling(
        columns = label,
        footnote = "test footnote",
        rows = variable == footnote_variable
      ) %>%
      as.data.frame()
  )

  null_value <- NULL
  expect_snapshot(
    tbl_summary(trial[c("trt", "age")]) %>%
      modify_table_styling(
        columns = label,
        footnote = "test footnote",
        rows = null_value
      ) %>%
      as.data.frame()
  )


  # testing column merging
  expect_error(
    tbl1 <-
      trial %>%
      select(trt, age, response) %>%
      tbl_summary(by = trt, missing = "no") %>%
      add_difference() %>%
      modify_table_styling(
        columns = stat_1,
        cols_merge_pattern = "{stat_1}  ---  {stat_2}"
      ),
    NA
  )
  expect_snapshot(tbl1 %>% as.data.frame())
  expect_equal(
    as_tibble(tbl1, col_labels = FALSE) %>% pull(stat_1),
    c("46 (37, 59)  ---  48 (39, 56)", "28 (29%)  ---  33 (34%)")
  )

  # testing un-merging columns
  expect_error(
    tbl2 <-
      tbl1 %>%
      modify_table_styling(
        columns = stat_1,
        cols_merge_pattern = NA
      ) %>%
      modify_column_unhide(all_stat_cols()),
    NA
  )
  expect_snapshot(tbl2 %>% as.data.frame())
  expect_equal(
    as_tibble(tbl2, col_labels = FALSE) %>% pull(stat_1),
    c("46 (37, 59)", "28 (29%)")
  )
  expect_equal(
    as_tibble(tbl2, col_labels = FALSE) %>% pull(stat_2),
    c("48 (39, 56)", "33 (34%)")
  )

  expect_error(
    tbl1 %>%
      modify_table_styling(
        columns = stat_2,
        cols_merge_pattern = "{stat_1}  ---  {stat_2}"
      )
  )

  expect_error(
    tbl1 %>%
      modify_table_styling(
        columns = stat_1,
        cols_merge_pattern = "{not_a_column}"
      )
  )
})
