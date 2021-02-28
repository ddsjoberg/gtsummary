skip_on_cran()

test_that("checks for rows arg", {
  expect_error(
    tbl_summary(trial[c("trt", "age")]) %>%
      modify_table_styling(columns = label,
                           footnote =  "test footnote",
                           rows = variable == "age"),
    NA
  )

  footnote_variable <- "age"
  expect_error(
    tbl_summary(trial[c("trt", "age")]) %>%
      modify_table_styling(columns = label,
                           footnote =  "test footnote",
                           rows = variable == footnote_variable),
    NA
  )

  null_value <- NULL
  expect_error(
    tbl_summary(trial[c("trt", "age")]) %>%
      modify_table_styling(columns = label,
                           footnote =  "test footnote",
                           rows = null_value),
    NA
  )


})
