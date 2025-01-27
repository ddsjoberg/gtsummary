test_that("modify_missing_symbol()", {
  expect_equal(
    lm(marker ~ trt, data = trial) |>
      tbl_regression() |>
      modify_missing_symbol(
        symbol = "Ref.",
        columns = c(estimate, conf.low, conf.high),
        rows = reference_row == TRUE
      ) |>
      as.data.frame(fmt_missing = TRUE, col_labels = FALSE) %>%
      `[`(2, c("estimate", "conf.low")) |>
      unlist() |>
      unique(),
    "Ref."
  )
})
