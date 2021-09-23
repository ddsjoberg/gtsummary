test_that("tbl_custom_summary() basics", {
  mean_age <- function(data,...) {
    dplyr::tibble(mean_age = mean(data$age, na.rm = TRUE))
  }

  expect_error(
    tbl1 <-
      trial %>%
      tbl_custom_summary(
        include = c("grade", "response", "marker"),
        by = "trt",
        stat_fns = everything() ~ mean_age,
        statistic = everything() ~ "{mean_age}",
        digits = everything() ~ 1
      ) %>%
      add_overall(last = TRUE) %>%
      modify_footnote(all_stat_cols() ~ "Mean age") %>%
      as_tibble(col_labels = FALSE),
    NA
  )

})
