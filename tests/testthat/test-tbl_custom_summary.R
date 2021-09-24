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
      modify_column_unhide(everything()) %>%
      as_tibble(col_labels = FALSE),
    NA
  )

  expect_equal(
    tbl1 %>%
      filter(variable == "grade", row_type == "level") %>%
      select(all_stat_cols(F)),
    trial %>%
      select(age, grade, trt) %>%
      filter(complete.cases(.)) %>%
      group_by(trt, grade) %>%
      dplyr::summarise(mean_age = mean(age), .groups = "drop") %>%
      tidyr::pivot_wider(id_cols = grade, names_from = trt, values_from = mean_age) %>%
      select(-1) %>%
      set_names(c("stat_1", "stat_2")) %>%
      dplyr::mutate_all(~style_number(., 1))
  )

})
