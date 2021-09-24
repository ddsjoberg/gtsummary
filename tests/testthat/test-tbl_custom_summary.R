test_that("tbl_custom_summary() basics", {
  mean_age <- function(data, ...) {
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
      dplyr::filter(variable == "grade", row_type == "level") %>%
      select(all_stat_cols(F)),
    trial %>%
      select(age, grade, trt) %>%
      dplyr::filter(complete.cases(.)) %>%
      dplyr::group_by(trt, grade) %>%
      dplyr::summarise(mean_age = mean(age), .groups = "drop") %>%
      tidyr::pivot_wider(id_cols = grade, names_from = trt, values_from = mean_age) %>%
      select(-1) %>%
      set_names(c("stat_1", "stat_2")) %>%
      dplyr::mutate_all(~ style_number(., 1))
  )

  expect_equal(
    tbl1 %>%
      filter(variable == "response", row_type == "label") %>%
      select(all_stat_cols(F)) %>%
      unlist() %>%
      unname(),
    trial %>%
      select(age, response, trt) %>%
      filter(complete.cases(.)) %>%
      group_by(trt, response) %>%
      dplyr::summarise(mean_age = mean(age), .groups = "drop") %>%
      filter(response == 1) %>%
      dplyr::pull(mean_age) %>%
      style_number(digits = 1)
  )

  expect_equal(
    tbl1$stat_0,
    c(NA, "46.2", "47.5", "48.1", "49.8", "7", "47.0", "10")
  )
})
