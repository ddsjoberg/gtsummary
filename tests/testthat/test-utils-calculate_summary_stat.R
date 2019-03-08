context("test-utils-calculate_summary_stat")

test_that("Testing when all values are NA", {

  # no by var
  expect_error(
    calculate_summary_stat(
      data = trial %>% dplyr::mutate(nas = NA), variable = nas, by = NULL,
      summary_type = "continuous", dichotomous_value = NULL, var_label = "All NA",
      stat_display = "{median}", digits = NA, class = NA, missing = "no"
    ),
    NA
  )
  expect_warning(
    calculate_summary_stat(
      data = trial %>% dplyr::mutate(nas = NA), variable = nas, by = NULL,
      summary_type = "continuous", dichotomous_value = NULL, var_label = "All NA",
      stat_display = "{median}", digits = NA, class = NA, missing = "no"
    ),
    NA
  )

  # with by var
  expect_error(
    calculate_summary_stat(
      data = trial %>% dplyr::mutate(nas = NA), variable = nas, by = "trt",
      summary_type = "continuous", dichotomous_value = NULL, var_label = "All NA",
      stat_display = "{median}", digits = NA, class = NA, missing = "no"
    ),
    NA
  )
  expect_warning(
    calculate_summary_stat(
      data = trial %>% dplyr::mutate(nas = NA), variable = nas, by = "trt",
      summary_type = "continuous", dichotomous_value = NULL, var_label = "All NA",
      stat_display = "{median}", digits = NA, class = NA, missing = "no"
    ),
    NA
  )
})

