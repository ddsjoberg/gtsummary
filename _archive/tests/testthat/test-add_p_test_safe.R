skip_on_cran()
skip_if_not(broom.helpers::.assert_package("lme4", pkg_search = "gtsummary", boolean = TRUE))

test_that("testing statistical tests", {
  # t.test
  expect_snapshot(
    .get_add_p_test_fun(class = "tbl_summary", test = "t.test") %>%
      .run_add_p_test_fun(
        data = trial, variable = "age", by = "trt",
        type = "continuous", group = NULL
      ) %>%
      purrr::pluck("df_result")
  )

  # random effects model
  expect_snapshot(
    .get_add_p_test_fun(class = "tbl_summary", test = "lme4") %>%
      .run_add_p_test_fun(
        data = trial, variable = "age", by = "trt",
        type = "continuous", group = "stage"
      ) %>%
      purrr::pluck("df_result")
  )

  expect_snapshot(
    .get_add_p_test_fun(class = "tbl_summary", test = "lme4") %>%
      .run_add_p_test_fun(
        data = trial, variable = "response", by = "trt",
        type = "categorical", group = "stage"
      ) %>%
      purrr::pluck("df_result")
  )
})
