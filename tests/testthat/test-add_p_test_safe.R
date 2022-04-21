skip_on_cran()
skip_if_not(broom.helpers::.assert_package("lme4", pkg_search = "gtsummary", boolean = TRUE))

test_that("testing statistical tests", {

  # t.test
  expect_error(
    .get_add_p_test_fun(class = "tbl_summary", test = "t.test") %>%
      .run_add_p_test_fun(
        data = trial, variable = "age", by = "trt",
        type = "continuous", group = NULL
      ),
    NA
  )
  expect_warning(
    .get_add_p_test_fun(class = "tbl_summary", test = "t.test") %>%
      .run_add_p_test_fun(
        data = trial, variable = "age", by = "trt",
        type = "continuous", group = NULL
      ),
    NA
  )

  # random effects model
  expect_error(
    .get_add_p_test_fun(class = "tbl_summary", test = "lme4") %>%
      .run_add_p_test_fun(
        data = trial, variable = "age", by = "trt",
        type = "continuous", group = "stage"
      ),
    NA
  )
  expect_warning(
    .get_add_p_test_fun(class = "tbl_summary", test = "lme4") %>%
      .run_add_p_test_fun(
        data = trial, variable = "age", by = "trt",
        type = "continuous", group = "stage"
      ),
    NA
  )
  expect_error(
    .get_add_p_test_fun(class = "tbl_summary", test = "lme4") %>%
      .run_add_p_test_fun(
        data = trial, variable = "response", by = "trt",
        type = "categorical", group = "stage"
      ),
    NA
  )
  expect_warning(
    .get_add_p_test_fun(class = "tbl_summary", test = "lme4") %>%
      .run_add_p_test_fun(
        data = trial, variable = "response", by = "trt",
        type = "categorical", group = "stage"
      ),
    NA
  )
})
