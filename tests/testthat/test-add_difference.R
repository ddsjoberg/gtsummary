context("test-add_difference")
testthat::skip_on_cran()

test_that("add_difference-basic use", {
  expect_error(
    tbl_diff <-
      trial %>%
      select(trt, marker, age) %>%
      tbl_summary(by = trt, missing = "no") %>%
      add_difference(
        test = everything() ~ "t.test",
        test.args = all_tests("t.test") ~ list(var.equal = TRUE)
      ),
    NA
  )

  expect_equal(
    dplyr::filter(tbl_diff$table_body, variable == "marker") %>% select(estimate, conf.low, conf.high, p.value),
    t.test(marker ~ trt, trial, var.equal = TRUE) %>% broom::tidy() %>% select(estimate, conf.low, conf.high, p.value)
  )

})
