context("test-add_q")

test_that("no errors/warnings with standard use after fmt_table1() and add_comparison()", {
  table1 <- trial %>%
    fmt_table1(by = "trt") %>%
    add_comparison()

  expect_error(add_q(table1), NA)
  expect_warning(add_q(table1), NA)
})


test_that("expect error if no p value in table 1", {
  table1 <- trial %>% fmt_table1(by = "trt")

  expect_error(
    add_q(table1),
    "There are no p-values yet. You need to use the function add_comparison()
    after fmt_table1() and before add_q()",
    fixed = TRUE
  )
})


test_that("no errors/warnings with standard use after fmt_uni_regression() and add_global()", {
  uni_reg <- trial %>%
    fmt_uni_regression(
      method = "lm",
      y = "age"
    ) %>%
    add_global()

  expect_error(add_q(uni_reg), NA)
  expect_warning(add_q(uni_reg), NA)
})



test_that("expect error with no global p value in fmt_uni_regression", {
  uni_reg <- trial %>% fmt_uni_regression(
    method = "lm",
    y = "age"
  )

  expect_error(
    add_q(uni_reg, p = 0.2),
    "You need global p-values first. Use the function add_global() after
    fmt_uni_regression() and before add_q()",
    fixed = TRUE
  )
})
