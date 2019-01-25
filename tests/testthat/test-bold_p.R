context("test-bold_p")

#### new comment
test_that("no errors/warnings with standard use in fmt_table1() and add_comparison()", {
  fmt_table1_comp <- fmt_table1(mtcars, by = "am") %>%
    add_comparison()

  expect_error(bold_p(fmt_table1_comp), NA)
  expect_warning(bold_p(fmt_table1_comp), NA)
})


test_that("expect error with use in fmt_table1() but NO add_comparison()", {
  table1_without_comp <-
    fmt_table1(mtcars, by = "am")

  expect_error(bold_p(table1_without_comp),
    "There are no p-values to bold. You need to use add_comparison() after fmt_table1() and before using bold_p()",
    fixed = TRUE
  )
})



test_that("no errors/warnings with q=TRUE and add_q() used in fmt_table1", {
  table1_comp_with_q <-
    fmt_table1(mtcars, by = "am") %>%
    add_comparison() %>%
    add_q()

  expect_error(bold_p(table1_comp_with_q, q = TRUE), NA)
  expect_warning(bold_p(table1_comp_with_q, q = TRUE), NA)
})


test_that("expect error with q=TRUE and add_q() NOT USED in fmt_table1", {
  table1_comp_without_q <-
    fmt_table1(mtcars, by = "am") %>%
    add_comparison()

  expect_error(bold_p(table1_comp_without_q, q = TRUE),
    "There are no q-values to bold. You need to use add_q() after add_comparison() and before using bold_p(q = TRUE)",
    fixed = TRUE
  )
})


test_that("no errors/warnings with standard use in fmt_regression()", {
  fmt_reg <- lm(mpg ~ hp + am, mtcars) %>%
    fmt_regression()

  expect_error(bold_p(fmt_reg), NA)
  expect_warning(bold_p(fmt_reg), NA)
})


test_that("no errors/warnings with standard use in fmt_uni_regression()", {
  fmt_uni_reg <- trial %>%
    fmt_uni_regression(
      method = "lm",
      y = "age"
    )

  expect_error(bold_p(fmt_uni_reg, p = 0.3), NA)
  expect_warning(bold_p(fmt_uni_reg, p = 0.3), NA)
})


test_that("no errors/warnings with use in fmt_uni_regression() with add_global()", {
  fmt_uni_reg_global_p <- trial %>%
    fmt_uni_regression(
      method = "lm",
      y = "age"
    ) %>%
    add_global()

  expect_error(bold_p(fmt_uni_reg_global_p, p = 0.3), NA)
  expect_warning(bold_p(fmt_uni_reg_global_p, p = 0.3), NA)
})
