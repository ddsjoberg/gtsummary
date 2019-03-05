context("test-tab_style_bold_p")

#### new comment
test_that("no errors/warnings with standard use in tbl_summary() and add_comparison()", {
  tbl_summary_comp <- tbl_summary(mtcars, by = "am") %>%
    add_comparison()

  expect_error(tab_style_bold_p(tbl_summary_comp), NA)
  expect_warning(tab_style_bold_p(tbl_summary_comp), NA)
})


test_that("expect error with use in tbl_summary() but NO add_comparison()", {
  table1_without_comp <-
    tbl_summary(mtcars, by = "am")

  expect_error(tab_style_bold_p(table1_without_comp),
               "Before bolding p-values, run add_comparison() to calculate the p-values",
               fixed = TRUE
  )
})



test_that("no errors/warnings with q=TRUE and add_q() used in tbl_summary", {
  table1_comp_with_q <-
    tbl_summary(mtcars, by = "am") %>%
    add_comparison() %>%
    add_q()

  expect_error(tab_style_bold_p(table1_comp_with_q, q = TRUE), NA)
  expect_warning(tab_style_bold_p(table1_comp_with_q, q = TRUE), NA)
})


test_that("expect error with q=TRUE and add_q() NOT USED in tbl_summary", {
  table1_comp_without_q <-
    tbl_summary(mtcars, by = "am") %>%
    add_comparison()

  expect_error(tab_style_bold_p(table1_comp_without_q, q = TRUE),
               "Before bolding q-values, run add_q() to calculate the q-values",
               fixed = TRUE
  )
})


test_that("no errors/warnings with standard use in tbl_regression()", {
  fmt_reg <- lm(mpg ~ hp + am, mtcars) %>%
    tbl_regression()

  expect_error(tab_style_bold_p(fmt_reg), NA)
  expect_warning(tab_style_bold_p(fmt_reg), NA)
})


test_that("no errors/warnings with standard use in tbl_uvregression()", {
  fmt_uni_reg <- trial %>%
    tbl_uvregression(
      method = "lm",
      y = "age"
    )

  expect_error(tab_style_bold_p(fmt_uni_reg, p = 0.3), NA)
  expect_warning(tab_style_bold_p(fmt_uni_reg, p = 0.3), NA)
})


test_that("no errors/warnings with use in tbl_uvregression() with add_global()", {
  fmt_uni_reg_global_p <- trial %>%
    tbl_uvregression(
      method = "lm",
      y = "age"
    ) %>%
    add_global()

  expect_error(tab_style_bold_p(fmt_uni_reg_global_p, p = 0.3), NA)
  expect_warning(tab_style_bold_p(fmt_uni_reg_global_p, p = 0.3), NA)
})
