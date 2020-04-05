context("test-bold_p")

#### new comment
test_that("no errors/warnings with standard use in tbl_summary() and add_p()", {
  tbl_summary_comp <- tbl_summary(mtcars, by = am) %>%
    add_p()

  expect_error(bold_p(tbl_summary_comp), NA)
  expect_warning(bold_p(tbl_summary_comp), NA)
})


test_that("expect error with use in tbl_summary() but NO add_p()", {
  table1_without_comp <-
    tbl_summary(mtcars, by = am)

  expect_error(bold_p(table1_without_comp),
    "*",
  )
})



test_that("no errors/warnings with q=TRUE and add_q() used in tbl_summary", {
  table1_comp_with_q <-
    tbl_summary(mtcars, by = am) %>%
    add_p() %>%
    add_q()

  expect_error(bold_p(table1_comp_with_q, q = TRUE), NA)
  expect_warning(bold_p(table1_comp_with_q, q = TRUE), NA)
})


test_that("expect error with q=TRUE and add_q() NOT USED in tbl_summary", {
  table1_comp_without_q <-
    tbl_summary(mtcars, by = am) %>%
    add_p()

  expect_error(bold_p(table1_comp_without_q, q = TRUE),
    "*",
  )
})


test_that("no errors/warnings with standard use in tbl_regression()", {
  fmt_reg <- lm(mpg ~ hp + am, mtcars) %>%
    tbl_regression()

  expect_error(bold_p(fmt_reg), NA)
  expect_warning(bold_p(fmt_reg), NA)
})


test_that("no errors/warnings with standard use in tbl_uvregression()", {
  fmt_uni_reg <- trial %>%
    tbl_uvregression(
      method = lm,
      y = age
    )

  expect_error(bold_p(fmt_uni_reg, t = 0.3), NA)
  expect_warning(bold_p(fmt_uni_reg, t = 0.3), NA)
})


test_that("no errors/warnings with use in tbl_uvregression() with add_global_p()", {
  fmt_uni_reg_global_p <- trial %>%
    tbl_uvregression(
      method = lm,
      y = age
    ) %>%
    add_global_p()

  expect_error(bold_p(fmt_uni_reg_global_p, t = 0.3), NA)
  expect_warning(bold_p(fmt_uni_reg_global_p, t = 0.3), NA)
})
