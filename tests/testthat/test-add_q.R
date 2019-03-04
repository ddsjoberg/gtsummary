context("test-add_q")

test_that("no errors/warnings with standard use after tbl_summary() and add_comparison()", {
  table1 <- trial %>%
    tbl_summary(by = "trt") %>%
    add_comparison()

  expect_error(add_q(table1), NA)
  expect_warning(add_q(table1), NA)
})


test_that("expect error if no p value in table 1", {
  table1 <- trial %>% tbl_summary(by = "trt")

  expect_error(
    add_q(table1),
    "There are no p-values yet. You need to use the function add_comparison()
    after tbl_summary() and before add_q()",
    fixed = TRUE
  )
})


test_that("no errors/warnings with standard use after tbl_uregression() and add_global()", {
  uni_reg <- trial %>%
    tbl_uregression(
      method = "lm",
      y = "age"
    ) %>%
    add_global()

  expect_error(add_q(uni_reg), NA)
  expect_warning(add_q(uni_reg), NA)
})



test_that("expect error with no global p value in tbl_uregression", {
  uni_reg <- trial %>% tbl_uregression(
    method = "lm",
    y = "age"
  )

  expect_error(
    add_q(uni_reg),
    "You need global p-values first. Use the function add_global() after
    tbl_uregression() and before add_q()",
    fixed = TRUE
  )
})
