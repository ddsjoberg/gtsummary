context("test-add_comparison")

test_that("add_comparison creates output without error/warning", {
  expect_error(
    tbl_summary(mtcars, by = "am") %>% add_comparison(),
    NA
  )
  expect_warning(
    tbl_summary(mtcars, by = "am") %>% add_comparison(),
    NA
  )
})


test_that("add_comparison creates errors when non-function in input", {
  expect_error(
    tbl_summary(mtcars, by = "am") %>%
      add_comparison(pvalue_fun = mtcars),
    "*"
  )
})
