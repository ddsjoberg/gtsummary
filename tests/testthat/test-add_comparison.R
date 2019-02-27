context("test-add_comparison")

test_that("add_comparison creates output without error/warning (with by var)", {
  expect_error(
    tbl_summary(mtcars, by = "am") %>% add_comparison(),
    NA
  )
  expect_warning(
    tbl_summary(mtcars, by = "am") %>% add_comparison(),
    NA
  )
})
