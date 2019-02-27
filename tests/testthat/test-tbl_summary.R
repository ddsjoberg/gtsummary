context("test-tbl_summary")

test_that("fmt_table1 creates output without error/warning (no by var)", {
  expect_error(
    purrr::map(list(mtcars, iris), ~ tbl_summary(.x)),
    NA
  )
  expect_warning(
    purrr::map(list(mtcars, iris), ~ tbl_summary(.x)),
    NA
  )
})


test_that("fmt_table1 creates output without error/warning (with by var)", {
  expect_error(
    tbl_summary(mtcars, by = "am"),
    NA
  )
  expect_warning(
    tbl_summary(mtcars, by = "am"),
    NA
  )
})
