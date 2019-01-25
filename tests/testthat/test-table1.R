context("test-fmt_table1")

test_that("fmt_table1 creates output without error/warning (no by var)", {
  expect_error(
    purrr::map(list(mtcars, iris), ~ fmt_table1(.x)),
    NA
  )
  expect_warning(
    purrr::map(list(mtcars, iris), ~ fmt_table1(.x)),
    NA
  )
})


test_that("fmt_table1 creates output without error/warning (with by var)", {
  expect_error(
    fmt_table1(mtcars, by = "am"),
    NA
  )
  expect_warning(
    fmt_table1(mtcars, by = "am"),
    NA
  )
})
