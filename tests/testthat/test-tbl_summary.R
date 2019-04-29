context("test-tbl_summary")

test_that("tbl_summary creates output without error/warning (no by var)", {
  expect_error(
    purrr::map(list(mtcars, iris), ~ tbl_summary(.x)),
    NA
  )
  expect_warning(
    purrr::map(list(mtcars, iris), ~ tbl_summary(.x)),
    NA
  )
})


test_that("tbl_summary creates output without error/warning (with by var)", {
  expect_error(
    tbl_summary(mtcars, by = "am"),
    NA
  )
  expect_warning(
    tbl_summary(mtcars, by = "am"),
    NA
  )
})


test_that("tbl_summary throws errors/messages with bad 'sort = ' specifications", {
  expect_message(
    tbl_summary(mtcars, sort = list(not_a_var = "frequency")),
    "*"
  )
  expect_error(
    tbl_summary(mtcars, sort = list(..categorical.. = c("frequency", "two"))),
    "*"
  )
  expect_error(
    tbl_summary(mtcars, sort = list(..categorical.. = "freq5555uency")),
    "*"
  )
})

test_that("tbl_summary omits date var", {
  expect_message(
    tbl_summary(sp500),
    "Column(s)*"
  )
})
