context("test-inline_text.fmt_table1")

test_that("no errors/warnings from mtcars", {
  t1 <- fmt_table1(mtcars)
  t2 <- fmt_table1(mtcars, by = "am")
  expect_error(inline_text(t1, "mpg"), NA)
  expect_error(inline_text(t1, "cyl:4"), NA)
  expect_error(inline_text(t2, "mpg:1"), NA)
  expect_error(inline_text(t2, "cyl:4:1"), NA)
})
