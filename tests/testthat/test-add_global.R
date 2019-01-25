context("test-add_global")

lm(hp ~ factor(cyl) + mpg + factor(am), mtcars) %>% fmt_regression() %>% add_global()
lm(hp ~ factor(cyl) + mpg + factor(am), mtcars) %>% fmt_regression() %>% add_global(keep = TRUE)
test_that("no errors/warnings with standard use", {
  expect_error(
    lm(hp ~ factor(cyl) + mpg + factor(am), mtcars) %>% fmt_regression() %>% add_global(), NA
  )
  expect_warning(
    lm(hp ~ factor(cyl) + mpg + factor(am), mtcars) %>% fmt_regression() %>% add_global(), NA
  )

  expect_error(
    lm(hp ~ factor(cyl) + mpg + factor(am), mtcars) %>% fmt_regression() %>% add_global(keep = TRUE), NA
  )
  expect_warning(
    lm(hp ~ factor(cyl) + mpg + factor(am), mtcars) %>% fmt_regression() %>% add_global(keep = TRUE), NA
  )
})
