context("test-add_global")

test_that("no errors/warnings with standard use after tbl_regression", {
  expect_error(
    lm(hp ~ factor(cyl) + mpg + factor(am), mtcars) %>% tbl_regression() %>% add_global(), NA
  )
  expect_warning(
    lm(hp ~ factor(cyl) + mpg + factor(am), mtcars) %>% tbl_regression() %>% add_global(), NA
  )

  expect_error(
    lm(hp ~ factor(cyl) + mpg + factor(am), mtcars) %>% tbl_regression() %>% add_global(keep = TRUE), NA
  )
  expect_warning(
    lm(hp ~ factor(cyl) + mpg + factor(am), mtcars) %>% tbl_regression() %>% add_global(keep = TRUE), NA
  )
})

test_that("no errors/warnings with standard use after tbl_uvregression", {
  expect_error(
    trial %>% tbl_uvregression(method = lm, y = age) %>% add_global(), NA
  )
  expect_warning(
    trial %>% tbl_uvregression(method = lm, y = age) %>% add_global(), NA
  )
})
