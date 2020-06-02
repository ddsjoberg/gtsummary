context("test-add_global_p")
testthat::skip_on_cran()

test_that("no errors/warnings with standard use after tbl_regression", {
  mod1 <- lm(hp ~ factor(cyl) + mpg + factor(am), mtcars) %>% tbl_regression()
  expect_error(
    mod1 %>% add_global_p(), NA
  )
  expect_warning(
    mod1 %>% add_global_p(), NA
  )

  expect_error(
    mod1 %>% add_global_p(keep = TRUE), NA
  )
  expect_warning(
    mod1 %>% add_global_p(keep = TRUE), NA
  )

  expect_message(mod1 %>% add_global_p(quiet = TRUE), NA)
  expect_message(mod1 %>% add_global_p(quiet = FALSE), "*")
})

test_that("no errors/warnings with standard use after tbl_uvregression", {
  mod2 <- trial %>% tbl_uvregression(method = lm, y = age)
  expect_error(
    mod2 %>% add_global_p(), NA
  )
  expect_warning(
    mod2 %>% add_global_p(), NA
  )

  expect_message(mod2 %>% add_global_p(quiet = TRUE), NA)
  expect_message(mod2 %>% add_global_p(quiet = FALSE), "*")
})
