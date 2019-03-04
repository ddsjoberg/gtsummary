context("test-style_percent")


test_that("no errors/warnings with standard use", {
  percent_vals <- c(-1, 0, 0.0001, 0.005, 0.01, 0.10, 0.45356, 0.99, 1.45)

  expect_error(style_percent(percent_vals), NA)
  expect_warning(style_percent(percent_vals), NA)
})


test_that("<0 returns NA", {
  expect_true(is.na(style_percent(-1)))
})
