skip_on_cran()

test_that("no errors/warnings with standard use", {
  percent_vals <- c(-1, 0, 0.00001, 0.0001, 0.005, 0.01, 0.10, 0.45356, 0.99, 1.45)

  expect_error(sty <- style_percent(percent_vals, symbol = TRUE), NA)
  expect_warning(sty, NA)
  expect_equal(sty, c(NA, "0%", "<0.1%", "<0.1%", "0.5%", "1.0%", "10%", "45%", "99%", "145%"))


  expect_error(sty2 <- style_percent(percent_vals, symbol = TRUE, digits = 1), NA)
  expect_warning(sty2, NA)
  expect_equal(sty2, c(NA, "0%", "<0.01%", "0.01%", "0.50%", "1.00%", "10.0%", "45.4%", "99.0%", "145.0%"))

  vec2 <- c(one = 0.99, two = 0.0005)
  expect_equal(
    attributes(vec2),
    style_percent(vec2) %>% attributes()
  )
})


test_that("<0 returns NA", {
  expect_true(is.na(style_percent(-1)))
})
