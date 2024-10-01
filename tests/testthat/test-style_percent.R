test_that("style_percent() works", {
  percent_vals <- c(-1, 0, 0.00001, 0.0001, 0.005, 0.01, 0.10, 0.45356, 0.99, 1.45)

  expect_equal(
    style_percent(percent_vals, suffix = "%"),
    c(NA, "0%", "<0.1%", "<0.1%", "0.5%", "1.0%", "10%", "45%", "99%", "145%")
  )

  expect_equal(
    style_percent(percent_vals, suffix = "%", digits = 1),
    c(NA, "0%", "<0.01%", "0.01%", "0.50%", "1.00%", "10.0%", "45.4%", "99.0%", "145.0%")
  )

  vec2 <- c(one = 0.99, two = 0.0005)
  expect_equal(
    attributes(vec2),
    style_percent(vec2) |> attributes()
  )
})

