test_that("style_ratio() works", {
  vec <- c(0.99, 0.999, 1.1)
  expect_equal(
    style_ratio(vec),
    c("0.99", "1.00", "1.10")
  )

  expect_equal(
    seq(0.999, 1.001, by = 0.0005) |> style_ratio(digits = 3),
    c("0.999", "1.000", "1.000", "1.001", "1.001")
  )
  expect_equal(
    -seq(0.999, 1.001, by = 0.0005) |> style_ratio(digits = 3),
    c("-0.999", "-1.000", "-1.000", "-1.001", "-1.001")
  )

  expect_false(
    style_ratio(0.99) == "1.0"
  )

  vec2 <- c(one = 0.99, two = 1.1)
  expect_equal(
    attributes(vec2),
    style_ratio(vec2) %>% attributes()
  )

  expect_equal(
    style_ratio(c(0, 1, 11, NA), digits = 2, prefix = "$", suffix = "*"),
    c("$0.00*", "$1.00*", "$11.0*",  NA)
  )
})
