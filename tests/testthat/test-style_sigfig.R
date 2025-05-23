test_that("style_sigfig() works", {
  expect_equal(
    c(1.0001, 1, 0.9999, 0.90000) |> style_sigfig(digits = 1),
    c("1", "1", "1", "0.9")
  )

  expect_true(
    style_sigfig(-9.98) == "-10"
  )

  expect_equal(
    style_sigfig(seq(from = 9.94, to = 10, by = 0.01)),
    c("9.9", "10", "10", "10", "10", "10", "10")
  )

  expect_true(
    style_sigfig(0.97, digits = 1) == "1"
  )

  expect_equal(
    style_sigfig(NA, na = "NE"),
    "NE"
  )

  vec2 <- c(one = 0.99, two = 0.0005)
  expect_equal(
    attributes(vec2),
    style_sigfig(vec2) |> attributes()
  )

  expect_equal(
    style_sigfig(c(0, 1, 11, NA), digits = 2, prefix = "$", suffix = "*"),
    c("$0.00*", "$1.0*", "$11*",  NA)
  )

  expect_equal(
    style_sigfig(c(0, 1, NA, 11, NA), digits = 2, prefix = "$", suffix = "*", na = "NE"),
    c("$0.00*", "$1.0*", "NE", "$11*",  "NE")
  )
})
