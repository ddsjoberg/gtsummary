test_that("style_number() works", {
  expect_equal(
    style_number(c(-0.5, 0.5)),
    c("-1", "1")
  )

  expect_equal(
    style_number(c(-1.25, 1.25), digits = 1),
    c("-1.3", "1.3")
  )

  expect_equal(
    style_number(c(-1.25, 1.255, 0.4999999), digits = c(1, 2, 0)),
    c("-1.3", "1.26", "0")
  )
  expect_equal(
    (10 ^ (1:9) + 0.5) %>% style_number(),
    c("11", "101", "1,001", "10,001", "100,001", "1,000,001", "10,000,001", "100,000,001", "1,000,000,001")
  )
  expect_equal(
    (10 ^ -(1:9) / 2) %>% style_number(digits = 1:9),
    c("0.1", "0.01", "0.001", "0.0001", "0.00001", "0.000001", "0.0000001", "0.00000001", "0.000000001")
  )

  expect_equal(
    style_number(c(-2.5, 2.5, 1000002.5, -1000002.5, 1000000002.5, -1000000002.5)),
    c("-3", "3", "1,000,003", "-1,000,003", "1,000,000,003", "-1,000,000,003")
  )

  expect_equal(
    style_number(c(-1:1, NA), digits = 1, prefix = "$", suffix = "*"),
    c("$-1.0*", "$0.0*", "$1.0*",  NA)
  )

  expect_error(
    style_number(4, prefix = letters),
    "must be strings."
  )
})
