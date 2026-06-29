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

test_that("style_sigfig() works with matrix input", {
  vec <- c(0.123, 0.9, 1.1234, NA)
  mat <- matrix(vec, nrow = 2, dimnames = list(c("r1", "r2"), c("c1", "c2")))
  result <- style_sigfig(mat)

  expect_true(is.matrix(result))
  expect_equal(dim(result), c(2L, 2L))
  expect_equal(dimnames(result), list(c("r1", "r2"), c("c1", "c2")))
  expect_equal(as.vector(result), style_sigfig(vec))
})
