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

  expect_equal(
    style_percent(NA, na = "NE"),
    "NE"
  )
  expect_equal(
    style_percent(c(NA, -1, NA, 1), na = "NE", prefix = "*", suffix = "*"),
    c("NE", NA, "NE", "*100*")
  )
  vec2 <- c(one = 0.99, two = 0.0005)
  expect_equal(
    attributes(vec2),
    style_percent(vec2) |> attributes()
  )
})

test_that("style_percent() works with matrix input", {
  vec <- c(0, 0.0001, 0.10, NA)
  mat <- matrix(vec, nrow = 2, dimnames = list(c("r1", "r2"), c("c1", "c2")))
  result <- style_percent(mat)

  expect_true(is.matrix(result))
  expect_equal(dim(result), c(2L, 2L))
  expect_equal(dimnames(result), list(c("r1", "r2"), c("c1", "c2")))
  expect_equal(as.vector(result), style_percent(vec))
})

