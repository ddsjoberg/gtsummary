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

  vec2 <- c(one = 0.99, two = 0.0005)
  expect_equal(
    attributes(vec2),
    style_sigfig(vec2) |> attributes()
  )
})
