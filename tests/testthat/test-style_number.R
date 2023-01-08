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
    style_number(c(-1.25, 1.255, 0.499999999), digits = c(1, 2, 0)),
    c("-1.3", "1.26", "0")
  )
})
