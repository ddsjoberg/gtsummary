skip_on_cran()

test_that("no errors/warnings with standard use", {
  pvals <- c(
    1.5, 1, 0.999, 0.5, 0.25, 0.2, 0.12, 0.10, 0.06,
    0.03, 0.002, 0.0002, 0.00002, -1
  )
  expect_error(style_pvalue(pvals), NA)
  expect_error(style_pvalue(pvals, digits = 1, prepend_p = TRUE), NA)
  expect_warning(style_pvalue(pvals), NA)
  expect_warning(style_pvalue(pvals, digits = 1, prepend_p = TRUE), NA)

  expect_error(style_pvalue(pvals, digits = 2), NA)
  expect_error(style_pvalue(pvals, digits = 2, prepend_p = TRUE), NA)
  expect_warning(style_pvalue(pvals, digits = 2), NA)
  expect_warning(style_pvalue(pvals, digits = 2, prepend_p = TRUE), NA)

  expect_error(style_pvalue(pvals, digits = 3), NA)
  expect_error(style_pvalue(pvals, digits = 3, prepend_p = TRUE), NA)
  expect_warning(style_pvalue(pvals, digits = 3), NA)
  expect_warning(style_pvalue(pvals, digits = 3, prepend_p = TRUE), NA)

  vec2 <- c(one = 0.99, two = 0.0005)
  expect_equal(
    attributes(vec2),
    style_pvalue(vec2) %>% attributes()
  )
})


test_that("NA, <0, and >1 returns NA", {
  expect_true(is.na(style_pvalue(NA)))
  expect_true(is.na(style_pvalue(-1)))
  expect_true(is.na(style_pvalue(2)))
})
