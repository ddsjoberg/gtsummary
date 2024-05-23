test_that("style_pvalue() works", {
  pvals <- c(
    1.5, 1, 0.999, 0.5, 0.25, 0.2, 0.12, 0.10, 0.06,
    0.03, 0.002, 0.0002, 0.00002, -1
  )

  expect_equal(
    style_pvalue(pvals),
    c(NA   ,    ">0.9" ,  ">0.9"  , "0.5"   , "0.3"  ,  "0.2"  ,  "0.12" ,  "0.10" ,  "0.060" , "0.030",  "0.002" , "<0.001", "<0.001", NA)
  )
  expect_equal(
    style_pvalue(pvals, digits = 1, prepend_p = TRUE),
    c(NA   ,     "p>0.9" ,  "p>0.9",   "p=0.5",   "p=0.3",   "p=0.2"  , "p=0.12"  ,"p=0.10" , "p=0.060" ,"p=0.030" ,"p=0.002", "p<0.001", "p<0.001", NA)
  )

  expect_equal(
    style_pvalue(pvals, digits = 2),
    c(NA, '>0.99', '>0.99', '0.50', '0.25', '0.20', '0.12', '0.10', '0.060', '0.030', '0.002', '<0.001', '<0.001', NA)
  )
  expect_equal(
    style_pvalue(pvals, digits = 2, prepend_p = TRUE),
    c(NA, 'p>0.99', 'p>0.99', 'p=0.50', 'p=0.25', 'p=0.20', 'p=0.12', 'p=0.10', 'p=0.060', 'p=0.030', 'p=0.002', 'p<0.001', 'p<0.001', NA)
  )

  expect_equal(
    style_pvalue(pvals, digits = 3),
    c(NA, '>0.999', '0.999', '0.500', '0.250', '0.200', '0.120', '0.100', '0.060', '0.030', '0.002', '<0.001', '<0.001', NA)
  )
  expect_equal(
    style_pvalue(pvals, digits = 3, prepend_p = TRUE),
    c(NA, 'p>0.999', 'p=0.999', 'p=0.500', 'p=0.250', 'p=0.200', 'p=0.120', 'p=0.100', 'p=0.060', 'p=0.030', 'p=0.002', 'p<0.001', 'p<0.001', NA)
  )

  vec2 <- c(one = 0.99, two = 0.0005)
  expect_equal(
    attributes(vec2),
    style_pvalue(vec2) %>% attributes()
  )
})


test_that("style_pvalue() works NA, <0, and >1 returns NA", {
  expect_true(is.na(style_pvalue(NA)))
  expect_true(is.na(style_pvalue(-1)))
  expect_true(is.na(style_pvalue(2)))
})

test_that("style_pvalue() messaging", {
  expect_snapshot(
    error = TRUE,
    style_pvalue(0.05, digits = 8)
  )
})
