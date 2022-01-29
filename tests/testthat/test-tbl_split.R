skip_on_cran()

t1 <- tbl_summary(trial)

test_that("no errors/warnings with standard use", {
  expect_error(t1, NA)
  expect_warning(t1, NA)
  expect_error(tbl_split(t1, variables = age), NA)
  expect_warning(tbl_split(t1, variables = age), NA)
})
