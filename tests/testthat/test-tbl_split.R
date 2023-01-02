skip_on_cran()

test_that("no errors/warnings with standard use", {
  t1 <- tbl_summary(trial)

  expect_snapshot(tbl_split(t1, variables = age) %>% purrr::map(as_tibble))
  expect_warning(tbl_split(t1, variables = age), NA)
})
