gts_tbl <- trial %>%
  select(trt, age, grade) %>%
  tbl_summary(by = trt) %>%
  add_p()

test_that("no errors/warnings with standard use", {
  expect_error(gts_tbl, NA)
  expect_warning(gts_tbl, NA)
  expect_error(tbl_split(gts_tbl), NA)
  expect_warning(tbl_split(gts_tbl), NA)
})
