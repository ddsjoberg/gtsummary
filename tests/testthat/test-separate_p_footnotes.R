skip_on_cran()

gts_tbl <-
  trial %>%
  select(trt, age, grade) %>%
  tbl_summary(by = trt) %>%
  add_p()

test_that("no errors/warnings with standard use", {
  expect_snapshot(separate_p_footnotes(gts_tbl) %>% render_as_html())
  expect_error(separate_p_footnotes(mtcars))

  expect_error(tbl_split(gts_tbl), NA)
  expect_warning(tbl_split(gts_tbl), NA)
})
