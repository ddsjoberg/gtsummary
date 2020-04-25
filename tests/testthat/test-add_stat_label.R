context("test-add_stat_label")

test_that("no errors/warnings with standard use", {
  tbl <- mtcars %>% tbl_summary(by = am)
  expect_error(tbl %>% add_stat_label(), NA)
  expect_warning(tbl %>% add_stat_label(), NA)

  expect_error(tbl %>% add_stat_label() %>% add_p(), NA)
  expect_warning(tbl %>% add_stat_label() %>% add_p(), NA)

  expect_error(tbl %>% add_overall() %>% add_stat_label(), NA)
  expect_warning(tbl %>% add_overall() %>% add_stat_label(), NA)

  expect_error(tbl %>% add_stat_label(location = "row", label = all_categorical() ~ "no. (%)"), NA)
  expect_warning(tbl %>% add_stat_label(location = "row", label = all_categorical() ~ "no. (%)"), NA)
})
