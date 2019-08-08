context("test-add_stat_label")

test_that("no errors/warnings with standard use", {
  expect_error(mtcars %>% tbl_summary(by = am) %>% add_stat_label(), NA)
  expect_warning(mtcars %>% tbl_summary(by = am) %>% add_stat_label(), NA)

  expect_error(mtcars %>% tbl_summary(by = am) %>% add_stat_label() %>% add_p(), NA)
  expect_warning(mtcars %>% tbl_summary(by = am) %>% add_stat_label() %>% add_p(), NA)

  expect_error(mtcars %>% tbl_summary(by = am) %>% add_overall() %>% add_stat_label(), NA)
  expect_warning(mtcars %>% tbl_summary(by = am) %>% add_overall() %>% add_stat_label(), NA)
})
