context("test-add_stat_label")
testthat::skip_on_cran()

test_that("no errors/warnings with standard use", {
  tbl <- mtcars %>% tbl_summary(by = am)
  expect_error(tbl %>% add_stat_label(), NA)
  expect_warning(tbl %>% add_stat_label(), NA)

  expect_error(tbl %>% add_stat_label() %>% add_p(), NA)
  expect_warning(tbl %>% add_stat_label() %>% add_p(), NA)

  expect_error(tbl %>% add_overall() %>% add_stat_label(), NA)
  expect_warning(tbl %>% add_overall() %>% add_stat_label(), NA)

  expect_error(tbl %>% add_stat_label(location = "column", label = all_categorical() ~ "no. (%)"), NA)
  expect_warning(tbl %>% add_stat_label(location = "column", label = all_categorical() ~ "no. (%)"), NA)
})

test_that("no errors/warnings with standard use for continuous2", {
  tbl <- mtcars %>% tbl_summary(by = am, type = all_continuous() ~ "continuous2")
  expect_error(tbl %>% add_stat_label(), NA)
  expect_warning(tbl %>% add_stat_label(), NA)

  expect_error(tbl %>% add_stat_label() %>% add_p(), NA)
  expect_warning(tbl %>% add_stat_label() %>% add_p(), NA)

  expect_error(tbl %>% add_overall() %>% add_stat_label(), NA)
  expect_warning(tbl %>% add_overall() %>% add_stat_label(), NA)

  expect_error(tbl %>% add_stat_label(location = "column", label = all_categorical() ~ "no. (%)"), NA)
  expect_warning(tbl %>% add_stat_label(location = "column", label = all_categorical() ~ "no. (%)"), NA)
})


test_that("no errors/warnings with standard use for tbl_svysummary", {
  tbl <- trial %>%
    survey::svydesign(data = ., ids = ~1, weights = ~1) %>%
    tbl_svysummary(by = trt)

  expect_error(tbl %>% add_stat_label(), NA)
  expect_warning(tbl %>% add_stat_label(), NA)

  expect_error(tbl %>% add_stat_label(location = "column", label = all_categorical() ~ "no. (%)"), NA)
  expect_warning(tbl %>% add_stat_label(location = "column", label = all_categorical() ~ "no. (%)"), NA)
})

test_that("no errors/warnings with standard use for tbl_svysummary with continuous2", {
  tbl <- trial %>%
    survey::svydesign(data = ., ids = ~1, weights = ~1) %>%
    tbl_svysummary(by = trt, type = all_continuous() ~ "continuous2")

  expect_error(tbl %>% add_stat_label(), NA)
  expect_warning(tbl %>% add_stat_label(), NA)

  expect_error(tbl %>% add_stat_label(location = "column", label = all_categorical() ~ "no. (%)"), NA)
  expect_warning(tbl %>% add_stat_label(location = "column", label = all_categorical() ~ "no. (%)"), NA)
})
