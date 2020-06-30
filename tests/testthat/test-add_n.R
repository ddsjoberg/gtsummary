context("test-add_n")
testthat::skip_on_cran()

t1 <- trial %>% tbl_summary()
t2 <- trial %>% tbl_summary(by = trt)
test_that("no errors/warnings with standard use", {
  expect_error(t1 %>% add_n(), NA)
  expect_error(t2 %>% add_n(), NA)
  expect_warning(t1 %>% add_n(), NA)
  expect_warning(t2 %>% add_n(), NA)

  expect_error(t1 %>% add_n(last = TRUE), NA)
  expect_error(t2 %>% add_n(last = TRUE), NA)
  expect_warning(t1 %>% add_n(last = TRUE), NA)
  expect_warning(t2 %>% add_n(last = TRUE), NA)

  expect_error(t1 %>% add_n(
    statistic = "{N}{n}{n_miss}{p}{p_miss}",
    footnote = TRUE
  ), NA)
  expect_error(t2 %>% add_n(
    statistic = "{N}{n}{n_miss}{p}{p_miss}",
    footnote = TRUE
  ), NA)
  expect_warning(t1 %>% add_n(
    statistic = "{N}{n}{n_miss}{p}{p_miss}",
    footnote = TRUE
  ), NA)
  expect_warning(t2 %>% add_n(
    statistic = "{N}{n}{n_miss}{p}{p_miss}",
    footnote = TRUE
  ), NA)
})

if (require(survey)) {
  test_that("no errors/warnings with standard use for tbl_svysummary", {
    t <- trial %>%
      svydesign(data = ., ids = ~ 1, weights = ~ 1) %>%
      tbl_svysummary(by = trt)

    expect_error(t %>% add_n(), NA)
    expect_warning(t %>% add_n(), NA)

    expect_error(t %>% add_n(last = TRUE), NA)
    expect_warning(t %>% add_n(last = TRUE), NA)

    t <- Titanic %>%
      as.data.frame() %>%
      svydesign(data = ., ids = ~ 1, weights = ~ Freq) %>%
      tbl_svysummary(by = Survived)

    expect_error(t %>% add_n(
      statistic = "{N}{n}{n_miss}{p}{p_miss}",
      footnote = TRUE
    ), NA)
    expect_warning(t %>% add_n(
      statistic = "{N}{n}{n_miss}{p}{p_miss}",
      footnote = TRUE
    ), NA)
  })

}
