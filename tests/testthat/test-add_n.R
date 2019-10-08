context("test-add_n")

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

  expect_error(t1 %>% add_n(statistic = "{N}{n}{n_miss}{p}{p_miss}",
                            footnote = TRUE), NA)
  expect_error(t2 %>% add_n(statistic = "{N}{n}{n_miss}{p}{p_miss}",
                            footnote = TRUE), NA)
  expect_warning(t1 %>% add_n(statistic = "{N}{n}{n_miss}{p}{p_miss}",
                              footnote = TRUE), NA)
  expect_warning(t2 %>% add_n(statistic = "{N}{n}{n_miss}{p}{p_miss}",
                              footnote = TRUE), NA)

})


test_that("testing deprecated missing argument", {
  expect_warning(t1 %>% add_n(missing = TRUE), "*")
  expect_error(t1 %>% add_n(missing = TRUE), NA)
  expect_error(t2 %>% add_n(missing = TRUE), NA)
})
