context("test-add_n")

t1 <- trial %>% fmt_table1()
t2 <- trial %>% fmt_table1(by = "trt")
test_that("no errors/warnings with standard use", {
  expect_error(t1 %>% add_n(), NA)
  expect_error(t2 %>% add_n(), NA)
  expect_warning(t1 %>% add_n(), NA)
  expect_warning(t2 %>% add_n(), NA)

  expect_error(t1 %>% add_n(missing = TRUE), NA)
  expect_error(t2 %>% add_n(missing = TRUE), NA)
  expect_warning(t1 %>% add_n(missing = TRUE), NA)
  expect_warning(t2 %>% add_n(missing = TRUE), NA)
})
