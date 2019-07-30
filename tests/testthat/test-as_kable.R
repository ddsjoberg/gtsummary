context("test-as_kable")

test_that("tbl_summary", {
  expect_error(tbl_summary(trial) %>% as_kable(), NA)
  expect_warning(tbl_summary(trial) %>% as_kable(), NA)
})

test_that("tbl_regression", {
  expect_error(lm(marker ~ age, trial) %>% tbl_regression() %>% as_kable(), NA)
  expect_warning(lm(marker ~ age, trial) %>% tbl_regression() %>% as_kable(), NA)
})

test_that("tbl_uvregression", {
  expect_error(trial %>% tbl_uvregression(method = lm, y = age) %>% as_kable(), NA)
  expect_warning(trial %>% tbl_uvregression(method = lm, y = age) %>% as_kable(), NA)
})

test_that("tbl_survival", {
  library(survival)
  fit1 <- survfit(Surv(ttdeath, death) ~ trt, trial)

  expect_error(tbl_survival(fit1, times = c(12, 24), label = "{time} Months") %>% as_kable(), NA)
  expect_warning(tbl_survival(fit1, times = c(12, 24), label = "{time} Months") %>% as_kable(), NA)
})
