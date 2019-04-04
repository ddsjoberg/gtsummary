context("test-tbl_survival")
library(survival)

test_that("no errors/warnings with stratified variable", {
  s1 <- survfit(Surv(ttdeath, death) ~ trt, trial)
  expect_error(
    s1 %>%
      tbl_survival(
        times = 1:2
      ),
    NA)
  expect_warning(
    s1 %>%
      tbl_survival(
        times = 1:2
      ),
    NA)
})

test_that("no errors/warnings with no stratified variable", {
  s2 <- survfit(Surv(ttdeath, death) ~ 1, trial)
  expect_error(
    s2 %>%
      tbl_survival(
        times = 1:2
      ),
    NA)
  expect_warning(
    s2 %>%
      tbl_survival(
        times = 1:2
      ),
    NA)
})
