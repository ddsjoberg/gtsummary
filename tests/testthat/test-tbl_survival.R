context("test-tbl_survival")

test_that("no errors/warnings with stratified variable", {
  s1 <- survfit(Surv(ttdeath, death) ~ trt, trial)
  expect_error(
      tbl_survival(
        trial,
        Surv(ttdeath, death) ~ trt,
        times = 1:2
      ),
    NA)
  expect_warning(
      tbl_survival(
        trial,
        Surv(ttdeath, death) ~ trt,
        times = 1:2
      ),
    NA)
})

test_that("no errors/warnings with no stratified variable", {
  expect_error(
      tbl_survival(
        trial,
        Surv(ttdeath, death) ~ 1,
        times = 1:2
      ),
    NA)
  expect_warning(
      tbl_survival(
        trial,
        Surv(ttdeath, death) ~ 1,
        times = 1:2
      ),
    NA)
})
