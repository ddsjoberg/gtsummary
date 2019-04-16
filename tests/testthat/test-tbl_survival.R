context("test-tbl_survival")

test_that("no errors/warnings with stratified variable", {
  s1 <- survfit(Surv(ttdeath, death) ~ trt, trial)
  expect_error(
      tbl_survival(
        s1,
        times = c(12, 24)
      ),
    NA)
  expect_warning(
      tbl_survival(
        s1,
        times = c(12, 24)
      ),
    NA)
})

test_that("no errors/warnings with no stratified variable", {
  s2 <- survfit(Surv(ttdeath, death) ~ 1, trial)
  expect_error(
      tbl_survival(
        s2,
        times = c(12, 24)
      ),
    NA)
  expect_warning(
      tbl_survival(
        s2,
        times = 1:2
      ),
    NA)
})
