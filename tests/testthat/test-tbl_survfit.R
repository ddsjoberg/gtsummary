context("test-tbl_survfit")
library(survival)

test_that("no errors/warnings with stratified variable", {
  s1 <- survfit(Surv(ttdeath, death) ~ trt, trial)
  expect_error(
    tbl_survfit(
      s1,
      times = c(12, 24)
    ),
    NA
  )
  expect_error(
    tbl_survfit(
      s1,
      times = c(12, 24),
      failure = TRUE
    ),
    NA
  )
  expect_warning(
    tbl_survfit(
      s1,
      times = c(12, 24)
    ),
    NA
  )
  expect_error(
    tbl_survfit(
      s1,
      probs = c(0.2, 0.4),
      estimate_fun = partial(style_sigfig, digits = 4)
    ),
    NA
  )
  expect_warning(
    tbl_survfit(
      s1,
      probs = c(0.2, 0.4),
      estimate_fun = partial(style_sigfig, digits = 4)
    ),
    NA
  )
})

test_that("no errors/warnings with no stratified variable", {
  s2 <- survfit(Surv(ttdeath, death) ~ 1, trial)
  expect_error(
    tbl_survfit(
      s2,
      times = c(12, 24)
    ),
    NA
  )
  expect_warning(
    tbl_survfit(
      s2,
      times = c(12, 24)
    ),
    NA
  )
  expect_error(
    tbl_survfit(
      s2,
      probs = c(0.2, 0.4),
      estimate_fun = partial(style_sigfig, digits = 4)
    ),
    NA
  )
  expect_warning(
    tbl_survfit(
      s2,
      probs = c(0.2, 0.4),
      estimate_fun = partial(style_sigfig, digits = 4)
    ),
    NA
  )
})
