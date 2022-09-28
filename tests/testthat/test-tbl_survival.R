skip_on_cran()
skip_if_not(broom.helpers::.assert_package("survival", pkg_search = "gtsummary", boolean = TRUE))

test_that("tbl_survival() deprecated", {
  lifecycle::expect_defunct(
    survival::survfit(survival::Surv(ttdeath, death) ~ trt, trial) %>%
      tbl_survival()
  )
})
