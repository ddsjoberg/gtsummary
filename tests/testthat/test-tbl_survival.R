skip_on_cran()
skip_if_not(broom.helpers::.assert_package("survival", pkg_search = "gtsummary", boolean = TRUE))
library(survival)

test_that("no errors/warnings with stratified variable", {
  s1 <- survfit(Surv(ttdeath, death) ~ trt, trial)
  expect_error(
    tbl1 <-
      tbl_survival(
        s1,
        times = c(12, 24)
      ),
    NA
  )
  expect_error(
    tbl_survival(
      s1,
      probs = c(0.2, 0.4),
      estimate_fun = partial(style_sigfig, digits = 4)
    ),
    NA
  )

  # using with `modify_table_header()`
  expect_error(
    tbl1 %>%
      modify_table_header(
        column = estimate,
        label = "**NEW LABEL**",
        fmt_fun = function(x) style_number(100 * x),
        footnote = "test footnote",
        footnote_abbrev = "SE = Standard Error",
        align = "left",
        missing_emdash = "variable == 'trt'",
        bold = "row_type == 'label'",
        italic = "row_type != 'label'",
      ),
    NA
  )
})

test_that("no errors/warnings with no stratified variable", {
  s2 <- survfit(Surv(ttdeath, death) ~ 1, trial)
  expect_error(
    tbl_survival(
      s2,
      times = c(12, 24)
    ),
    NA
  )
  expect_error(
    tbl_survival(
      s2,
      probs = c(0.2, 0.4),
      estimate_fun = partial(style_sigfig, digits = 4)
    ),
    NA
  )
})
