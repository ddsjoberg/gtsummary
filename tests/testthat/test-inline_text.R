context("test-inline_text")


# inline_text.tbl_summary tests --------------

test_inline1 <- trial %>% tbl_summary()
test_inline2 <- trial %>% tbl_summary(by = "trt")
test_inline2b <- trial %>%
  tbl_summary(by = "trt") %>%
  add_p()

test_that("inline_text.tbl_summary: no by", {
  expect_error(
    inline_text(test_inline1, variable = "age"),
    NA
  )
  expect_warning(
    inline_text(test_inline1, variable = "age"),
    NA
  )
  expect_error(
    inline_text(test_inline1, variable = "stage", level = "T1"),
    NA
  )
  expect_warning(
    inline_text(test_inline1, variable = "stage", level = "T1"),
    NA
  )
})

test_that("inline_text.tbl_summary: with by", {
  expect_error(
    inline_text(test_inline2, variable = "age", column = "Placebo"),
    NA
  )
  expect_warning(
    inline_text(test_inline2, variable = "age", column = "Placebo"),
    NA
  )
  expect_error(
    inline_text(test_inline2, variable = "stage", level = "T1", column = "Placebo"),
    NA
  )
  expect_warning(
    inline_text(test_inline2, variable = "stage", level = "T1", column = "Placebo"),
    NA
  )
  expect_error(
    inline_text(test_inline2b, variable = "stage", column = "p.value"),
    NA
  )
  expect_warning(
    inline_text(test_inline2b, variable = "stage", column = "p.value"),
    NA
  )
})


test_that("inline_text.tbl_summary: with by -  expect errors", {
  expect_error(
    inline_text(test_inline2, variable = "age", column = "Pla5cebo"),
    "No column selected.*"
  )
  expect_error(
    inline_text(test_inline2, variable = "stage", level = "Tsdfgsdfg1", column = "Placebo"),
    "Is the variable level spelled correctly.*"
  )

  expect_error(
    inline_text(test_inline2, variable = "st55age", level = "T1", column = "Placebo"),
    "Is the variable name spelled correctly.*"
  )
})


# inline_text.regression tests --------------

test_inline3 <- lm(marker ~ age + stage, trial) %>% tbl_regression()
test_inline4 <- glm(response ~ trt + age + stage, trial, family = binomial) %>%
  tbl_regression(exponentiate = TRUE)

test_that("inline_text.regression", {
  expect_error(
    inline_text(test_inline3, variable = "age"),
    NA
  )
  expect_warning(
    inline_text(test_inline3, variable = "age"),
    NA
  )
  expect_error(
    inline_text(test_inline4, variable = "stage", level = "T2"),
    NA
  )
  expect_warning(
    inline_text(test_inline4, variable = "stage", level = "T2"),
    NA
  )
})


test_that("inline_text.regression -  expect errors", {
  expect_error(
    inline_text(test_inline3, variable = "stage", level = "Tsdfgsdfg1"),
    "Is the variable level spelled correctly.*"
  )

  expect_error(
    inline_text(test_inline3, variable = "st55age"),
    "Is the variable name spelled correctly.*"
  )
})

# inline_text.tbl_survival tests  --------------
library(survival)
test_inline_surv_strata <-
  survfit(Surv(ttdeath, death) ~ trt, trial) %>%
  tbl_survival(
    times = c(12, 24),
    time_label = "{time} Months"
  )
test_inline_surv_nostrata <-
  survfit(Surv(ttdeath, death) ~ 1, trial) %>%
  tbl_survival(
    times = c(12, 24),
    time_label = "{time} Months"
  )

test_inline_surv_strata2 <-
  survfit(Surv(ttdeath, death) ~ trt, trial) %>%
  tbl_survival(
    probs = c(0.2, 0.5)
  )
test_inline_surv_nostrata2 <-
  survfit(Surv(ttdeath, death) ~ 1, trial) %>%
  tbl_survival(
    probs = c(0.2, 0.5)
  )

# test tbl_survival with strata
test_that("inline_text.tbl_survival - with strata", {
  expect_error(
    inline_text(test_inline_surv_strata, strata = "Drug", time = 24),
    NA
  )
  expect_error(
    inline_text(test_inline_surv_strata2, strata = "Drug", prob = 0.2),
    NA
  )
  expect_message(
    inline_text(test_inline_surv_strata, strata = "Drug", time = 30),
    "Specified 'time'*"
  )
  expect_error(
    inline_text(test_inline_surv_strata, strata = "Drug", time = -2),
    "Must specify a positive 'time'."
  )
  expect_error(
    inline_text(test_inline_surv_strata, strata = NULL, time = 24),
    "Must specify one of the following strata:*"
  )
  expect_error(
    inline_text(test_inline_surv_strata, strata = "Drururuug", time = 24),
    "Is the strata name spelled correctly*"
  )
})

# test tbl_survival with no strata
test_that("inline_text.tbl_survival - no strata", {
  expect_error(
    inline_text(test_inline_surv_nostrata, time = 24),
    NA
  )
  expect_error(
    inline_text(test_inline_surv_nostrata2, prob = 0.2),
    NA
  )
  expect_warning(
    inline_text(test_inline_surv_nostrata, strata = "Drug", time = 24),
    "Ignoring strata =*"
  )
})
