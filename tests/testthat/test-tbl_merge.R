context("test-tbl_merge")

library(survival)
t0 <-
  trial %>%
  dplyr::select(response, trt, grade, age) %>%
  tbl_uvregression(
    method = glm,
    y = response,
    method.args = list(family = binomial),
    exponentiate = TRUE,
    label = list(trt = "Treatment", grade = "Grade", age = "Age")
  )

t1 <-
  glm(response ~ trt + grade + age, trial, family = binomial) %>%
  tbl_regression(
    label = list(trt = "Treatment", grade = "Grade", age = "Age"),
    exponentiate = TRUE
  )
t2 <-
  coxph(Surv(ttdeath, death) ~ trt + grade + age, trial) %>%
  tbl_regression(
    label = list(trt = "Treatment", grade = "Grade", age = "Age"),
    exponentiate = TRUE
  )

t3 <-
  tbl_merge(
    tbls = list(t0, t1, t2),
    tab_spanner = c("UVA Tumor Respons", "MVA Tumor Response", "Time to Death")
  )

test_that("no errors/warnings with standard use", {
  expect_error(t3, NA)
  expect_warning(t3, NA)
})

test_that("number of rows the same after joining", {
  expect_true(nrow(t0$table_body) == nrow(t3$table_body))
  expect_true(nrow(t1$table_body) == nrow(t3$table_body))
  expect_true(nrow(t2$table_body) == nrow(t3$table_body))
})
