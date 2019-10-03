context("test-tbl_merge")

library(survival)
library(purrr)
# univariate regression models
t0 <-
  trial %>%
  dplyr::select(response, trt, grade, age) %>%
  tbl_uvregression(
    method = glm,
    y = response,
    method.args = list(family = binomial),
    exponentiate = TRUE,
  )
# MVA logistic regression
t1 <-
  glm(response ~ trt + grade + age, trial, family = binomial) %>%
  tbl_regression(
    exponentiate = TRUE
  )
# MVA cox regression
t2 <-
  coxph(Surv(ttdeath, death) ~ trt + grade + age, trial) %>%
  tbl_regression(
    exponentiate = TRUE
  )



# tbl_stack adjusted model
covars <- c("trt", "age")

# get model covariates adjusted by stage and grade
adj_mods <- map(covars, ~
coxph(
  as.formula(
    paste("Surv(ttdeath, death) ~ grade + ", .x)
  ),
  trial
) %>%
  tbl_regression(
    include = .x,
    exponentiate = TRUE
  ))

# now get stage and grade models adjusted for each other
adj_mods[["grade_mod"]] <- coxph(
  as.formula(
    paste("Surv(ttdeath, death) ~ grade")
  ),
  trial
) %>%
  tbl_regression(
    exponentiate = TRUE
  )

# stack all your adjusted models
t3 <- tbl_stack(adj_mods)


# putting all tables together
t4 <-
  tbl_merge(
    tbls = list(t0, t1, t2, t3),
    tab_spanner = c("UVA Tumor Response", "MVA Tumor Response", "MVA Time to Death", "TTD Adjusted for grade")
  )

test_that("no errors/warnings with standard use", {
  expect_error(t4, NA)
  expect_warning(t4, NA)
})

test_that("number of rows the same after joining", {
  expect_true(nrow(t0$table_body) == nrow(t4$table_body))
  expect_true(nrow(t1$table_body) == nrow(t4$table_body))
  expect_true(nrow(t2$table_body) == nrow(t4$table_body))
  expect_true(nrow(t3$table_body) == nrow(t4$table_body))
})
