context("test-tbl_stack")
library(survival)
t1 <-
  glm(response ~ trt, trial, family = binomial) %>%
  tbl_regression(
    exponentiate = TRUE,
    label = list("trt" ~ "Treatment (unadjusted)")
  )

t2 <-
  glm(response ~ trt + grade + stage + marker, trial, family = binomial) %>%
  tbl_regression(
    include = "trt",
    exponentiate = TRUE,
    label = list("trt" ~ "Treatment (adjusted)")
  )

t3 <-
  coxph(Surv(ttdeath, death) ~ trt, trial) %>%
  tbl_regression(
    exponentiate = TRUE,
    label = list("trt" ~ "Treatment (unadjusted)")
  )

t4 <-
  coxph(Surv(ttdeath, death) ~ trt + grade + stage + marker, trial) %>%
  tbl_regression(
    include = "trt",
    exponentiate = TRUE,
    label = list("trt" ~ "Treatment (adjusted)")
  )

row1 <- tbl_merge(list(t1, t3), tab_spanner = c("Tumor Response", "Death"))
row2 <- tbl_merge(list(t2, t4))

test_that("Stacking tbl_regression objects", {
  expect_error(
    tbl_stack(list(t1, t2)),
    NA
  )

  # must pass items as list
  expect_error(
    tbl_stack(t1, t2),
    "*"
  )

  # must pass more than 1 item
  expect_error(
    tbl_stack(list(t1)),
    "*"
  )

  # must pass acceptable objects
  expect_error(
    tbl_stack(list(mtcars)),
    "*"
  )
})

test_that("Stacking tbl_merge objects", {
  expect_error(
    tbl_stack(list(row1, row2)),
    NA
  )
})

test_that("Stacking tbl_summary objects", {
  yy <- tbl_summary(trial, by = response) %>% add_p() %>% add_q()
  tt <- tbl_summary(trial, by = trt) %>% add_p() %>% add_q()

  expect_error(
    zz <- tbl_stack(list(yy, tt)),
    NA
  )
})
