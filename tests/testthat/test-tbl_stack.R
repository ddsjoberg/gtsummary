skip_on_cran()
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
    tbl_stack(list(t1, t2), group_header = c("Group 1", "Group 2")),
    NA
  )

  # must pass items as list
  expect_error(
    tbl_stack(t1, t2),
    NULL
  )

  # must pass acceptable objects
  expect_error(
    tbl_stack(list(mtcars)),
    NULL
  )
})

test_that("Stacking tbl_merge objects", {
  expect_error(
    tbl_stack(list(row1, row2)),
    NA
  )
})

test_that("Stacking tbl_summary objects", {
  yy <- tbl_summary(trial, by = response) %>%
    add_p() %>%
    add_q()
  tt <- tbl_summary(trial, by = trt) %>%
    add_p() %>%
    add_q()

  expect_error(
    zz <- tbl_stack(list(yy, tt)),
    NA
  )

  # no error if the list is named
  lst_summary <- list(yy, tt) %>% set_names("one", "two")
  expect_error(
    tbl_stack(lst_summary, group_header = c("Group 1", "Group 2")),
    NA
  )
})
