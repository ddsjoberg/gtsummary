skip_on_cran()

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
  survival::coxph(survival::Surv(ttdeath, death) ~ trt, trial) %>%
  tbl_regression(
    exponentiate = TRUE,
    label = list("trt" ~ "Treatment (unadjusted)")
  )

t4 <-
  survival::coxph(survival::Surv(ttdeath, death) ~ trt + grade + stage + marker, trial) %>%
  tbl_regression(
    include = "trt",
    exponentiate = TRUE,
    label = list("trt" ~ "Treatment (adjusted)")
  )

row1 <- tbl_merge(list(t1, t3), tab_spanner = c("Tumor Response", "Death"))
row2 <- tbl_merge(list(t2, t4))

test_that("Stacking tbl_regression objects", {
  expect_snapshot(
    tbl_stack(list(t1, t2), group_header = c("Group 1", "Group 2")) %>%
      as.data.frame()
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
  expect_snapshot(
    tbl_stack(list(row1, row2)) %>%
      as.data.frame()
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
  expect_snapshot(zz %>% as.data.frame())

  # no error if the list is named
  lst_summary <- list(yy, tt) %>% set_names("one", "two")
  expect_snapshot(
    tbl_stack(lst_summary, group_header = c("Group 1", "Group 2")) %>%
      as.data.frame()
  )

  # complex row-specific formatting is maintained
  tbl <-
    trial %>%
    select(age, response, trt) %>%
    tbl_summary(
      by = trt,
      missing = "no"
    ) %>%
    add_difference()

  expect_equal(
    tbl_stack(
      list(
        tbl,
        tbl %>% modify_fmt_fun(p.value ~ purrr::partial(style_sigfig, digits = 3))
      )
    ) %>%
      as_tibble(col_labels = FALSE) %>%
      dplyr::pull(p.value),
    c("0.8", "0.6", "0.834", "0.637")
  )
})
