skip_on_cran()

test_that("tbl_summary", {
  expect_error(tbl <- tbl_summary(trial) %>% as_kable(format = "pipe"), NA)
  expect_warning(tbl_summary(trial) %>% as_kable(), NA)
  expect_snapshot(tbl)
})

test_that("tbl_cross", {
  expect_error(tbl <- tbl_cross(trial, grade, trt) %>% as_kable(format = "pipe"), NA)
  expect_warning(tbl_summary(trial) %>% as_kable(), NA)
  expect_snapshot(tbl)
})

test_that("tbl_summary", {
  expect_error(tbl_summary(trial) %>% as_kable(return_calls = TRUE), NA)
  expect_warning(tbl_summary(trial) %>% as_kable(return_calls = TRUE), NA)
})

test_that("tbl_regression", {
  expect_error(tbl <- lm(marker ~ age, trial) %>% tbl_regression() %>% as_kable(format = "pipe"), NA)
  expect_warning(lm(marker ~ age, trial) %>% tbl_regression() %>% as_kable(), NA)
  expect_snapshot(tbl)

  expect_snapshot(
    with_gtsummary_theme(
      x = theme_gtsummary_journal("qjecon"),
      lm(age ~ marker + response, data = trial) %>%
        tbl_regression() %>%
        as_kable(format = "pipe")
    )
  )
})

test_that("tbl_uvregression", {
  expect_error(tbl <- trial %>% tbl_uvregression(method = lm, y = age) %>% as_kable(format = "pipe"), NA)
  expect_warning(trial %>% tbl_uvregression(method = lm, y = age) %>% as_kable(), NA)
  expect_snapshot(tbl)
})

test_that("tbl_survfit", {
  skip_if_not(broom.helpers::.assert_package("survival", pkg_search = "gtsummary", boolean = TRUE))
  fit1 <- survival::survfit(survival::Surv(ttdeath, death) ~ trt, trial)

  expect_error(tbl <- tbl_survfit(fit1, times = c(12, 24), label_header = "{time} Months") %>% as_kable(format = "pipe"), NA)
  expect_warning(tbl_survfit(fit1, times = c(12, 24), label_header = "{time} Months") %>% as_kable(), NA)
  expect_snapshot(tbl)
})


test_that("tbl_merge/tbl_stack", {
  skip_if_not(broom.helpers::.assert_package("survival", pkg_search = "gtsummary", boolean = TRUE))

  t1 <-
    glm(response ~ trt + grade + age, trial, family = binomial) %>%
    tbl_regression(exponentiate = TRUE)
  t2 <-
    survival::coxph(survival::Surv(ttdeath, death) ~ trt + grade + age, trial) %>%
    tbl_regression(exponentiate = TRUE)
  tbl_merge_ex1 <-
    tbl_merge(
      tbls = list(t1, t2),
      tab_spanner = c("**Tumor Response**", "**Time to Death**")
    )

  tbl_stack_ex1 <-
    tbl_stack(
      tbls = list(t1, t2),
      group_header = c("**Tumor Response**", "**Time to Death**")
    )

  expect_error(tbl <- tbl_merge_ex1 %>% as_kable(format = "pipe"), NA)
  expect_warning(tbl_merge_ex1 %>% as_kable(), NA)
  expect_snapshot(tbl)

  expect_error(tbl <- tbl_stack_ex1 %>% as_kable(format = "pipe"), NA)
  expect_warning(tbl_stack_ex1 %>% as_kable(), NA)
  expect_snapshot(tbl)
})

test_that("No errors replacing default arg values", {
  expect_error(
    trial %>%
      tbl_summary(
        by = trt,
        include = c(age, grade),
        missing = "no"
      ) %>%
      as_kable(col.names = NULL),
    NA
  )
})
