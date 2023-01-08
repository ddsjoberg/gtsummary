skip_on_cran()
skip_if_not(broom.helpers::.assert_package("kableExtra", pkg_search = "gtsummary", boolean = TRUE))

test_that("tbl_summary", {
  expect_error(tbl <- tbl_summary(trial) %>% as_kable_extra(format = "latex"), NA)
  expect_warning(tbl_summary(trial) %>% as_kable_extra(), NA)
  expect_snapshot(tbl)

  expect_error(
    tbl <-
      tbl_summary(trial[c("trt", "age")]) %>%
      modify_table_styling(columns = label, footnote = "test footnote", rows = variable == "age") %>%
      as_kable_extra(format = "latex"),
    NA
  )
  expect_snapshot(tbl)
})

test_that("tbl_summary", {
  expect_error(tbl_summary(trial) %>% as_kable_extra(return_calls = TRUE), NA)
  expect_warning(tbl_summary(trial) %>% as_kable_extra(return_calls = TRUE), NA)
})

test_that("tbl_cross", {
  expect_error(tbl <- tbl_cross(trial, trt, grade) %>% as_kable_extra(format = "latex"), NA)
  expect_snapshot(tbl)
})

test_that("tbl_regression", {
  expect_error(tbl <- lm(marker ~ age, trial) %>% tbl_regression() %>% as_kable_extra(format = "latex"), NA)
  expect_warning(lm(marker ~ age, trial) %>% tbl_regression() %>% as_kable_extra(), NA)
  expect_snapshot(tbl)
})

test_that("tbl_uvregression", {
  expect_error(tbl <- trial %>% tbl_uvregression(method = lm, y = age) %>% as_kable_extra(format = "latex"), NA)
  expect_warning(trial %>% tbl_uvregression(method = lm, y = age) %>% as_kable_extra(), NA)
  expect_snapshot(tbl)
})

test_that("tbl_survfit", {
  skip_if_not(broom.helpers::.assert_package("survival", pkg_search = "gtsummary", boolean = TRUE))
  fit1 <- survival::survfit(survival::Surv(ttdeath, death) ~ trt, trial)

  expect_error(tbl <- tbl_survfit(fit1, times = c(12, 24), label_header = "{time} Months") %>% as_kable_extra(format = "latex"), NA)
  expect_warning(tbl_survfit(fit1, times = c(12, 24), label_header = "{time} Months") %>% as_kable_extra(), NA)
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

  expect_error(tbl <- tbl_merge_ex1 %>% as_kable_extra(format = "latex"), NA)
  expect_warning(tbl_merge_ex1 %>% as_kable_extra(), NA)
  expect_snapshot(tbl)
  expect_error(tbl <- tbl_stack_ex1 %>% as_kable_extra(format = "latex"), NA)
  expect_warning(tbl_stack_ex1 %>% as_kable_extra(), NA)
  expect_snapshot(tbl)

  expect_error(tbl <- tbl_stack_ex1 %>% as_kable_extra(format = "latex"), NA)
  expect_warning(tbl_stack_ex1 %>% as_kable_extra(format = "latex"), NA)
  expect_snapshot(tbl)

  expect_error(tbl <- tbl_stack_ex1 %>% bold_labels() %>% as_kable_extra(escape = TRUE, format = "latex"), NA)
  expect_warning(tbl_stack_ex1 %>% bold_labels() %>% as_kable_extra(escape = TRUE, format = "latex"), NA)
  expect_snapshot(tbl)

  expect_error(tbl <- tbl_stack_ex1 %>% as_kable_extra(escape = TRUE, format = "latex"), NA)
  expect_warning(tbl_stack_ex1 %>% as_kable_extra(escape = TRUE, format = "latex"), NA)
  expect_snapshot(tbl)

  expect_error(tbl <- tbl_stack_ex1 %>% as_kable_extra(format = "html"), NA)
  expect_warning(tbl_stack_ex1 %>% as_kable_extra(format = "html"), NA)
  expect_snapshot(tbl)

  expect_error(tbl <- tbl_stack_ex1 %>% bold_labels() %>% as_kable_extra(escape = TRUE, format = "html"), NA)
  expect_warning(tbl_stack_ex1 %>% bold_labels() %>% as_kable_extra(escape = TRUE, format = "html"), NA)
  expect_snapshot(tbl)
})

test_that("indent2", {
  expect_error(
    tbl <-
      trial %>%
      select(age) %>%
      tbl_summary() %>%
      modify_table_styling(
        columns = label,
        rows = variable == "age" & row_type != "label",
        text_format = "indent2"
      ) %>%
      as_kable_extra(format = "latex"),
    NA
  )
  expect_snapshot(tbl)
})

test_that("latex-column-alignment", {
  t1 <- trial %>%
    select(trt, age) %>%
    tbl_summary(
      by = trt
    )

  t2 <- trial %>%
    select(trt, grade) %>%
    tbl_summary(
      by = trt
    )

  tstack <- tbl_stack(list(t1, t2)) %>%
    modify_header(
      all_stat_cols() ~ "**{level}**\nN = {n}"
    ) %>%
    as_kable_extra(format = "latex")

  expect_snapshot(tstack)
})
