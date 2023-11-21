skip_on_cran()
skip_if_not(broom.helpers::.assert_package("flextable", pkg_search = "gtsummary", boolean = TRUE))

test_that("tbl_summary", {
  expect_error(tbl <- tbl_summary(trial) %>% as_flex_table(), NA)
  expect_warning(tbl_summary(trial) %>% as_flex_table(), NA)
  expect_snapshot(tbl)

  expect_error(
    tbl <-
      tbl_summary(trial[c("trt", "age")]) %>%
      modify_table_styling(columns = label, footnote = "test footnote", rows = variable == "age") %>%
      as_flex_table(),
    NA
  )
  expect_snapshot(tbl)
})

test_that("tbl_summary", {
  expect_error(tbl_summary(trial) %>% as_flex_table(return_calls = TRUE), NA)
  expect_warning(tbl_summary(trial) %>% as_flex_table(return_calls = TRUE), NA)
})

test_that("tbl_cross", {
  expect_error(
    tbl <-
      tbl_cross(trial, row = trt, col = response) %>%
      as_flex_table(),
    NA
  )
  expect_snapshot(tbl)
})

test_that("tbl_regression", {
  expect_error(tbl <- lm(marker ~ age, trial) %>% tbl_regression() %>% as_flex_table(), NA)
  expect_warning(lm(marker ~ age, trial) %>% tbl_regression() %>% as_flex_table(), NA)
  expect_snapshot(tbl)
})

test_that("tbl_uvregression", {
  expect_error(tbl <- trial %>% tbl_uvregression(method = lm, y = age) %>% as_flex_table(), NA)
  expect_warning(trial %>% tbl_uvregression(method = lm, y = age) %>% as_flex_table(), NA)
  expect_snapshot(tbl)
})

test_that("tbl_survfit", {
  skip_if_not(broom.helpers::.assert_package("survival", pkg_search = "gtsummary", boolean = TRUE))
  fit1 <- survival::survfit(survival::Surv(ttdeath, death) ~ trt, trial)

  expect_error(tbl <- tbl_survfit(fit1, times = c(12, 24), label_header = "{time} Months") %>% as_flex_table(), NA)
  expect_warning(tbl_survfit(fit1, times = c(12, 24), label_header = "{time} Months") %>% as_flex_table(), NA)
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

  expect_error(tbl <- as_flex_table(tbl_merge_ex1), NA)
  expect_warning(as_flex_table(tbl_merge_ex1), NA)
  expect_snapshot(tbl)

  tbl_stack_ex1 <-
    tbl_stack(
      tbls = list(t1, t2),
      group_header = c("**Tumor Response**", "**Time to Death**")
    )

  expect_error(tbl <- as_flex_table(tbl_merge_ex1), NA)
  expect_warning(as_flex_table(tbl_merge_ex1), NA)
  expect_snapshot(tbl)
})



test_that("indent2", {
  expect_error(
    trial %>%
      select(age) %>%
      tbl_summary() %>%
      modify_table_styling(
        columns = label,
        rows = variable == "age" & row_type != "label",
        text_format = "indent2"
      ) %>%
      as_flex_table(),
    NA
  )
})


test_that("source notes", {
  # this is a bad test, because it does not actually test that the footnote/source note is accurate
  expect_snapshot(
    mtcars %>%
      tbl_cross(row = vs, col = am,
                margin = "col") %>%
      add_p(source_note = TRUE) %>%
      as_flex_table()
  )
})
