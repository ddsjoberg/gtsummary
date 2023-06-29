skip_on_cran()

test_that("tbl_summary", {
  expect_error(tbl <- tbl_summary(trial) %>% as_gt(), NA)
  expect_snapshot(tbl %>% render_as_html())

  expect_error(
    tbl <-
      tbl_summary(trial[c("trt", "age")]) %>%
      modify_table_styling(columns = label, footnote = "test footnote", rows = variable == "age") %>%
      as_gt(),
    NA
  )
  expect_snapshot(tbl %>% render_as_html())
})

test_that("tbl_cross", {
  expect_error(tbl <- tbl_cross(trial, trt, grade) %>% as_gt(), NA)
  expect_snapshot(tbl %>% render_as_html())
})

test_that("tbl_summary", {
  expect_error(tbl_summary(trial) %>% as_gt(return_calls = TRUE), NA)
})

test_that("tbl_regression", {
  expect_error(tbl <- lm(marker ~ age, trial) %>% tbl_regression() %>% as_gt(), NA)
  expect_snapshot(tbl %>% render_as_html())
})

test_that("tbl_uvregression", {
  expect_error(tbl <- trial %>% tbl_uvregression(method = lm, y = age) %>% as_gt(), NA)
  expect_snapshot(tbl %>% render_as_html())
})

test_that("tbl_survfit", {
  skip_if_not(broom.helpers::.assert_package("survival", pkg_search = "gtsummary", boolean = TRUE))

  fit1 <- survival::survfit(survival::Surv(ttdeath, death) ~ trt, trial)

  expect_error(tbl <- tbl_survfit(fit1, times = c(12, 24), label_header = "**{time} Months**") %>% as_gt(), NA)
  expect_snapshot(tbl %>% render_as_html())
  expect_error(as_gt(fit1))
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
      as_gt(),
    NA
  )
  expect_snapshot(tbl %>% render_as_html())
})

test_that("spanning header-column gathering", {
  expect_error(
    tbl <-
      trial %>%
      tbl_summary(
        by = grade,
        include = age
      ) %>%
      modify_spanning_header(c(stat_1, stat_3) ~ "**Testing**") %>%
      as_gt(),
    NA
  )
  expect_snapshot(tbl %>% render_as_html())
})
