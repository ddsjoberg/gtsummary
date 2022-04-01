skip_on_cran()

test_that("tbl_summary", {
  expect_error(tbl_summary(trial) %>% as_gt(), NA)

  expect_error(
    tbl_summary(trial[c("trt", "age")]) %>%
      modify_table_styling(columns = label, footnote = "test footnote", rows = variable == "age") %>%
      as_gt(),
    NA
  )
})

test_that("tbl_summary", {
  expect_error(tbl_summary(trial) %>% as_gt(return_calls = TRUE), NA)
})

test_that("tbl_regression", {
  expect_error(lm(marker ~ age, trial) %>% tbl_regression() %>% as_gt(), NA)
})

test_that("tbl_uvregression", {
  expect_error(trial %>% tbl_uvregression(method = lm, y = age) %>% as_gt(), NA)
})

test_that("tbl_survfit", {
  library(survival)
  fit1 <- survfit(Surv(ttdeath, death) ~ trt, trial)

  expect_error(tbl_survfit(fit1, times = c(12, 24), label_header = "**{time} Months**") %>% as_gt(), NA)
  expect_error(as_gt(fit1))
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
      as_gt(),
    NA
  )
})

