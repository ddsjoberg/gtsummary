context("test-inline_text")


inline1 <- trial %>% tbl_summary()
inline2 <- trial %>% tbl_summary(by = "trt")

test_that("inline_text.tbl_summary: no by", {
  expect_error(
    inline_text(inline1, variable = "age"),
    NA
  )
  expect_warning(
    inline_text(inline1, variable = "age"),
    NA
  )
  expect_error(
    inline_text(inline1, variable = "stage", level = "T1"),
    NA
  )
  expect_warning(
    inline_text(inline1, variable = "stage", level = "T1"),
    NA
  )
})

test_that("inline_text.tbl_summary: with by", {
  expect_error(
    inline_text(inline2, variable = "age", column = "Placebo"),
    NA
  )
  expect_warning(
    inline_text(inline2, variable = "age", column = "Placebo"),
    NA
  )
  expect_error(
    inline_text(inline2, variable = "stage", level = "T1", column = "Placebo"),
    NA
  )
  expect_warning(
    inline_text(inline2, variable = "stage", level = "T1", column = "Placebo"),
    NA
  )
})

inline3 <- lm(marker ~ age + stage, trial) %>% tbl_regression()
inline4 <- glm(response ~ trt + age + stage, trial, family = binomial) %>%
  tbl_regression(exponentiate = TRUE)

test_that("inline_text.regression", {
  expect_error(
    inline_text(inline3, variable = "age"),
    NA
  )
  expect_warning(
    inline_text(inline3, variable = "age"),
    NA
  )
  expect_error(
    inline_text(inline4, variable = "stage", level = "T2"),
    NA
  )
  expect_warning(
    inline_text(inline4, variable = "stage", level = "T2"),
    NA
  )
})
