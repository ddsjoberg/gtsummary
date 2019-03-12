context("test-inline_text")


test_inline1 <- trial %>% tbl_summary()
test_inline2 <- trial %>% tbl_summary(by = "trt")
test_inline2b <- trial %>% tbl_summary(by = "trt") %>% add_comparison()

test_that("inline_text.tbl_summary: no by", {
  expect_error(
    inline_text(test_inline1, variable = "age"),
    NA
  )
  expect_warning(
    inline_text(test_inline1, variable = "age"),
    NA
  )
  expect_error(
    inline_text(test_inline1, variable = "stage", level = "T1"),
    NA
  )
  expect_warning(
    inline_text(test_inline1, variable = "stage", level = "T1"),
    NA
  )
})

test_that("inline_text.tbl_summary: with by", {
  expect_error(
    inline_text(test_inline2, variable = "age", column = "Placebo"),
    NA
  )
  expect_warning(
    inline_text(test_inline2, variable = "age", column = "Placebo"),
    NA
  )
  expect_error(
    inline_text(test_inline2, variable = "stage", level = "T1", column = "Placebo"),
    NA
  )
  expect_warning(
    inline_text(test_inline2, variable = "stage", level = "T1", column = "Placebo"),
    NA
  )
  expect_error(
    inline_text(test_inline2b, variable = "stage", column = "pvalue"),
    NA
  )
  expect_warning(
    inline_text(test_inline2b, variable = "stage", column = "pvalue"),
    NA
  )
})

test_that("inline_text.tbl_summary: with by -  expect errors", {
  expect_error(
    inline_text(test_inline2, variable = "age", column = "Pla5cebo"),
    "No column selected.*"
  )
  expect_error(
    inline_text(test_inline2, variable = "stage", level = "Tsdfgsdfg1", column = "Placebo"),
    "Is the variable level spelled correctly.*"
  )

  expect_error(
    inline_text(test_inline2, variable = "st55age", level = "T1", column = "Placebo"),
    "Is the variable name spelled correctly.*"
  )

})

test_inline3 <- lm(marker ~ age + stage, trial) %>% tbl_regression()
test_inline4 <- glm(response ~ trt + age + stage, trial, family = binomial) %>%
  tbl_regression(exponentiate = TRUE)

test_that("inline_text.regression", {
  expect_error(
    inline_text(test_inline3, variable = "age"),
    NA
  )
  expect_warning(
    inline_text(test_inline3, variable = "age"),
    NA
  )
  expect_error(
    inline_text(test_inline4, variable = "stage", level = "T2"),
    NA
  )
  expect_warning(
    inline_text(test_inline4, variable = "stage", level = "T2"),
    NA
  )
})


test_that("inline_text.regression -  expect errors", {
  expect_error(
    inline_text(test_inline3, variable = "stage", level = "Tsdfgsdfg1"),
    "Is the variable level spelled correctly.*"
  )

  expect_error(
    inline_text(test_inline3, variable = "st55age"),
    "Is the variable name spelled correctly.*"
  )

})
