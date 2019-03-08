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

