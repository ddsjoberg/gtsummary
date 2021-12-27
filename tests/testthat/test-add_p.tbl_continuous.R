test_that("add_p.tbl_continuous() works", {
  expect_error(
    tbl_continuous(
      data = trial,
      variable = age,
      by = trt,
      include = grade
    ) %>%
      add_p(),
    NA
  )
  expect_error(
    tbl_continuous(
      data = trial,
      variable = age,
      include = grade
    ) %>%
      add_p(),
    NA
  )
  expect_error(
    tbl_continuous(
      data = trial,
      variable = age,
      include = trt
    ) %>%
      add_p(everything() ~ "t.test"),
    NA
  )
  expect_error(
    tbl_continuous(
      data = trial,
      variable = age,
      include = trt
    ) %>%
      add_p(everything() ~ "wilcox.test"),
    NA
  )
  expect_error(
    tbl_continuous(
      data = trial,
      variable = age,
      include = trt
    ) %>%
      add_p(everything() ~ "lme4", group = "stage"),
    NA
  )
})
