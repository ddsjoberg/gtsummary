context("test-sort_p")

test_that("no errors/warnings with standard use after tbl_summary() and add_p()", {
  table1 <- trial %>%
    tbl_summary(by = "trt") %>%
    add_p()

  expect_error(sort_p(table1), NA)
  expect_warning(sort_p(table1), NA)
})


test_that("expect error if no p value in table 1", {
  table1 <- trial %>% tbl_summary(by = "trt")

  expect_error(
    sort_p(table1),
    glue(
      "Before you can sort by p-values, run add_p() to calculate the p-values"
    ),
    fixed = TRUE
  )
})

test_that("expect error if q = TRUE and no q values in table 1", {
  table1 <- trial %>% tbl_summary(by = "trt") %>%
    add_p()

  expect_error(
    sort_p(table1, q = TRUE),
    glue(
      "Before you sort by q-values, run add_q() to calculate the q-values"
    ),
    fixed = TRUE
  )
})

