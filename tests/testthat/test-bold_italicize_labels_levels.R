skip_on_cran()

tbl <- trial %>%
  tbl_summary(by = trt) %>%
  add_p() %>%
  add_q()


tbl_cross_ex <- trial %>%
  tbl_cross(row = trt, col = response)

tbl_uv_ex1 <-
  tbl_uvregression(
    trial[c("response", "age", "grade")],
    method = glm,
    y = response,
    method.args = list(family = binomial),
    exponentiate = TRUE
  )


test_that("tab_style: bold and italicize", {
  expect_snapshot(
    tbl %>%
      bold_labels() %>%
      bold_levels() %>%
      italicize_labels() %>%
      italicize_levels() %>%
      bold_p() %>%
      bold_p(q = TRUE, t = 0.2) %>%
      as.data.frame()
  )

  expect_snapshot(
    tbl_cross_ex %>%
      bold_labels() %>%
      bold_levels() %>%
      italicize_labels() %>%
      italicize_levels() %>%
      as.data.frame()
  )

  expect_snapshot(
    tbl_uv_ex1 %>%
      bold_labels() %>%
      bold_levels() %>%
      italicize_labels() %>%
      italicize_levels() %>%
      as.data.frame()
  )

  expect_warning(
    tbl %>%
      bold_labels() %>%
      bold_levels() %>%
      italicize_labels() %>%
      italicize_levels() %>%
      bold_p() %>%
      bold_p(q = TRUE, t = 0.2),
    NA
  )
})


test_that("error when non-gtsummary object passed", {
  expect_error(
    mtcars %>%
      bold_labels(),
    NULL
  )
  expect_error(
    mtcars %>%
      bold_levels(),
    NULL
  )
  expect_error(
    mtcars %>%
      italicize_labels(),
    NULL
  )
  expect_error(
    mtcars %>%
      italicize_levels(),
    NULL
  )
})
