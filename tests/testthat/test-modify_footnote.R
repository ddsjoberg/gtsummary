skip_on_cran()

test_that("modify_footnote works", {
  tbl_summary <-
    trial %>%
    dplyr::select(trt, age, grade) %>%
    tbl_summary(by = trt)

  expect_snapshot(
    tbl_summary %>%
      modify_footnote(
        update = starts_with("stat_") ~
          "median (IQR) for continuous variables; n (%) categorical variables"
      ) %>%
      render_as_html()
  )

  expect_snapshot(
    tbl_summary %>%
      modify_footnote(
        label = "Variable Footnote",
        starts_with("stat_") ~
        "median (IQR) for continuous variables; n (%) categorical variables"
      ) %>%
      render_as_html()
  )

  expect_snapshot(
    tbl_summary %>%
      modify_footnote(update = everything() ~ NA) %>%
      render_as_html()
  )

  expect_snapshot(
    tbl_summary %>%
      modify_footnote(update = NULL) %>%
      render_as_html()
  )

  expect_snapshot(
    glm(response ~ age + grade, trial, family = binomial) %>%
      tbl_regression(exponentiate = TRUE) %>%
      modify_footnote(ci ~ "CI = Credible Interval", abbreviation = TRUE) %>%
      render_as_html()
  )

  expect_true(
    tbl_summary %>%
      modify_footnote(everything() ~ NA) %>%
      purrr::pluck(
        "table_styling", "header",
        "footnote"
      ) %>%
      is.na() %>%
      all()
  )
})
