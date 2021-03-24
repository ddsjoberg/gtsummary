skip_on_cran()

tbl1 <-
  lm(time ~ sex + ph.ecog, survival::lung) %>%
  tbl_regression()

test_that("works as expected without error", {
  expect_error(
    tbl1 %>%
      add_significance_stars(hide_ci = FALSE, hide_p = FALSE),
    NA
  )

  expect_error(
    tbl1 %>%
      add_significance_stars(hide_ci = FALSE, hide_p = FALSE),
    NA
  )

  expect_error(
    tbl1 %>%
      add_significance_stars(thresholds = c(0.0000001, 0.55, 0.9, 1),
                             hide_p = FALSE),
    NA
  )
})

test_that("errors with bad inputs", {
  expect_error(
    tbl1 %>% add_significance_stars(thresholds = c(0.0000001, 0.55, 0.9, 1.1))
  )
  expect_error(
    add_significance_stars(trial)
  )
})
