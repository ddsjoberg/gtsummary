skip_on_cran()

tbl1 <-
  lm(time ~ sex + ph.ecog, survival::lung) %>%
  tbl_regression()

tbl2 <-
  lm(time ~ ph.ecog + sex, survival::lung) %>%
  tbl_regression(label = list(sex = "Sex", ph.ecog = "ECOG Score"))


test_that("works as expected without error", {
  expect_snapshot(
    tbl1 %>%
      add_significance_stars(hide_ci = FALSE, hide_p = FALSE) %>%
      as.data.frame()
  )

  expect_error(
    tbl_stars <-
      tbl1 %>%
      add_significance_stars(hide_ci = FALSE, hide_p = FALSE),
    NA
  )
  expect_snapshot(tbl_stars %>% as.data.frame())

  expect_error(
    tbl_merge(list(tbl_stars, tbl_stars)),
    NA
  )

  expect_equal(
    tbl_stack(list(tbl_stars, tbl_stars)) %>% as_tibble(col_labels = FALSE) %>% pull(estimate),
    c("52", "-58**", "52", "-58**")
  )

  expect_snapshot(
    tbl1 %>%
      add_significance_stars(
        thresholds = c(0.0000001, 0.55, 0.9, 1),
        hide_p = FALSE
      ) %>%
      as.data.frame()
  )

  expect_equal(
    tbl2 %>%
      add_significance_stars(
        pattern = "{estimate} ({conf.low}, {conf.high}){stars}",
        hide_ci = TRUE, hide_se = TRUE
      ) %>%
      as_tibble(col_labels = FALSE) %>% purrr::pluck("estimate", 1),
    "-58 (-96, -21)**"
  )
})

test_that("errors/messages with bad inputs", {
  expect_error(
    tbl1 %>% add_significance_stars(thresholds = c(0.0000001, 0.55, 0.9, 1.1))
  )

  expect_error(
    add_significance_stars(trial)
  )

  expect_error(
    add_significance_stars(trial, pattern = c("afds", "asf"))
  )

  expect_error(
    tbl1 %>% add_significance_stars(pattern = c("afds", "asf"))
  )

  expect_error(
    tbl1 %>% add_significance_stars(pattern = "no columns selected")
  )

  expect_message(
    tbl1 %>% add_significance_stars(pattern = "{estimate}")
  )
})
