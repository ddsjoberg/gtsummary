skip_on_cran()
skip_if_not(is_pkg_installed(c("broom.helpers")))

tbl1 <-
  lm(time ~ sex + ph.ecog, survival::lung) |>
  tbl_regression()

tbl2 <-
  lm(time ~ ph.ecog + sex, survival::lung) |>
  tbl_regression(label = list(sex = "Sex", ph.ecog = "ECOG Score"))


test_that("add_significance_stars(x)", {
  expect_error(
    tbl_stars <-
      tbl1 |>
      add_significance_stars(hide_ci = FALSE, hide_p = FALSE),
    NA
  )
  expect_snapshot(tbl_stars |> as.data.frame())


  expect_snapshot(
    tbl_merge(list(tbl_stars, tbl_stars)) |> as.data.frame()
  )

  expect_equal(
    tbl_stack(list(tbl_stars, tbl_stars)) |>
      dplyr::as_tibble(col_labels = FALSE) |>
      dplyr::pull(estimate),
    c("52", "-58**", "52", "-58**")
  )

  expect_error(
    lm(age ~ grade, trial) |>
      tbl_regression(tidy_fun = \(x, ...) broom::tidy(x, ...) |> dplyr::select(-p.value)) |>
      add_significance_stars(),
    "There is no p-value column in the table and significance stars cannot be placed."
  )
})

test_that("add_significance_stars(thresholds)", {
  expect_snapshot(
    tbl1 |>
      add_significance_stars(
        thresholds = c(0.0000001, 0.55, 0.9, 1),
        hide_p = FALSE
      ) |>
      as.data.frame()
  )
})

test_that("add_significance_stars(pattern)", {
  expect_equal(
    tbl2 |>
      add_significance_stars(
        pattern = "{estimate} ({conf.low}, {conf.high}){stars}",
        hide_ci = TRUE, hide_se = TRUE
      ) |>
      as_tibble(col_labels = FALSE) |>
      getElement("estimate") |>
      getElement(1L),
    "-58 (-96, -21)**"
  )
})

test_that("add_significance_stars(pattern) messaging", {
  expect_error(
    tbl2 |>
      add_significance_stars(
        pattern = "{estimate} ({conf.low}, {conf.high}){stars} {not_a_column}"
      ),
    "not present"
  )

  expect_error(
    tbl2 |>
      add_significance_stars(
        pattern = "nothing selecting"
      ),
    "must be a string using glue syntax"
  )

  expect_message(
    tbl2 |>
      add_significance_stars(
        pattern = "{estimate} ({conf.low}, {conf.high})"
      ),
    "no stars will be added"
  )
})
