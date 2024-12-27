skip_on_cran()
skip_if_not(is_pkg_installed("survival"))

# pkgwide-fn:prependpvalue_fun -------------------------------------------------
test_that("pkgwide-fn:prependpvalue_fun", {
  # works in `inline_text.tbl_summary()`
  expect_equal(
    with_gtsummary_theme(
      x = list("pkgwide-fn:prependpvalue_fun" = label_style_pvalue(digits = 3, prepend_p = TRUE)),
      expr =
        trial |>
        tbl_summary(by = trt, include = age) |>
        add_p() |>
        inline_text(variable = age, column = "p.value")
    ),
    "p=0.718"
  )

  # works in `inline_text.tbl_cross()`
  expect_equal(
    with_gtsummary_theme(
      x = list("pkgwide-fn:prependpvalue_fun" = label_style_pvalue(digits = 3, prepend_p = TRUE)),
      expr =
        trial |>
        tbl_cross(col = trt, row = grade) |>
        add_p() |>
        inline_text(col_level = "p.value")
    ),
    "p=0.871"
  )

  # works in `inline_text.tbl_regression()`
  expect_equal(
    with_gtsummary_theme(
      x = list("pkgwide-fn:prependpvalue_fun" = label_style_pvalue(digits = 3, prepend_p = TRUE)),
      expr =
        lm(marker ~ age, trial) |>
        tbl_regression() |>
        inline_text(variable = age, pattern = "{p.value}")
    ),
    "p=0.965"
  )

  # works in `inline_text.tbl_survfit()`
  expect_equal(
    with_gtsummary_theme(
      x = list("pkgwide-fn:prependpvalue_fun" = label_style_pvalue(digits = 3, prepend_p = TRUE)),
      tbl_survfit(
        survival::survfit(survival::Surv(ttdeath, death) ~ trt, trial),
        times = c(12, 24),
        label = ~"Treatment",
        label_header = "**{time} Month**"
      ) |>
        add_p() |>
        inline_text(column = p.value)
    ),
    "p=0.239"
  )
})

# pkgwide-str:ci.sep -----------------------------------------------------------
test_that("pkgwide-str:ci.sep works", {
  # works with add_difference()
  expect_equal(
    with_gtsummary_theme(
      x = list("pkgwide-str:ci.sep" = " --- "),
      expr =
        trial |>
        tbl_summary(by = trt, include = age, missing = "no") |>
        add_difference() |>
        as_tibble(col_labels = FALSE) |>
        dplyr::pull(conf.low)
    ),
    "-4.6 --- 3.7"
  )

  # works with `tbl_survfit()`
  expect_equal(
    with_gtsummary_theme(
      x = list("pkgwide-str:ci.sep" = " --- "),
      tbl_survfit(
        survival::survfit(survival::Surv(ttdeath, death) ~ trt, trial),
        times = 12,
        label = ~"Treatment",
        label_header = "**{time} Month**"
      ) |>
        as_tibble(col_labels = FALSE) |>
        dplyr::pull(stat_1) |>
        dplyr::last()
    ),
    "86% (80% --- 93%)"
  )
})

