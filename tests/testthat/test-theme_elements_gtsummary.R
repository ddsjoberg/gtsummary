skip_on_cran()
skip_if_not(is_pkg_installed(c("survival", "broom.helpers", "cardx", "survey")))

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

# tbl_hierarchical-fn:addnl-fn-to-run ------------------------------------
test_that("tbl_hierarchical_count-fn:addnl-fn-to-run", {
  expect_s3_class(
    with_gtsummary_theme(
      x = list("tbl_hierarchical-fn:addnl-fn-to-run" = as.data.frame),
      expr =
        tbl_hierarchical(
          data = cards::ADAE[1:20,],
          variables = c(AESOC, AETERM),
          by = TRTA,
          denominator = cards::ADSL,
          id = USUBJID
        )
    ),
    "data.frame"
  )
})

# tbl_hierarchical_count-fn:addnl-fn-to-run ------------------------------------
test_that("tbl_hierarchical_count-fn:addnl-fn-to-run", {
  expect_s3_class(
    with_gtsummary_theme(
      x = list("tbl_hierarchical_count-fn:addnl-fn-to-run" = as.data.frame),
      expr =
        tbl_hierarchical_count(
          data = cards::ADAE[1:20,],
          variables = c(AESOC, AETERM, AESEV),
          by = TRTA
        )
    ),
    "data.frame"
  )
})

# tbl_summary-arg:missing_stat -------------------------------------------------
test_that("tbl_summary-arg:missing_stat works", {
  # works with add_difference()
  expect_equal(
    with_gtsummary_theme(
      x = list("tbl_summary-arg:missing_stat" = "{N_miss} / {N_obs}"),
      expr =
        trial |>
        tbl_summary(by = trt, include = age, missing = "always") |>
        as_tibble(col_labels = FALSE) |>
        dplyr::pull(stat_1) |>
        dplyr::last()
    ),
    "7 / 98"
  )
})

# tbl_svysummary-arg:missing_stat ----------------------------------------------
test_that("tbl_summary-arg:missing_stat works", {
  expect_equal(
    with_gtsummary_theme(
      x = list("tbl_svysummary-arg:missing_stat" = "{N_miss} / {N_obs}"),
      expr =
        survey::svydesign(~1, data = trial, weights = ~1) |>
        tbl_svysummary(by = trt, include = age, missing = "always") |>
        as_tibble(col_labels = FALSE) |>
        dplyr::pull(stat_1) |>
        dplyr::last()
    ),
    "7 / 98"
  )
})

# add_overall.tbl_summary-arg:col_label ----------------------------------------
test_that("tbl_summary-arg:missing_stat works", {
  expect_equal(
    with_gtsummary_theme(
      x = list("add_overall.tbl_summary-arg:col_label" = "All Participants  \nN = {style_number(N)}"),
      expr =
        tbl_summary(trial, include = age, by = trt) |>
        add_overall(last = TRUE) |>
        as.data.frame() |>
        names() |>
        dplyr::last()
    ),
    "All Participants  \nN = 200"
  )

  expect_equal(
    with_gtsummary_theme(
      x = list("add_overall.tbl_summary-arg:col_label" = "All Participants  \nN = {style_number(N)}"),
      expr =
        survey::svydesign(~1, data = trial, weights = ~1) |>
        tbl_svysummary(by = trt, include = age) |>
        add_overall(last = TRUE) |>
        as.data.frame() |>
        names() |>
        dplyr::last()
    ),
    "All Participants  \nN = 200"
  )
})
