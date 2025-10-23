skip_on_cran()
skip_if_pkg_not_installed(c("survival", "broom.helpers", "survey"))

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

# assign_summary_type-arg:cat_threshold ----------------------------------------
test_that("assign_summary_type-arg:cat_threshold", {
  expect_equal(
    with_gtsummary_theme(
      x = list("assign_summary_type-arg:cat_threshold" = 0L),
      expr = tbl_summary(mtcars, include = cyl)$table_body$var_type[1]
    ),
    "continuous"
  )
})

# tbl_hierarchical-fn:addnl-fn-to-run ------------------------------------------
test_that("tbl_hierarchical_count-fn:addnl-fn-to-run", {
  expect_s3_class(
    with_gtsummary_theme(
      x = list("tbl_hierarchical-fn:addnl-fn-to-run" = as.data.frame),
      expr =
        tbl_hierarchical(
          data = cards::ADAE[1:20, ],
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
          data = cards::ADAE[1:20, ],
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


# pkgwide-str:print_engine -----------------------------------------------------
test_that("pkgwide-str:print_engine changes print methods as expected", {
  # When setting `pkgwide-str:print_engine` equal to `flextable`, the output
  # has expected class `flextable`
  capture.output(
    expect_equal(
      with_gtsummary_theme(
        x = list("pkgwide-str:print_engine" = "flextable"),
        expr =
          trial |>
            tbl_summary(by = trt, include = age, missing = "no") |>
            print() |>
            class()
      ),
      "flextable"
    )
  )

  # When setting `pkgwide-str:print_engine` equal to `huxtable`, the output
  # has expected class "huxtable",  "data.frame"
  capture.output(
    expect_equal(
      with_gtsummary_theme(
        x = list("pkgwide-str:print_engine" = "huxtable"),
        expr =
          trial |>
            tbl_summary(by = trt, include = age, missing = "no") |>
            print() |>
            class()
      ),
      c("huxtable", "data.frame")
    )
  )

  # When setting `pkgwide-str:print_engine` equal to `tibble`, the output
  # has expected class `tbl_df`, `tbl`, `data.frame`
  capture.output(
    expect_equal(
      with_gtsummary_theme(
        x = list("pkgwide-str:print_engine" = "tibble"),
        expr =
          trial |>
            tbl_summary(by = trt, include = age, missing = "no") |>
            print() |>
            class()
      ),
      c("tbl_df", "tbl", "data.frame")
    )
  )

  # When setting `pkgwide-str:print_engine` equal to `kable`, the output
  # has expected class `knitr_kable`
  capture.output(
    expect_equal(
      with_gtsummary_theme(
        x = list("pkgwide-str:print_engine" = "kable"),
        expr =
          trial |>
            tbl_summary(by = trt, include = age, missing = "no") |>
            print() |>
            class()
      ),
      "knitr_kable"
    )
  )

  # When setting `pkgwide-str:print_engine` equal to `kable_extra`, the output
  # has expected class `kableExtra`, `knitr_kable`
  capture.output(
    expect_equal(
      with_gtsummary_theme(
        x = list("pkgwide-str:print_engine" = "kable_extra"),
        expr =
          trial |>
            tbl_summary(by = trt, include = age, missing = "no") |>
            print() |>
            class()
      ),
      c("kableExtra", "knitr_kable")
    )
  )

  # When setting `pkgwide-str:print_engine` equal to `gt`, the output
  # has expected class `gt_tbl`, `list`
  capture.output(
    expect_equal(
      with_gtsummary_theme(
        x = list("pkgwide-str:print_engine" = "gt"),
        expr =
          trial |>
            tbl_summary(by = trt, include = age, missing = "no") |>
            print() |>
            class()
      ),
      c("gt_tbl", "list")
    )
  )
})
