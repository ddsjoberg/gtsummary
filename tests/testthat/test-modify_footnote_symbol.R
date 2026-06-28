skip_on_cran()
skip_if_pkg_not_installed("broom.helpers")

test_that("modify_footnote_symbol() stores the symbol sequence", {
  tbl <-
    trial |>
    tbl_summary(by = trt, include = c(age, grade), missing = "no") |>
    add_p() |>
    modify_footnote_symbol(symbol = c("*", "\u2020", "\u2021"))

  expect_equal(
    tbl$table_styling$footnote_symbol,
    c("*", "\u2020", "\u2021")
  )

  # call is recorded in the call list
  expect_true("modify_footnote_symbol" %in% names(tbl$call_list))
})

test_that("modify_footnote_symbol() input checks", {
  tbl <- tbl_summary(trial, include = marker)

  # missing symbol
  expect_error(modify_footnote_symbol(tbl))
  # non-character symbol
  expect_error(
    modify_footnote_symbol(tbl, symbol = 1:3),
    class = "check_class"
  )
  # empty symbol
  expect_error(
    modify_footnote_symbol(tbl, symbol = character(0)),
    "non-empty"
  )
  # non-gtsummary x
  expect_error(
    modify_footnote_symbol(letters, symbol = "*"),
    class = "check_class"
  )
})

test_that("modify_footnote_symbol() works with as_gt()", {
  skip_if_pkg_not_installed("gt")

  tbl <-
    trial |>
    tbl_summary(by = trt, include = c(age, grade), missing = "no") |>
    add_p()

  # custom symbols emit an opt_footnote_marks() call
  gt_calls <-
    tbl |>
    modify_footnote_symbol(symbol = c("*", "\u2020", "\u2021")) |>
    as_gt(return_calls = TRUE)
  expect_true("opt_footnote_marks" %in% names(gt_calls))
  expect_equal(
    gt_calls[["opt_footnote_marks"]],
    expr(gt::opt_footnote_marks(marks = !!c("*", "\u2020", "\u2021")))
  )

  # default (no custom symbols) does not emit the call
  gt_calls_default <- tbl |> as_gt(return_calls = TRUE)
  expect_false("opt_footnote_marks" %in% names(gt_calls_default))

  # tables render without error
  expect_silent(
    tbl |> modify_footnote_symbol(symbol = c("*", "\u2020")) |> as_gt() |> gt::as_raw_html()
  )
})

test_that("modify_footnote_symbol() works with as_flex_table()", {
  skip_if_pkg_not_installed("flextable")

  tbl <-
    trial |>
    tbl_summary(by = trt, include = c(age, grade), missing = "no") |>
    add_p() |>
    modify_footnote_body(
      footnote = "grade note",
      columns = "label",
      rows = variable == "grade" & row_type == "label"
    )

  # default: integer reference marks (no regression)
  ft_calls_default <- tbl |> as_flex_table(return_calls = TRUE)
  expect_equal(ft_calls_default$footnote_header[[1]]$ref_symbols, 1L)
  expect_equal(ft_calls_default$footnote_header[[2]]$ref_symbols, 2L)
  expect_equal(ft_calls_default$footnote_body[[1]]$ref_symbols, 3L)

  # custom symbols replace the integer marks (in appearance order)
  ft_calls <-
    tbl |>
    modify_footnote_symbol(symbol = c("*", "\u2020", "\u2021")) |>
    as_flex_table(return_calls = TRUE)
  expect_equal(ft_calls$footnote_header[[1]]$ref_symbols, "*")
  expect_equal(ft_calls$footnote_header[[2]]$ref_symbols, "\u2020")
  expect_equal(ft_calls$footnote_body[[1]]$ref_symbols, "\u2021")

  # symbols recycle when there are more footnotes than symbols
  ft_calls_recycle <-
    tbl |>
    modify_footnote_symbol(symbol = "@") |>
    as_flex_table(return_calls = TRUE)
  expect_equal(ft_calls_recycle$footnote_header[[1]]$ref_symbols, "@")
  expect_equal(ft_calls_recycle$footnote_header[[2]]$ref_symbols, "@")
  expect_equal(ft_calls_recycle$footnote_body[[1]]$ref_symbols, "@")

  # tables render without error
  expect_silent(
    tbl |> modify_footnote_symbol(symbol = c("*", "\u2020", "\u2021")) |> as_flex_table()
  )
})

test_that("pkgwide-chr:footnote_symbol theme element is respected", {
  skip_if_pkg_not_installed("flextable")

  withr::defer(reset_gtsummary_theme())
  set_gtsummary_theme(list("pkgwide-chr:footnote_symbol" = c("a", "b", "c")))

  tbl <-
    trial |>
    tbl_summary(by = trt, include = age, missing = "no") |>
    add_p()

  # theme element supplies the default symbols
  ft_calls <- tbl |> as_flex_table(return_calls = TRUE)
  expect_equal(ft_calls$footnote_header[[1]]$ref_symbols, "a")
  expect_equal(ft_calls$footnote_header[[2]]$ref_symbols, "b")

  # an explicit modify_footnote_symbol() overrides the theme element
  ft_calls_override <-
    tbl |>
    modify_footnote_symbol(symbol = c("*", "\u2020")) |>
    as_flex_table(return_calls = TRUE)
  expect_equal(ft_calls_override$footnote_header[[1]]$ref_symbols, "*")
  expect_equal(ft_calls_override$footnote_header[[2]]$ref_symbols, "\u2020")
})
