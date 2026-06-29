skip_on_cran()
skip_if_pkg_not_installed("gt")

# helper: render a gtsummary table to gt HTML
as_gt_html <- function(x) {
  gt::as_raw_html(as_gt(x))
}

test_that("text_interpret = 'none' is accepted by modify functions", {
  tbl <- tbl_summary(trial, include = marker)

  expect_silent(modify_header(tbl, label = "**lab**", text_interpret = "none"))
  expect_silent(modify_caption(tbl, "*cap*", text_interpret = "none"))
  expect_silent(modify_source_note(tbl, "*sn*", text_interpret = "none"))
  expect_silent(modify_abbreviation(tbl, "Q1 = first quartile", text_interpret = "none"))
  expect_silent(
    modify_footnote_header(tbl, footnote = "*fn*", columns = label, text_interpret = "none")
  )
  expect_silent(
    modify_spanning_header(tbl, all_stat_cols() ~ "*sp*", text_interpret = "none")
  )
})

test_that("text_interpret rejects values other than md/html/none", {
  tbl <- tbl_summary(trial, include = marker)
  expect_error(
    modify_footnote_header(tbl, footnote = "x", columns = label, text_interpret = "latex")
  )
})

test_that("'none' stores the identity interpret function", {
  tbl <- tbl_summary(trial, include = marker) |>
    modify_footnote_header(footnote = "*p<0.05", columns = label, text_interpret = "none")

  expect_equal(
    tbl$table_styling$footnote_header |>
      dplyr::filter(footnote == "*p<0.05") |>
      dplyr::pull("text_interpret"),
    "identity"
  )

  # md still stores gt::md
  tbl_md <- tbl_summary(trial, include = marker) |>
    modify_footnote_header(footnote = "*p<0.05", columns = label, text_interpret = "md")
  expect_equal(
    tbl_md$table_styling$footnote_header |>
      dplyr::filter(footnote == "*p<0.05") |>
      dplyr::pull("text_interpret"),
    "gt::md"
  )
})

test_that("'none' footnote renders verbatim in as_gt()", {
  tbl <- tbl_summary(trial, include = marker) |>
    modify_footnote_header(
      footnote = "*p<0.05; **p<0.01",
      columns = label,
      text_interpret = "none"
    )
  html <- as_gt_html(tbl)

  # asterisks are preserved (not stripped into emphasis markup)
  expect_match(html, "\\*p", fixed = FALSE)
  # md would have produced an <em> wrapping; none should not
  expect_false(grepl("<em>p&lt;0.05", html))
})

test_that("'none' caption renders verbatim in as_gt()", {
  tbl <- tbl_summary(trial, include = marker) |>
    modify_caption("*literal caption*", text_interpret = "none")

  expect_equal(attr(tbl$table_styling$caption, "text_interpret"), "none")
  html <- as_gt_html(tbl)
  expect_match(html, "\\*literal caption\\*", fixed = FALSE)
})

test_that("md interpretation is unchanged for footnotes", {
  tbl <- tbl_summary(trial, include = marker) |>
    modify_footnote_header(
      footnote = "**bold note**",
      columns = label,
      text_interpret = "md"
    )
  html <- as_gt_html(tbl)
  # md renders bold via <strong>; the literal asterisks should be gone
  expect_false(grepl("\\*\\*bold note\\*\\*", html))
})

test_that("add_significance_stars() footnote uses 'none' and renders verbatim", {
  skip_if_pkg_not_installed("broom.helpers")

  tbl <- lm(age ~ marker + grade, trial) |>
    tbl_regression() |>
    add_significance_stars()

  fh <- tbl$table_styling$footnote_header
  star_row <- fh[!is.na(fh$footnote) & grepl("p<", fh$footnote), ]

  expect_equal(unique(star_row$text_interpret), "identity")

  html <- as_gt_html(tbl)
  # the stars footnote should appear with literal asterisks
  expect_match(html, "\\*p&lt;0.05|\\*p<0.05", fixed = FALSE)
})
