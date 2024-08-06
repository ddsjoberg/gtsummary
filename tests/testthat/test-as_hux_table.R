skip_on_cran()
skip_if_not(is_pkg_installed("huxtable", reference_pkg = "gtsummary"))

my_tbl_summary <- trial |>
  select(trt, age, death) |>
  tbl_summary()
my_tbl_regression <- lm(marker ~ age, trial) |> tbl_regression()

# as_hux_table ----

test_that("as_hux_table works with standard use", {
  # include argument does not produce warnings
  expect_silent(my_tbl_summary |> as_hux_table(include = tibble))

  expect_silent(ht_summary <- my_tbl_summary |> as_hux_table())

  # correct number of rows
  expect_equal(nrow(ht_summary), 8)
})

test_that("as_hux_table(return_calls) works as expected", {
  # no warnings produced
  expect_silent(ht <- my_tbl_summary |> as_hux_table(return_calls = TRUE))

  # correct elements are returned
  expect_equal(
    names(ht),
    c("tibble", "fmt", "cols_merge", "cols_hide", "huxtable", "set_left_padding", "add_footnote",
      "set_bold", "set_italic", "fmt_missing", "insert_row", "set_markdown", "align", "set_number_format")
  )
})

test_that("as_hux_table works with tbl_merge", {
  skip_if_not(is_pkg_installed("survival", reference_pkg = "gtsummary"))

  t1 <- glm(response ~ trt + grade + age, trial, family = binomial) |>
    tbl_regression(exponentiate = TRUE)
  t2 <- survival::coxph(survival::Surv(ttdeath, death) ~ trt + grade + age, trial) |>
    tbl_regression(exponentiate = TRUE)

  tbl_merge_ex1 <- tbl_merge(
    tbls = list(t1, t2),
    tab_spanner = c("**Tumor Response**", "**Time to Death**")
  )

  expect_silent(ht_merge <- tbl_merge_ex1 |> as_hux_table())
  expect_true(attr(ht_merge, "header_rows")[1:2] |> all())
  expect_snapshot(ht_merge)
})

test_that("as_hux_table works with tbl_stack", {
  t1 <- trial |>
    dplyr::filter(trt == "Drug A") |>
    select(age, response, death) |>
    tbl_summary() |>
    modify_header(stat_0 ~ "**Statistic**")

  t2 <- trial |>
    dplyr::filter(trt == "Drug B") |>
    select(age, response, death) |>
    tbl_summary()

  tbl_stack_ex1 <- tbl_stack(
    tbls = list(t1, t2),
    group_header = c("Drug A", "Drug B"),
    quiet = TRUE
  )

  expect_silent(ht_stack <- tbl_stack_ex1 |> as_hux_table())
  expect_snapshot(ht_stack)
})

test_that("as_hux_table works with bold/italics", {
  tbl <- my_tbl_summary |>
    bold_labels() |>
    italicize_levels()
  ht <- tbl |> as_hux_table()

  # labels correctly bolded
  expect_equal(
    attr(ht, "bold")[, 1] |> unname(),
    c(FALSE, TRUE, FALSE, FALSE, TRUE, FALSE, TRUE, FALSE)
  )

  # labels correctly italicized
  expect_equal(
    attr(ht, "italic")[, 1] |> unname(),
    c(FALSE, FALSE, TRUE, TRUE, FALSE, TRUE, FALSE, FALSE)
  )
})

test_that("as_hux_table passes table column alignment correctly", {
  ht <- my_tbl_regression |> as_hux_table()

  expect_equal(
    attr(ht, "align")[1, ] |> unname(),
    c("left", "center", "center", "center")
  )

  # customize alignment
  tbl <- my_tbl_regression |>
    modify_table_styling(columns = "estimate", align = "right")
  ht <- tbl |> as_hux_table()

  expect_equal(
    attr(ht, "align")[1, ] |> unname(),
    c("left", "right", "center", "center")
  )
})

test_that("as_hux_table passes table footnotes & footnote abbreviations correctly", {
  tbl_fn <- my_tbl_summary |>
    modify_table_styling(columns = label, footnote = "test footnote", rows = variable == "age")
  ht_fn <- tbl_fn |> as_hux_table()

  # footnote
  expect_equal(
    ht_fn[8:9, 1] |> unlist(use.names = FALSE),
    c("n (%); Median (Q1, Q3)", "test footnote")
  )
  expect_equal(
    ht_fn[8:9, 1] |> unlist(use.names = FALSE),
    ht_fn[8:9, 2] |> unlist(use.names = FALSE)
  )

  tbl_fa <- tbl_fn |>
    modify_footnote(stat_0 = "N = number of observations", abbreviation = TRUE)
  ht_fa <- tbl_fa |> as_hux_table()

  # footnote_abbrev
  expect_equal(
    ht_fa[9, 1] |> unlist(use.names = FALSE),
    "N = number of observations"
  )

  # customized footnotes
  tbl <- my_tbl_summary |>
    modify_footnote(
      all_stat_cols() ~ "replace old footnote",
      label = "another new footnote"
    )
  ht <- tbl |> as_hux_table()

  expect_equal(
    ht[8:9, 1] |> unlist(use.names = FALSE),
    c("another new footnote", "replace old footnote")
  )
})

test_that("as_hux_table passes appended glance statistics correctly", {
  tbl <- my_tbl_regression |>
    add_glance_source_note(c("r.squared", "BIC")) |>
    add_glance_table(c("r.squared", "BIC"))
  ht <- tbl |> as_hux_table()

  # horizontal lines added to table
  expect_equal(
    attr(ht, "tb_borders")$thickness[5:6, ],
    matrix(0.8, nrow = 2, ncol = 4)
  )

  # correct row ordering
  expect_equal(
    ht$label,
    c("**Characteristic**", "Age", "R²", "BIC", "CI = Confidence Interval", "R² = 0.000; BIC = 471")
  )
})

test_that("as_hux_table passes captions correctly", {
  tbl <- my_tbl_regression |>
    modify_caption("My table caption")
  ht <- tbl |> as_hux_table()

  expect_equal(
    attr(ht, "caption"),
    "My table caption"
  )
})

test_that("as_hux_table passes table indentation correctly", {
  ht <- my_tbl_summary |> as_hux_table()

  expect_equal(
    attr(ht, "left_padding")[, 1] |> unname(),
    c(6, 6, 15, 15, 6, 15, 6, 6)
  )

  # indentation removed
  tbl <- my_tbl_summary |>
    modify_column_indent(columns = label, indent = 0)
  ht <- tbl |> as_hux_table()

  expect_equal(
    attr(ht, "left_padding")[, 1] |> unname(),
    rep(6, 8)
  )
})

test_that("as_hux_table passes missing symbols correctly", {
  tbl <- my_tbl_summary |>
    modify_table_body(~ .x |> mutate(stat_0 = NA_character_))
  ht <- tbl |> as_hux_table()

  expect_true(
    all(attr(ht, "na_string")[2:7, 2] == "")
  )

  # specify missing symbol
  tbl <- tbl |>
    modify_table_styling(stat_0, rows = !is.na(label), missing_symbol = "n / a")
  ht <- tbl |> as_hux_table()

  # correct substitution for missing values
  expect_true(
    all(attr(ht, "na_string")[2:7, 2] == "n / a")
  )
})

test_that("as_hux_table(strip_md_bold) causes defunct error", {
  lifecycle::expect_defunct(
    my_tbl_summary |> as_hux_table(strip_md_bold = TRUE)
  )
})

# as_hux_xlsx ----

test_that("as_hux_xlsx works with standard use", {
  skip_if_not(is_pkg_installed("openxlsx", reference_pkg = "gtsummary"))

  tf <- tempfile(fileext = ".xlsx")
  expect_silent(as_hux_xlsx(my_tbl_summary, file = tf))
  expect_true(file.exists(tf))
})

test_that("as_hux_xlsx(bold_header_rows) works", {
  skip_if_not(is_pkg_installed("openxlsx", reference_pkg = "gtsummary"))

  tf <- tempfile(fileext = ".xlsx")
  expect_silent(as_hux_xlsx(my_tbl_summary, file = tf, bold_header_rows = FALSE))
  expect_true(file.exists(tf))
})
