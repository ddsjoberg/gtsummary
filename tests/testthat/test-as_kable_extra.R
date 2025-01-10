skip_on_cran()
skip_if_not(is_pkg_installed(c("kableExtra", "knitr")))

my_tbl_summary <- trial |>
  select(trt, age, death) |>
  tbl_summary()
my_tbl_regression <- lm(marker ~ age, trial) |> tbl_regression()

test_that("as_kable_extra works with standard use", {
  # include argument does not produce warnings
  expect_silent(my_tbl_summary |> as_kable_extra(include = tibble))

  # no errors replacing default kable argument value
  expect_silent(my_tbl_summary |> as_kable_extra(col.names = NULL))

  expect_silent(kbl_summary <- my_tbl_summary |> as_kable_extra())

  # correct number of rows (+2 for header styling)
  kbl <- kbl_summary[1] |> strsplit("* </?tr>\n *")
  expect_equal(length(kbl[[1]]), 10)
})

test_that("as_kable_extra(return_calls) works as expected", {
  # no warnings produced
  expect_silent(kbl <- my_tbl_summary |> as_kable_extra(return_calls = TRUE))

  # correct elements are returned
  expect_equal(
    names(kbl),
    c("tibble", "fmt", "cols_merge", "fmt_missing", "cols_hide", "remove_line_breaks",
      "escape_table_body", "bold_italic", "kable", "add_indent",
      "add_header_above", "source_note", "abbreviations", "footnote")
  )
})

test_that("as_kable_extra(format) works as expected", {
  # html (default)
  expect_silent(kbl_html <- my_tbl_summary |> as_kable_extra())
  kbl_html <- kbl_html[1] |> strsplit("* </?tr>\n *")
  expect_snapshot(kbl_html)

  # latex
  expect_silent(kbl_latex <- my_tbl_summary |> as_kable_extra(format = "latex"))
  kbl_latex <- kbl_latex[1] |> strsplit("\n")
  expect_snapshot(kbl_latex)

  # simple (warnings generated since formatting is removed)
  expect_warning(expect_warning(
    kbl_simple <- my_tbl_summary |> as_kable_extra(format = "simple")
  ))
  expect_snapshot(kbl_simple)
})

test_that("as_kable_extra(escape) works as expected", {
  tbl_line_brks <- trial |>
    select(trt, age, death) |>
    tbl_summary(label = list(age = "Pt\n Age")) |>
    modify_header(label = "Test\n Columns")

  # escape = FALSE (default)
  kbl <- tbl_line_brks |> as_kable_extra()
  expect_true(grepl("Test<br>Columns", kbl[1]))
  expect_true(grepl("Pt Age", kbl[1]))

  # escape = TRUE
  kbl <- tbl_line_brks |> as_kable_extra(escape = TRUE)
  expect_true(grepl("TestColumns", kbl[1]))
  expect_true(grepl("Pt Age", kbl[1]))
})

test_that("as_kable_extra(addtl_fmt) works as expected", {
  tbl_line_brks <- trial |>
    select(trt, age, death) |>
    tbl_summary(label = list(age = "Pt\n Age")) |>
    modify_header(label = "Test\n Columns")

  # addtl_fmt = TRUE (default)
  kbl <- tbl_line_brks |> as_kable_extra()
  expect_true(grepl("Test<br>Columns", kbl[1]))
  expect_true(grepl("Pt Age", kbl[1]))

  # addtl_fmt = FALSE
  kbl <- tbl_line_brks |> as_kable_extra(addtl_fmt = FALSE)
  expect_true(grepl("TestColumns", kbl[1]))
  expect_true(grepl("Pt Age", kbl[1]))
})

test_that("as_kable_extra works with tbl_merge", {
  skip_if_not(is_pkg_installed("survival"))

  t1 <- glm(response ~ trt + grade + age, trial, family = binomial) |>
    tbl_regression(exponentiate = TRUE)
  t2 <- survival::coxph(survival::Surv(ttdeath, death) ~ trt + grade + age, trial) |>
    tbl_regression(exponentiate = TRUE)

  tbl_merge_ex1 <- tbl_merge(
    tbls = list(t1, t2),
    tab_spanner = c("**Tumor Response**", "**Time to Death**")
  )

  expect_silent(kbl_merge <- tbl_merge_ex1 |> as_kable_extra())
  kbl_merge <- strsplit(kbl_merge[1], "* </?tr>\n *")[[1]]
  expect_snapshot(kbl_merge)
})

test_that("as_kable_extra works with tbl_stack", {
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

  expect_silent(kbl_stack <- tbl_stack_ex1 |> as_kable_extra())
  kbl_stack <- strsplit(kbl_stack[1], "* </?tr>\n *")[[1]]
  expect_snapshot(kbl_stack)
})

test_that("as_kable_extra checking the placement of a second spanning header", {
  expect_silent(
    tbl2 <-
      trial |>
      tbl_summary(by = grade, include = age) |>
      modify_spanning_header(c(stat_1, stat_3) ~ "**Testing**") |>
      modify_spanning_header(all_stat_cols() ~ "**Tumor Grade**", level = 2) |>
      as_kable_extra()
  )

  # this isn't a great test, but it's something!
  expect_true(as.character(tbl2) |> str_detect("Testing"))
  expect_true(as.character(tbl2) |> str_detect("Tumor Grade"))
})


test_that("as_kable_extra works with bold/italics", {
  tbl <- my_tbl_summary |>
    bold_labels() |>
    italicize_levels()
  kbl <- tbl |> as_kable_extra()

  kbl1 <- strsplit(kbl[1], "* </?tr>\n *")[[1]][-c(1, 3)]

  # labels correctly bolded
  expect_equal(
    sapply(kbl1, grepl, pattern = "bold") |> unname(),
    c(FALSE, TRUE, FALSE, FALSE, TRUE, FALSE, TRUE, FALSE)
  )

  # labels correctly italicized
  expect_equal(
    sapply(kbl1, grepl, pattern = "italic") |> unname(),
    c(FALSE, FALSE, TRUE, TRUE, FALSE, TRUE, FALSE, FALSE)
  )
})

test_that("as_kable_extra passes table column alignment correctly", {
  kbl <- my_tbl_regression |> as_kable_extra()
  kbl <- strsplit(kbl[1], "* </?tr>\n *")[[1]][-c(1, 3)]

  expect_equal(
    sapply(sapply(kbl[2], strsplit, split = "text-align:")[[1]][-1], word, start = 1, sep = ";") |> unname(),
    c("left", "center", "center", "center")
  )

  # customize alignment
  tbl <- my_tbl_regression |>
    modify_table_styling(columns = "estimate", align = "right")
  kbl <- tbl |> as_kable_extra()
  kbl <- strsplit(kbl[1], "* </?tr>\n *")[[1]][-c(1, 3)]

  expect_equal(
    sapply(sapply(kbl[2], strsplit, split = "text-align:")[[1]][-1], word, start = 1, sep = ";") |> unname(),
    c("left", "right", "center", "center")
  )
})

test_that("as_kable_extra passes table footnotes & abbreviations correctly", {
  tbl_fn <- my_tbl_summary |>
    modify_footnote_body(columns = label, footnote = "test footnote", rows = variable == "age")
  kbl_fn <- tbl_fn |> as_kable_extra()

  # footnote
  expect_snapshot_value(
    strsplit(kbl_fn[1], "* </?tr>\n *")[[1]][10]
  )

  tbl_fa <- tbl_fn |>
    modify_abbreviation("N = number of observations")
  kbl_fa <- tbl_fa |> as_kable_extra()

  # footnote_abbrev
  expect_snapshot_value(
    strsplit(kbl_fa[1], "* </?tr>\n *")[[1]][10]
  )

  # customized footnotes
  tbl <- my_tbl_summary |>
    modify_footnote_header("replace old footnote", columns = all_stat_cols()) |>
    modify_footnote_header("another new footnote", columns = label)
  kbl <- tbl |> as_kable_extra()

  expect_snapshot_value(
    strsplit(kbl[1], "* </?tr>\n *")[[1]][10]
  )

  expect_true(
    my_tbl_summary |>
      modify_footnote_spanning_header("footnote test", columns = all_stat_cols()) |>
      as_kable_extra(format = "html") |>
      str_detect("footnote test")
  )
})

test_that("as_kable_extra passes appended glance statistics correctly", {
  tbl <- my_tbl_regression |>
    add_glance_source_note(c("r.squared", "BIC")) |>
    add_glance_table(c("r.squared", "BIC"))
  kbl <- tbl |> as_kable_extra()

  expect_snapshot_value(
    strsplit(kbl[1], "* </?tr>\n *")[[1]][5]
  )
  expect_snapshot_value(
    strsplit(kbl[1], "* </?tr>\n *")[[1]][6]
  )
})

test_that("as_kable_extra passes captions correctly", {
  tbl <- my_tbl_regression |>
    modify_caption("My table caption")
  kbl <- tbl |> as_kable_extra()

  expect_snapshot_value(
    strsplit(kbl[1], "* </?tr>\n *")[[1]][1]
  )
})

test_that("as_kable_extra passes table indentation correctly", {
  kbl <- my_tbl_summary |> as_kable_extra()

  expect_true(
    sapply(
      strsplit(kbl[1], "* </?tr>\n *")[[1]][c(5, 6, 8)],
      grepl,
      pattern = 'indentlevel=\"1\"'
    ) |>
      all()
  )

  # indentation removed
  tbl <- my_tbl_summary |>
    modify_column_indent(columns = label, indent = 0)
  kbl <- tbl |> as_kable_extra()

  expect_false(
    sapply(
      strsplit(kbl[1], "* </?tr>\n *")[[1]][c(5, 6, 8)],
      grepl,
      pattern = 'indentlevel=\"1\"'
    ) |>
      any()
  )
})

test_that("as_kable_extra passes missing symbols correctly", {
  tbl <- my_tbl_summary |>
    modify_table_body(~ .x |> mutate(stat_0 = NA_character_))
  kbl <- tbl |> as_kable_extra()

  expect_true(
    sapply(
      strsplit(kbl[1], "* </?tr>\n *")[[1]][c(5, 6, 8)],
      grepl,
      pattern = "   "
    ) |>
      all()
  )

  # specify missing symbol
  tbl <- tbl |>
    modify_missing_symbol(stat_0, rows = !is.na(label), symbol = "n / a")
  kbl <- tbl |> as_kable_extra()

  # correct substitution for missing values
  expect_true(
    sapply(
      strsplit(kbl[1], "* </?tr>\n *")[[1]][c(5, 6, 8)],
      grepl,
      pattern = "n / a"
    ) |>
      all()
  )
})
