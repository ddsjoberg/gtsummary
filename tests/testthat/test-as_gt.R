skip_on_cran()

my_tbl_summary <- trial |>
  select(trt, age, death) |>
  tbl_summary()
my_tbl_regression <- lm(marker ~ age, trial) |> tbl_regression()
my_spanning_tbl <- trial |>
  tbl_summary(by = grade, include = age) |>
  modify_spanning_header(c(stat_1, stat_3) ~ "**Testing**")

gt_tbl_summary <- my_tbl_summary |> as_gt()
gt_tbl_regression <- my_tbl_regression |> as_gt()
gt_spanning_tbl <- my_spanning_tbl |> as_gt()

test_that("as_gt works with standard use", {
  # return_calls argument does not produce warnings
  expect_silent(my_tbl_summary |> as_gt(return_calls = TRUE))

  # include argument does not produce warnings
  expect_silent(my_tbl_summary |> as_gt(include = gt))

  # correct elements are returned
  expect_equal(
    names(gt_tbl_summary),
    c("_data", "_boxhead", "_stub_df", "_row_groups", "_heading", "_spanners", "_stubhead", "_footnotes",
      "_source_notes", "_formats", "_substitutions", "_styles", "_summary", "_options", "_transforms",
      "_locale", "_has_built")
  )
})

test_that("as_gt passes table body correctly", {
  # tbl_summary
  expect_equal(
    my_tbl_summary$table_body,
    gt_tbl_summary$`_data`
  )

  # tbl_cross
  my_tbl_cross <- tbl_cross(trial, trt, grade)
  expect_silent(gt_tbl_cross <- my_tbl_cross |> as_gt())
  expect_equal(
    my_tbl_cross$table_body |> as_tibble(),
    gt_tbl_cross$`_data`
  )

  # tbl_regression
  expect_equal(
    my_tbl_regression$table_body,
    gt_tbl_regression$`_data`,
    ignore_attr = "class"
  )

  # tbl_uvregression
  my_tbl_uvregression <- trial |> tbl_uvregression(method = lm, y = age)
  expect_silent(gt_tbl_uvregression <- my_tbl_uvregression |> as_gt())

  expect_equal(
    my_tbl_uvregression$table_body,
    gt_tbl_uvregression$`_data`,
    ignore_attr = "class"
  )

  # spanning_tbl
  expect_equal(
    my_spanning_tbl$table_body,
    gt_spanning_tbl$`_data`
  )
})

test_that("as_gt works with bold/italics", {
  tbl <- my_tbl_summary |>
    bold_labels() |>
    italicize_levels()
  gt_tbl <- tbl |> as_gt()

  gt_styles <- gt_tbl$`_styles` |>
    dplyr::arrange(rownum) |>
    dplyr::pull(styles) |>
    unlist(use.names = FALSE)

  # labels correctly bolded
  expect_equal(
    eval_tidy(tbl$table_styling$text_format$rows[[1]], data = tbl$table_body),
    gt_styles == "bold"
  )

  # labels correctly italicized
  expect_equal(
    eval_tidy(tbl$table_styling$text_format$rows[[2]], data = tbl$table_body),
    gt_styles == "italic"
  )
})

test_that("as_gt passes table header labels correctly", {
  # tbl_summary
  expect_equal(
    my_tbl_summary$table_styling$header$column,
    gt_tbl_summary$`_boxhead`$var
  )
  expect_equal(
    my_tbl_summary$table_styling$header$label,
    gt_tbl_summary$`_boxhead`$column_label |> unlist()
  )

  # spanning_tbl
  expect_equal(
    my_spanning_tbl$table_styling$header$column,
    gt_spanning_tbl$`_boxhead`$var
  )
  expect_equal(
    my_spanning_tbl$table_styling$header$label,
    gt_spanning_tbl$`_boxhead`$column_label |> unlist()
  )

  # spanning_tbl - spanning header
  expect_equal(
    my_spanning_tbl$table_styling$spanning_header |>
      dplyr::filter(!is.na(spanning_header)) |>
      dplyr::pull(column),
    gt_spanning_tbl$`_spanners`$vars[[1]]
  )
})

test_that("as_gt passes table column visibility correctly", {
  expect_equal(
    my_tbl_regression$table_styling$header |>
      dplyr::filter(!hide) |>
      dplyr::pull(column),
    gt_tbl_regression$`_boxhead` |>
      dplyr::filter(type != "hidden") |>
      dplyr::pull(var)
  )

  # customize visibility
  tbl <- my_tbl_regression |>
    modify_table_styling(columns = "estimate", hide = TRUE) |>
    modify_table_styling(columns = "N_obs", hide = FALSE)
  gt_tbl <- tbl |> as_gt()

  expect_equal(
    tbl$table_styling$header |>
      dplyr::filter(!hide) |>
      dplyr::pull(column),
    gt_tbl$`_boxhead` |>
      dplyr::filter(type != "hidden") |>
      dplyr::pull(var)
  )
})

test_that("as_gt passes table column alignment correctly", {
  expect_equal(
    my_tbl_regression$table_styling$header |>
      dplyr::filter(!hide) |>
      dplyr::pull(align),
    gt_tbl_regression$`_boxhead` |>
      dplyr::filter(type != "hidden") |>
      dplyr::pull(column_align)
  )

  # customize alignment
  tbl <- my_tbl_regression |>
    modify_table_styling(columns = "estimate", align = "right")
  gt_tbl <- tbl |> as_gt()

  expect_equal(
    tbl$table_styling$header |>
      dplyr::filter(!hide) |>
      dplyr::pull(align),
    gt_tbl$`_boxhead` |>
      dplyr::filter(type != "hidden") |>
      dplyr::pull(column_align)
  )
})

test_that("as_gt passes table text interpreters correctly", {
  # header
  expect_equal(
    lapply(
      my_spanning_tbl$table_styling$header$interpret_label,
      \(x) do.call(eval(parse(text = x)), list("")) |> class()
    ),
    lapply(gt_spanning_tbl$`_boxhead`$column_label, class)
  )

  # spanning header
  expect_equal(
    sapply(
      my_spanning_tbl$table_styling$spanning_header |>
        dplyr::filter(!is.na(spanning_header)) |>
        dplyr::pull(text_interpret),
      \(x) do.call(eval(parse(text = x)), list("")) |> class()
    ),
    gt_spanning_tbl$`_spanners`$spanner_label[[1]] |> class() |>
      rep(length(gt_spanning_tbl$`_spanners`$vars[[1]])),
    ignore_attr = "names"
  )

  # customize interpreter
  tbl <- my_spanning_tbl |>
    modify_table_styling(columns = "stat_1", label = "Stat I", spanning_header = "MyTest", text_interpret = "html")
  gt_tbl <- tbl |> as_gt()

  # header
  expect_equal(
    lapply(
      tbl$table_styling$header$interpret_label,
      \(x) do.call(eval(parse(text = x)), list("")) |> class()
    ),
    lapply(gt_tbl$`_boxhead`$column_label, class)
  )

  # spanning header
  expect_true(attr(gt_tbl$`_spanners`$spanner_label[[2]], "html"))

  # checking the placement of a second spanning header
  expect_silent(
    tbl2 <-
      my_spanning_tbl |>
      modify_spanning_header(all_stat_cols() ~ "**Tumor Grade**", level = 2) |>
      as_gt()
  )

  expect_equal(
    tbl2$`_spanners` |>
      dplyr::select(vars, spanner_label, spanner_level) |>
      dplyr::mutate(
        vars = map_chr(vars, ~paste(.x, collapse = ", ")),
        spanner_label = map_chr(spanner_label, as.character)
      ),
    data.frame(
      stringsAsFactors = FALSE,
      vars = c("stat_1, stat_3", "stat_1, stat_2, stat_3"),
      spanner_label = c("**Testing**", "**Tumor Grade**"),
      spanner_level = c(1L, 2L)
    )
  )
})

test_that("as_gt passes table footnotes & abbreviations correctly", {
  tbl_fn <- my_tbl_summary |>
    modify_footnote_body(footnote = "test footnote", columns = label,rows = variable == "age")
  gt_tbl_fn <- tbl_fn |> as_gt()

  # footnote
  expect_equal(
    tbl_fn$table_styling$footnote_header$column |>
      append(tbl_fn$table_styling$footnote_body$column) |>
      unique(),
    gt_tbl_fn$`_footnotes`$colname |> unique()
  )
  expect_equal(
    tbl_fn$table_styling$footnote_header$footnote,
    gt_tbl_fn$`_footnotes`$footnotes[[1]],
    ignore_attr = TRUE
  )
  expect_equal(
    tbl_fn$table_styling$footnote_body$footnote,
    gt_tbl_fn$`_footnotes`$footnotes[-1] |> unlist() |> unique(),
    ignore_attr = TRUE
  )

  tbl_fa <- tbl_fn |>
    modify_abbreviation("N = number of observations")
  gt_tbl_fa <- tbl_fa |> as_gt()

  # abbreviation
  expect_equal(
    gt_tbl_fa$`_source_notes` |>
      unlist(),
    "Abbreviation: N = number of observations"
  )

  # customized footnotes
  tbl <- my_tbl_summary |>
    modify_footnote_header("replace old footnote", columns = all_stat_cols()) |>
    modify_footnote_header("another new footnote", columns = label)
  gt_tbl <- tbl |> as_gt()

  expect_equal(
    gt_tbl$`_footnotes`$colname,
    c("stat_0", "label")
  )
  expect_equal(
    gt_tbl$`_footnotes`$footnotes |> unlist(),
    c("replace old footnote", "another new footnote")
  )

  # footnotes in the body of the table
  expect_equal(
    tbl_summary(trial, include = "age") |>
      modify_footnote_body(columns = label, rows = TRUE, footnote = "my footnote") |>
      modify_footnote_body(columns = stat_0, rows = row_type == "label", footnote = "my footnote") |>
      as_gt() |>
      getElement("_footnotes") |>
      dplyr::filter(footnotes == "my footnote") |>
      dplyr::select(colname, rownum),
    data.frame(
      colname = c("label", "label", "stat_0"),
      rownum = c(1, 2, 1)
    )
  )

  # footnotes in spanning headers
  expect_equal(
    my_spanning_tbl |>
      modify_footnote_spanning_header(
        footnote = "Testing 1 Footnote",
        columns = stat_1
      ) |>
      as_gt() |>
      getElement("_footnotes") |>
      dplyr::filter(footnotes == "Testing 1 Footnote") |>
      dplyr::pull(locname),
    "columns_groups"
  )
  expect_equal(
    my_spanning_tbl |>
      modify_spanning_header(c(stat_1, stat_2) ~ "**Another Span**", level = 2L) |>
      modify_footnote_spanning_header(
        footnote = "Testing 1 Footnote",
        columns = stat_1,
        level = 2L
      ) |>
      as_gt() |>
      getElement("_footnotes") |>
      dplyr::filter(footnotes == "Testing 1 Footnote") |>
      dplyr::pull(locname),
    "columns_groups"
  )
})

test_that("as_gt passes table indentation correctly", {
  expect_equal(
    my_tbl_summary$table_styling$indent$n_spaces[2],
    gt_tbl_summary$`_transforms`[[1]]$fn("") |> nchar()
  )

  # indentation removed
  tbl <- my_tbl_summary |>
    modify_column_indent(columns = label, indent = 0)
  gt_tbl <- tbl |> as_gt()

  expect_equal(
    gt_tbl$`_transforms` |> length(),
    0
  )
})

test_that("as_gt passes appended glance statistics correctly", {
  tbl <- my_tbl_regression |>
    add_glance_source_note(c("r.squared", "BIC")) |>
    add_glance_table(c("r.squared", "BIC"))
  gt_tbl <- tbl |> as_gt()

  loc_hline <- tbl$table_body |>
    rownames_to_column() |>
    dplyr::filter(!!tbl$table_styling$horizontal_line_above) |>
    dplyr::pull(rowname) |>
    as.numeric()

  expect_equal(
    tbl$table_body |> dplyr::select(-conf.low),
    gt_tbl$`_data` |> dplyr::select(-conf.low),
    ignore_attr = "class"
  )
  expect_equal(
    tbl$table_styling$source_note$source_note,
    gt_tbl$`_source_notes`[[2]],
    ignore_attr = "class"
  )
  expect_equal(
    loc_hline,
    gt_tbl$`_styles`$rownum |> unique()
  )
})

test_that("as_gt passes captions correctly", {
  tbl <- my_tbl_regression |>
    modify_caption("My table caption")
  gt_tbl <- tbl |> as_gt()

  expect_equal(
    tbl$table_styling$caption,
    (gt_tbl$`_options` |> dplyr::filter(parameter == "table_caption"))$value[[1]],
    ignore_attr = "class"
  )
})

test_that("as_gt passes missing symbols correctly", {
  tbl <- my_tbl_summary |>
    modify_table_body(~ .x |> mutate(stat_0 = NA_character_))
  gt_tbl <- tbl |> as_gt()

  # no substitution for missing values
  expect_equal(
    length(gt_tbl$`_substitutions`),
    1
  )

  # specify missing symbol
  tbl <- tbl |>
    modify_missing_symbol(stat_0, rows = !is.na(label), symbol = "n / a")
  gt_tbl <- tbl |> as_gt()

  # correct substitution for missing values
  expect_equal(
    tbl |>
      as_tibble(col_labels = FALSE, fmt_missing = TRUE) |>
      dplyr::pull(stat_0),
    gt_tbl$`_substitutions`[[2]]$func$default(gt_tbl$`_data`$stat_0)
  )
})

test_that("as_gt applies formatting functions correctly", {
  tbl <- glm(response ~ age + grade, trial, family = binomial(link = "logit")) |>
    tbl_regression(exponentiate = TRUE) |>
    modify_fmt_fun(
      p.value ~ function(x) style_pvalue(x, digits = 3),
      rows = variable == "grade"
    ) |>
    modify_fmt_fun(
      estimate ~ function(x) style_ratio(x, digits = 4, decimal.mark = ",")
    )
  gt_tbl <- tbl |> as_gt()

  # formatted cells
  expect_equal(
    gt_tbl$`_formats`[[12]]$func$default(gt_tbl$`_data`$p.value),
    c("0.096", NA, NA, "0.688", "0.972")
  )

  # formatted column
  expect_equal(
    gt_tbl$`_formats`[[14]]$func$default(gt_tbl$`_data`$estimate),
    c("1,0191", NA, NA, "0,8535", "1,0136")
  )

  tbl2 <- tbl_uvregression(
    trial |> dplyr::select(response, age, grade),
    method = glm,
    y = response,
    method.args = list(family = binomial),
    exponentiate = TRUE
  ) |>
    modify_fmt_fun(
      stat_n ~ function(x) style_number(x, digits = 2),
      rows = variable == "age"
    ) |>
    modify_fmt_fun(
      stat_n ~ label_style_number(digits = 4),
      rows = variable == "grade"
    ) |>
    modify_fmt_fun(
      c(conf.low, conf.high) ~ label_style_sigfig(digits = 3)
    )
  gt_tbl2 <- tbl2 |> as_gt()

  # formatted cell
  expect_equal(
    gt_tbl2$`_formats`[[22]]$func$default(gt_tbl2$`_data`$stat_n),
    c("183.0000", "193.0000", NA, NA, NA)
  )

  # formatted column
  expect_equal(
    gt_tbl2$`_formats`[[23]]$func$default(gt_tbl2$`_data`$conf.low),
    c("0.997", NA, NA, "0.446", "0.524")
  )

  expect_equal(
    gt_tbl2$`_formats`[[24]]$func$default(gt_tbl2$`_data`$conf.high),
    c("1.04", NA, NA, "2.00", "2.29")
  )
})

test_that("as_gt passes column merging correctly", {
  tbl <- lm(marker ~ age + grade, trial) |>
    tbl_regression() |>
    modify_column_merge(
      pattern = "{estimate} (pval {p.value})",
      rows = !is.na(estimate) & estimate < 0
    )
  gt_tbl <- tbl |> as_gt()

  # conf.low (default column merging)
  expect_equal(
    eval_tidy(tbl$table_styling$cols_merge$rows[[1]], data = tbl$table_body) |> which(),
    gt_tbl$`_col_merge`[[1]]$rows
  )
  expect_equal(
    gt_tbl$`_col_merge`[[1]]$vars,
    c("conf.low", "conf.high")
  )
  expect_equal(
    gt_tbl$`_col_merge`[[1]]$pattern,
    c("{1}, {2}")
  )

  # estimate (added custom column merging)
  expect_equal(
    eval_tidy(tbl$table_styling$cols_merge$rows[[2]], data = tbl$table_body) |> which(),
    gt_tbl$`_col_merge`[[2]]$rows
  )
  expect_equal(
    gt_tbl$`_col_merge`[[2]]$vars,
    c("estimate", "p.value")
  )
  expect_equal(
    gt_tbl$`_col_merge`[[2]]$pattern,
    c("{1} (pval {2})")
  )
  expect_equal(
    as.data.frame(gt_tbl)$estimate,
    c("0.00 (pval >0.9)", "<br />", "—", "-0.38 (pval 0.015)", "-0.12 (pval 0.5)")
  )

  # modify column merging pattern
  tbl <- tbl |>
    modify_table_styling(
      columns = estimate,
      rows = !is.na(estimate) & estimate < 0,
      cols_merge_pattern = "{estimate} (p is {p.value})"
    )
  gt_tbl <- tbl |> as_gt()

  expect_equal(
    gt_tbl$`_col_merge`[[2]]$pattern,
    c("{1} (p is {2})")
  )
  expect_equal(
    as.data.frame(gt_tbl)$estimate,
    c("0.00 (p is >0.9)", "<br />", "—", "-0.38 (p is 0.015)", "-0.12 (p is 0.5)")
  )

  # remove column merging
  tbl <- tbl |>
    modify_table_styling(
      columns = estimate,
      cols_merge_pattern = NA
    )
  gt_tbl <- tbl |> as_gt()

  expect_equal(length(gt_tbl$`_col_merge`), 1)
  expect_equal(
    as.data.frame(gt_tbl)$estimate,
    c("0.00", "<br />", "—", "-0.38", "-0.12")
  )
})
