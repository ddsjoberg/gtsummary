skip_on_cran()

my_tbl_summary <- trial |>
  select(trt, age, death) |>
  tbl_summary()
my_tbl_cross <- tbl_cross(trial, trt, grade)
my_tbl_regression <- lm(marker ~ age, trial) |> tbl_regression()
my_tbl_uvregression <- trial |> tbl_uvregression(method = lm, y = age)
my_spanning_tbl <- trial |>
  tbl_summary(by = grade, include = age) |>
  modify_spanning_header(c(stat_1, stat_3) ~ "**Testing**")

expect_silent(gt_tbl_summary <- my_tbl_summary |> as_gt())
expect_silent(gt_tbl_cross <- my_tbl_cross |> as_gt())
expect_silent(gt_tbl_regression <- my_tbl_regression |> as_gt())
expect_silent(gt_tbl_uvregression <- my_tbl_uvregression |> as_gt())
expect_silent(gt_spanning_tbl <- my_spanning_tbl |> as_gt())

test_that("as_gt works with standard use", {
  expect_silent(my_tbl_summary |> as_gt(return_calls = TRUE))
  expect_silent(my_tbl_summary |> as_gt(include = gt))
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
  expect_equal(
    my_tbl_cross$table_body,
    gt_tbl_cross$`_data`
  )

  # tbl_regression
  expect_equal(
    my_tbl_regression$table_body |>
      dplyr::mutate(conf.low = paste0(round(conf.low, digits = 2), ", ", round(conf.high, digits = 2))),
    gt_tbl_regression$`_data`,
    ignore_attr = "class"
  )

  # tbl_uvregression
  expect_equal(
    my_tbl_uvregression$table_body |>
      dplyr::select(-conf.low),
    gt_tbl_uvregression$`_data` |>
      dplyr::select(-conf.low),
    ignore_attr = "class"
  )

  # spanning_tbl
  expect_equal(
    my_spanning_tbl$table_body,
    gt_spanning_tbl$`_data`
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
    my_spanning_tbl$table_styling$header |>
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
      my_spanning_tbl$table_styling$header |>
        dplyr::filter(!is.na(spanning_header)) |>
        dplyr::pull(interpret_spanning_header),
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
  if ("html" %in% names(attributes(gt_tbl$`_spanners`$spanner_label[[1]]))) {
    attr(gt_tbl$`_spanners`$spanner_label[[1]], "html") <- c("html", "character")
  }

  # header
  expect_equal(
    lapply(
      tbl$table_styling$header$interpret_label,
      \(x) do.call(eval(parse(text = x)), list("")) |> class()
    ),
    lapply(gt_tbl$`_boxhead`$column_label, class)
  )

  # spanning header
  expect_equal(
    sapply(
      tbl$table_styling$header |>
        dplyr::filter(!is.na(spanning_header)) |>
        dplyr::pull(interpret_spanning_header),
      \(x) do.call(eval(parse(text = x)), list("")) |> class()
    ),
    attributes(gt_tbl$`_spanners`$spanner_label[[1]]),
    ignore_attr = "names"
  )
})

test_that("as_gt passes table footnotes & footnote abbreviations correctly", {
  # footnote
  expect_equal(
    my_tbl_summary$table_styling$footnote$column,
    gt_tbl_summary$`_footnotes`$colname |> unique()
  )
  expect_equal(
    my_tbl_summary$table_styling$footnote$footnote,
    gt_tbl_summary$`_footnotes`$footnotes |> unlist() |> unique()
  )

  tbl_fa <- my_tbl_summary |>
    modify_footnote(stat_0 = "N = number of observations", abbreviation = TRUE)
  gt_tbl_fa <- tbl_fa |> as_gt()

  # footnote_abbrev
  vis_cols <- tbl_fa$table_styling$header |>
    dplyr::filter(hide == FALSE) |>
    dplyr::select(column) |>
    unlist()
  footnotes_vis <- rbind(
    tbl_fa$table_styling$footnote,
    tbl_fa$table_styling$footnote_abbrev
  ) |>
    dplyr::filter(column %in% vis_cols) |>
    dplyr::arrange(rows)
  expect_equal(
    footnotes_vis$column,
    gt_tbl_fa$`_footnotes` |>
      dplyr::distinct(pick(!any_of("rownum"))) |>
      dplyr::arrange(locnum) |>
      dplyr::pull(colname)
  )
  expect_equal(
    footnotes_vis$footnote |> unname(),
    gt_tbl_fa$`_footnotes` |>
      dplyr::distinct(pick(!any_of("rownum"))) |>
      dplyr::arrange(locnum) |>
      dplyr::pull(footnotes) |>
      unlist()
  )

  # customized footnotes
  tbl <- my_tbl_summary |>
    modify_footnote(
      all_stat_cols() ~ "replace old footnote",
      label = "another new footnote"
    )
  gt_tbl <- tbl |> as_gt()

  expect_equal(
    tbl$table_styling$footnote$column,
    gt_tbl$`_footnotes`$colname
  )
  expect_equal(
    tbl$table_styling$footnote$footnote,
    gt_tbl$`_footnotes`$footnotes |> unlist()
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
    tibble::rownames_to_column() |>
    dplyr::filter(!!tbl$table_styling$horizontal_line_above) |>
    dplyr::pull(rowname) |>
    as.numeric()

  expect_equal(
    tbl$table_body |> dplyr::select(-conf.low),
    gt_tbl$`_data` |> dplyr::select(-conf.low),
    ignore_attr = "class"
  )
  expect_equal(
    tbl$table_styling$source_note,
    gt_tbl$`_source_notes`[[1]],
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
