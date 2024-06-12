skip_on_cran()

my_tbl_summary <- trial |>
  select(trt, age, death) |>
  tbl_summary() |>
  modify_table_styling(columns = label, footnote = "test footnote", rows = variable == "age")
my_tbl_cross <- tbl_cross(trial, trt, grade)
my_tbl_regression <- lm(marker ~ age, trial) |> tbl_regression()
my_tbl_uvregression <- trial |> tbl_uvregression(method = lm, y = age)
my_spanning_tbl <- trial |>
  tbl_summary(
    by = grade,
    include = age
  ) |>
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
      select(-conf.low),
    gt_tbl_uvregression$`_data` |>
      select(-conf.low),
    ignore_attr = "class"
  )

  # spanning_tbl
  expect_equal(
    my_spanning_tbl$table_body,
    gt_spanning_tbl$`_data`
  )
})

test_that("as_gt passes table header correctly", {
  # tbl_summary
  expect_equal(
    my_tbl_summary$table_styling$header$column,
    gt_tbl_summary$`_boxhead`$var
  )
  expect_equal(
    my_tbl_summary$table_styling$header$label,
    gt_tbl_summary$`_boxhead`$column_label |> unlist()
  )

  # tbl_cross
  expect_equal(
    my_tbl_cross$table_styling$header$column,
    gt_tbl_cross$`_boxhead`$var
  )
  expect_equal(
    my_tbl_cross$table_styling$header$label,
    gt_tbl_cross$`_boxhead`$column_label |> unlist()
  )

  # tbl_regression
  expect_equal(
    my_tbl_regression$table_styling$header$column,
    gt_tbl_regression$`_boxhead`$var
  )
  expect_equal(
    my_tbl_regression$table_styling$header$label,
    gt_tbl_regression$`_boxhead`$column_label |> unlist()
  )

  # tbl_uvregression
  expect_equal(
    my_tbl_uvregression$table_styling$header$column,
    gt_tbl_uvregression$`_boxhead`$var
  )
  expect_equal(
    my_tbl_uvregression$table_styling$header$label,
    gt_tbl_uvregression$`_boxhead`$column_label |> unlist()
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
})

test_that("as_gt passes table footnotes/footnote abbreviations correctly", {
  # tbl_summary
  expect_equal(
    my_tbl_summary$table_styling$footnote$column,
    gt_tbl_summary$`_footnotes`$colname |> unique()
  )
  expect_equal(
    my_tbl_summary$table_styling$footnote$footnote,
    gt_tbl_summary$`_footnotes`$footnotes |> unlist() |> unique()
  )

  # tbl_regression
  vis_cols <- my_tbl_regression$table_styling$header |>
    dplyr::filter(hide == FALSE) |>
    dplyr::select(column) |>
    unlist()
  footnotes_vis <- my_tbl_regression$table_styling$footnote_abbrev |>
    dplyr::filter(column %in% vis_cols)
  expect_equal(
    footnotes_vis$column,
    gt_tbl_regression$`_footnotes`$colname |> unique()
  )
  expect_equal(
    footnotes_vis$footnote,
    gt_tbl_regression$`_footnotes`$footnotes |> unlist() |> unique()
  )

  # tbl_uvregression
  vis_cols <- my_tbl_uvregression$table_styling$header |>
    dplyr::filter(hide == FALSE) |>
    dplyr::select(column) |>
    unlist()
  footnotes_vis <- my_tbl_uvregression$table_styling$footnote_abbrev |>
    dplyr::filter(column %in% vis_cols) |>
    unique()
  expect_equal(
    footnotes_vis$column,
    gt_tbl_uvregression$`_footnotes`$colname |> unique()
  )
  expect_equal(
    footnotes_vis$footnote,
    gt_tbl_uvregression$`_footnotes`$footnotes |> unlist() |> unique()
  )

  # spanning_tbl
  expect_equal(
    my_spanning_tbl$table_styling$footnote$column,
    gt_spanning_tbl$`_footnotes`$colname
  )
  expect_equal(
    my_spanning_tbl$table_styling$footnote$footnote,
    gt_spanning_tbl$`_footnotes`$footnotes |> unlist()
  )
})

test_that("as_gt passes table indentation correctly", {
  # tbl_summary
  expect_equal(
    my_tbl_summary$table_styling$indent$n_spaces[2],
    gt_tbl_summary$`_transforms`[[1]]$fn("") |> nchar()
  )

  # tbl_cross
  expect_equal(
    my_tbl_cross$table_styling$indent$n_spaces[2],
    gt_tbl_cross$`_transforms`[[1]]$fn("") |> nchar()
  )

  # tbl_uvregression
  expect_equal(
    my_tbl_uvregression$table_styling$indent$n_spaces[2],
    gt_tbl_uvregression$`_transforms`[[1]]$fn("") |> nchar()
  )

  # spanning_tbl
  expect_equal(
    my_spanning_tbl$table_styling$indent$n_spaces[2],
    gt_spanning_tbl$`_transforms`[[1]]$fn("") |> nchar()
  )
})
