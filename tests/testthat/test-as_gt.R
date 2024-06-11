test_that("as_gt works with standard use", {
  my_tbl_summary <- trial |>
    select(trt, age, death) |>
    tbl_summary() |>
    modify_table_styling(columns = label, footnote = "test footnote", rows = variable == "age")

  expect_silent(res <- my_tbl_summary |> as_gt())
  expect_silent(my_tbl_summary |> as_gt(return_calls = TRUE))
  expect_equal(
    names(res),
    c("_data", "_boxhead", "_stub_df", "_row_groups", "_heading", "_spanners", "_stubhead", "_footnotes",
      "_source_notes", "_formats", "_substitutions", "_styles", "_summary", "_options", "_transforms",
      "_locale", "_has_built")
  )

  # body
  expect_equal(
    my_tbl_summary$table_body,
    res$`_data`
  )

  # header
  expect_equal(
    my_tbl_summary$table_styling$header$column,
    res$`_boxhead`$var
  )
  expect_equal(
    my_tbl_summary$table_styling$header$label,
    res$`_boxhead`$column_label |> unlist()
  )

  # footnotes
  expect_equal(
    my_tbl_summary$table_styling$footnote$column,
    res$`_footnotes`$colname |> unique()
  )
  expect_equal(
    my_tbl_summary$table_styling$footnote$footnote,
    res$`_footnotes`$footnotes |> unlist() |> unique()
  )

  # indentation
  expect_equal(
    my_tbl_summary$table_styling$indent$n_spaces[2],
    res$`_transforms`[[1]]$fn("") |> nchar()
  )
})

test_that("as_gt works with tbl_cross", {
  my_tbl_cross <- tbl_cross(trial, trt, grade)

  expect_silent(res <- my_tbl_cross |> as_gt())
  expect_equal(
    names(res),
    c("_data", "_boxhead", "_stub_df", "_row_groups", "_heading", "_spanners", "_stubhead", "_footnotes",
      "_source_notes", "_formats", "_substitutions", "_styles", "_summary", "_options", "_transforms",
      "_locale", "_has_built")
  )

  # body
  expect_equal(
    my_tbl_cross$table_body,
    res$`_data`
  )

  # header
  expect_equal(
    my_tbl_cross$table_styling$header$column,
    res$`_boxhead`$var
  )
  expect_equal(
    my_tbl_cross$table_styling$header$label,
    res$`_boxhead`$column_label |> unlist()
  )

  # indentation
  expect_equal(
    my_tbl_cross$table_styling$indent$n_spaces[2],
    res$`_transforms`[[1]]$fn("") |> nchar()
  )
})

test_that("as_gt works with tbl_regression", {
  my_tbl_regression <- lm(marker ~ age, trial) |> tbl_regression()

  expect_silent(res <- my_tbl_regression |> as_gt())
  expect_equal(
    names(res),
    c("_data", "_boxhead", "_stub_df", "_row_groups", "_heading", "_spanners", "_stubhead", "_footnotes",
      "_source_notes", "_formats", "_substitutions", "_styles", "_summary", "_options", "_transforms",
      "_locale", "_has_built")
  )

  # body
  expect_equal(
    my_tbl_regression$table_body |>
      mutate(conf.low = paste0(round(conf.low, digits = 2), ", ", round(conf.high, digits = 2))),
    res$`_data`,
    ignore_attr = "class"
  )

  # header
  expect_equal(
    my_tbl_regression$table_styling$header$column,
    res$`_boxhead`$var
  )
  expect_equal(
    my_tbl_regression$table_styling$header$label,
    res$`_boxhead`$column_label |> unlist()
  )

  # footnotes
  vis_cols <- my_tbl_uvregression$table_styling$header |> filter(hide == FALSE) |> select(column) |> unlist()
  footnotes_vis <- my_tbl_regression$table_styling$footnote_abbrev |> filter(column %in% vis_cols)
  expect_equal(
    footnotes_vis$column,
    res$`_footnotes`$colname |> unique()
  )
  expect_equal(
    footnotes_vis$footnote,
    res$`_footnotes`$footnotes |> unlist() |> unique()
  )
})

test_that("as_gt works with tbl_uvregression", {
  my_tbl_uvregression <- trial |> tbl_uvregression(method = lm, y = age)

  expect_silent(res <- my_tbl_uvregression |> as_gt())
  expect_equal(
    names(res),
    c("_data", "_boxhead", "_stub_df", "_row_groups", "_heading", "_spanners", "_stubhead", "_footnotes",
      "_source_notes", "_formats", "_substitutions", "_styles", "_summary", "_options", "_transforms",
      "_locale", "_has_built")
  )

  # body
  expect_equal(
    my_tbl_uvregression$table_body |>
      select(-conf.low),
    res$`_data` |>
      select(-conf.low),
    ignore_attr = "class"
  )

  # header
  expect_equal(
    my_tbl_uvregression$table_styling$header$column,
    res$`_boxhead`$var
  )
  expect_equal(
    my_tbl_uvregression$table_styling$header$label,
    res$`_boxhead`$column_label |> unlist()
  )

  # footnotes
  vis_cols <- my_tbl_uvregression$table_styling$header |> filter(hide == FALSE) |> select(column) |> unlist()
  footnotes_vis <- my_tbl_regression$table_styling$footnote_abbrev |> filter(column %in% vis_cols)
  expect_equal(
    footnotes_vis$column,
    res$`_footnotes`$colname |> unique()
  )
  expect_equal(
    footnotes_vis$footnote,
    res$`_footnotes`$footnotes |> unlist() |> unique()
  )

  # indentation
  expect_equal(
    my_tbl_uvregression$table_styling$indent$n_spaces[2],
    res$`_transforms`[[1]]$fn("") |> nchar()
  )
})

test_that("as_gt works with spanning header-column gathering", {
  my_spanning_tbl <- trial |>
    tbl_summary(
      by = grade,
      include = age
    ) |>
    modify_spanning_header(c(stat_1, stat_3) ~ "**Testing**")

  expect_silent(res <- my_spanning_tbl |> as_gt())
  expect_equal(
    names(res),
    c("_data", "_boxhead", "_stub_df", "_row_groups", "_heading", "_spanners", "_stubhead", "_footnotes",
      "_source_notes", "_formats", "_substitutions", "_styles", "_summary", "_options", "_transforms",
      "_locale", "_has_built")
  )

  # body
  expect_equal(
    my_spanning_tbl$table_body,
    res$`_data`
  )

  # header
  expect_equal(
    my_spanning_tbl$table_styling$header$column,
    res$`_boxhead`$var
  )
  expect_equal(
    my_spanning_tbl$table_styling$header$label,
    res$`_boxhead`$column_label |> unlist()
  )

  # footnotes
  expect_equal(
    my_spanning_tbl$table_styling$footnote$column,
    res$`_footnotes`$colname
  )
  expect_equal(
    my_spanning_tbl$table_styling$footnote$footnote,
    res$`_footnotes`$footnotes |> unlist()
  )

  # indentation
  expect_equal(
    my_spanning_tbl$table_styling$indent$n_spaces[2],
    res$`_transforms`[[1]]$fn("") |> nchar()
  )
})
