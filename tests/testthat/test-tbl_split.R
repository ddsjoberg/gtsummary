skip_on_cran()

test_that("tbl_split_by_rows()", {
  expect_silent(
    tbl <- tbl_summary(trial) |>
      tbl_split_by_rows(variables = c(marker, grade))
  )
  expect_s3_class(tbl, "tbl_split")

  expect_equal(
    tbl[[1]] |> as.data.frame(),
    tbl_summary(trial, include = c(trt, age, marker)) |> as.data.frame()
  )

  expect_equal(
    tbl[[2]] |> as.data.frame(),
    tbl_summary(trial, include = c(stage, grade)) |> as.data.frame()
  )
})

test_that("tbl_split_by_columns()", {
  expect_silent(
    tbl <- tbl_summary(trial, by = trt) |>
      tbl_split_by_columns(groups = list("stat_1", "stat_2"))
  )
  expect_s3_class(tbl, "tbl_split")


  expect_equal(
    tbl[[1]] |> as.data.frame(),
    tbl_summary(trial |> dplyr::filter(trt == "Drug A"), by = trt) |> as.data.frame()
  )

  expect_equal(
    tbl[[2]] |> as.data.frame(),
    tbl_summary(trial |> dplyr::filter(trt == "Drug B"), by = trt) |> as.data.frame()
  )
})

test_that("tbl_split_by_columns() warns if not all columns are selected", {
  expect_snapshot(
    tbl <- tbl_summary(trial, by = trt) |>
      tbl_split_by_columns(groups = list("stat_2"))
  )
})

# footnotes/caption ------------------------------------------------------------
# create standard table
tbl_fc <- trial |>
  tbl_summary(by = trt, missing = "no") |>
  modify_footnote_header(
    footnote = "All but four subjects received both treatments in a crossover design",
    columns = all_stat_cols(),
    replace = FALSE
  ) |>
  modify_footnote_body(
    footnote = "Tumor grade was assessed _before_ treatment began",
    columns = "label",
    rows = variable == "grade" & row_type == "label"
  ) |>
  modify_spanning_header(
    c(stat_1, stat_2) ~ "**TRT**"
  ) |>
  modify_abbreviation("I = 1, II = 2, III = 3") |>
  modify_caption("_Some caption_") |>
  modify_footnote_spanning_header(
    footnote = "Treatment",
    columns = c(stat_1)
  ) |>
  modify_source_note("Some source note!")


test_that("tbl_split_by_rows(footntes, caption) works", {
  expect_silent(
    def_tbl_list <- tbl_fc |>
      tbl_split_by_rows(variables = c(marker, grade))
  )

  # footnotes = "first", caption = "first"
  expect_silent(
    tbl_list <- tbl_fc |>
      tbl_split_by_rows(variables = c(marker, grade), footnotes = "first", caption = "first")
  )
  expect_s3_class(tbl_list, "tbl_split")

  test_tbl <- def_tbl_list[[2]] |> remove_source_note() |> remove_abbreviation()
  test_tbl$table_styling$caption <- NULL
  expect_equal(
    tbl_list[[2]]$table_styling,
    test_tbl$table_styling
  )
  expect_equal(
    tbl_list[[1]]$table_styling,
    tbl_fc$table_styling
  )

  # footnotes = "last", caption = "all"
  expect_silent(
    tbl_list <- tbl_fc |>
      tbl_split_by_rows(variables = c(marker, grade), footnotes = "last", caption = "all")
  )
  expect_s3_class(tbl_list, "tbl_split")

  test_tbl <- def_tbl_list[[2]] |> remove_source_note() |> remove_abbreviation()
  expect_equal(
    tbl_list[[2]]$table_styling,
    test_tbl$table_styling
  )
  expect_equal(
    tbl_list[[length(tbl_list)]]$table_styling,
    tbl_fc$table_styling
  )
})

test_that("tbl_split_by_columns(footntes, caption) works", {
  expect_silent(
    def_tbl_list <- tbl_fc |>
      tbl_split_by_columns(groups = list("stat_1", "stat_2"))
  )

  # footnotes = "first", caption = "first"
  expect_silent(
    tbl_list <- tbl_fc |>
      tbl_split_by_columns(groups = list("stat_1", "stat_2"), footnotes = "first", caption = "first")
  )
  expect_s3_class(tbl_list, "tbl_split")

  test_tbl <- def_tbl_list[[2]] |> remove_source_note() |> remove_abbreviation()
  test_tbl$table_styling$caption <- NULL
  expect_equal(
    tbl_list[[2]]$table_styling,
    test_tbl$table_styling
  )
  tbl_fc_test <- tbl_fc |> modify_column_hide(stat_2)
  expect_equal(
    tbl_list[[1]]$table_styling,
    tbl_fc_test$table_styling
  )

  # footnotes = "last", caption = "all"
  expect_silent(
    tbl_list <- tbl_fc |>
      tbl_split_by_columns(groups = list("stat_1", "stat_2"), footnotes = "last", caption = "all")
  )
  expect_s3_class(tbl_list, "tbl_split")

  test_tbl <- def_tbl_list[[2]] |> remove_source_note() |> remove_abbreviation()
  tbl_fc_test <- tbl_fc |> modify_column_hide(stat_1)
  expect_equal(
    tbl_list[[2]]$table_styling,
    tbl_fc_test$table_styling
  )
  expect_equal(
    tbl_list[[length(tbl_list)]]$table_styling,
    tbl_fc_test$table_styling
  )
})

# row_numbers -------------------------------------------------------------
test_that("tbl_split_by_rows(row_numbers) works", {
  tbl_lst <- trial |>
    tbl_summary(by = trt) |>
    tbl_split_by_rows(row_numbers = c(1, 2))

  expect_s3_class(tbl_lst, "tbl_split")

  expect_equal(
    tbl_lst[[1]] |> as.data.frame(),
    tbl_summary(trial, by = trt) |> as.data.frame() |> dplyr::slice(1)
  )
  expect_equal(
    tbl_lst[[2]] |> as.data.frame(),
    tbl_summary(trial, by = trt) |> as.data.frame() |> dplyr::slice(2)
  )
  expect_equal(
    tbl_lst[[3]] |> as.data.frame(),
    tbl_summary(trial, by = trt) |> as.data.frame() |> dplyr::slice(- c(1, 2))
  )
})

test_that("tbl_split_by_rows(row_numbers) throws errors", {
  expect_snapshot(
    error = TRUE,
    tbl_lst <- trial |>
      tbl_summary(by = trt) |>
      tbl_split_by_rows(row_numbers = -1)
  )
  expect_snapshot(
    error = TRUE,
    tbl_lst <- trial |>
      tbl_summary(by = trt) |>
      tbl_split_by_rows(row_numbers = "grade")
  )
})

test_that("tbl_split_by_rows(row_numbers, variables) throws an error", {
  expect_snapshot(
    error = TRUE,
    tbl_lst <- trial |>
      tbl_summary(by = trt) |>
      tbl_split_by_rows(row_numbers = c(2), variables = grade)
  )
})
