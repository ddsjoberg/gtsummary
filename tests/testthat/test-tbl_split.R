skip_on_cran()

test_that("tbl_split_by_rows()", {
  expect_silent(
    tbl <- tbl_summary(trial) |>
      tbl_split_by_rows(variables = c(marker, grade))
  )

  # Must be tbl_split class
  expect_s3_class(tbl, "tbl_split")

  # First split is equal to tbl_summary with trt, marker, and grade
  expect_equal(
    tbl[[1]] |> as.data.frame(),
    tbl_summary(trial, include = c(trt, age, marker)) |> as.data.frame()
  )

  # Second split is equal to tbl_summary with stage and grade
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

  # Must be tbl_split class
  expect_s3_class(tbl, "tbl_split")

  # First split is equal to tbl_summary with stat_1
  expect_equal(
    tbl[[1]] |> as.data.frame(),
    tbl_summary(trial |> dplyr::filter(trt == "Drug A"), by = trt) |> as.data.frame()
  )

  # Second split is equal to tbl_summary with stat_2
  expect_equal(
    tbl[[2]] |> as.data.frame(),
    tbl_summary(trial |> dplyr::filter(trt == "Drug B"), by = trt) |> as.data.frame()
  )
})

test_that("tbl_split_by_columns() warns if not all columns are selected", {
  # If not all columns are selected, a warning is issued
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
  # default: footnotes = "all", caption = "all"
  expect_silent(
    def_tbl_list <- tbl_fc |>
      tbl_split_by_rows(variables = c(marker, grade))
  )

  # footnotes = "first", caption = "first"
  expect_silent(
    tbl_list <- tbl_fc |>
      tbl_split_by_rows(variables = c(marker, grade), footnotes = "first", caption = "first")
  )

  # Must be tbl_split class
  expect_s3_class(tbl_list, "tbl_split")

  # Second split does not contain footnotes and caption
  test_tbl <- def_tbl_list[[2]] |>
    remove_source_note() |>
    remove_abbreviation()
  test_tbl$table_styling$caption <- NULL
  expect_equal(
    tbl_list[[2]]$table_styling,
    test_tbl$table_styling
  )

  # First split contains footnotes and caption
  expect_equal(
    tbl_list[[1]]$table_styling,
    tbl_fc$table_styling
  )

  # footnotes = "last", caption = "last"
  expect_silent(
    tbl_list <- tbl_fc |>
      tbl_split_by_rows(variables = c(marker, grade), footnotes = "last", caption = "last")
  )

  # Must be tbl_split class
  expect_s3_class(tbl_list, "tbl_split")

  # Second split does not contain footnotes but captions
  test_tbl <- def_tbl_list[[2]] |>
    remove_source_note() |>
    remove_abbreviation()
  test_tbl$table_styling$caption <- NULL
  expect_equal(
    tbl_list[[2]]$table_styling,
    test_tbl$table_styling
  )

  # Last split does contain footnotes and caption
  expect_equal(
    tbl_list[[length(tbl_list)]]$table_styling,
    tbl_fc$table_styling
  )
})

test_that("tbl_split_by_columns(footntes, caption) works", {
  # default: footnotes = "all", caption = "all"
  expect_silent(
    def_tbl_list <- tbl_fc |>
      tbl_split_by_columns(groups = list("stat_1", "stat_2"))
  )

  # footnotes = "first", caption = "first"
  expect_silent(
    tbl_list <- tbl_fc |>
      tbl_split_by_columns(groups = list("stat_1", "stat_2"), footnotes = "first", caption = "first")
  )

  # Must be tbl_split class
  expect_s3_class(tbl_list, "tbl_split")

  # Second split does not contain footnotes and caption
  test_tbl <- def_tbl_list[[2]] |>
    remove_source_note() |>
    remove_abbreviation()
  test_tbl$table_styling$caption <- NULL
  expect_equal(
    tbl_list[[2]]$table_styling,
    test_tbl$table_styling
  )

  # First split contains footnotes and caption
  tbl_fc_test <- tbl_fc |> modify_column_hide(stat_2)
  expect_equal(
    tbl_list[[1]]$table_styling,
    tbl_fc_test$table_styling
  )

  # footnotes = "last", caption = "last"
  expect_silent(
    tbl_list <- tbl_fc |>
      tbl_split_by_columns(groups = list("stat_1", "stat_2"), footnotes = "last", caption = "last")
  )

  # Must be tbl_split class
  expect_s3_class(tbl_list, "tbl_split")

  # Second split does not contain footnotes, nor captions
  test_tbl <- def_tbl_list[[2]] |>
    remove_source_note() |>
    remove_abbreviation()
  tbl_fc_test <- tbl_fc |> modify_column_hide(stat_1)
  expect_equal(
    tbl_list[[2]]$table_styling,
    tbl_fc_test$table_styling
  )

  # Last split does contain footnotes and caption
  expect_equal(
    tbl_list[[length(tbl_list)]]$table_styling,
    tbl_fc_test$table_styling
  )
})

# row_numbers -------------------------------------------------------------
test_that("tbl_split_by_rows(row_numbers) works", {
  tbl <- trial |>
    tbl_summary(by = trt)

  # Create a tbl_split object by splitting the first two rows and last row
  expect_silent(
    tbl_lst <- tbl |>
      tbl_split_by_rows(row_numbers = c(1, 2, nrow(tbl$table_body) - 1))
  )

  # If the last row is selected the split will be the same
  expect_equal(
    as.data.frame(tbl),
    tbl_split_by_rows(tbl, row_numbers = nrow(tbl$table_body))[[1]] |> as.data.frame()
  )

  # Must be tbl_split class
  expect_s3_class(tbl_lst, "tbl_split")

  # First split contains first row
  expect_equal(
    tbl_lst[[1]] |> as.data.frame(),
    tbl_summary(trial, by = trt) |> as.data.frame() |> dplyr::slice(1)
  )

  # Second split contains second row
  expect_equal(
    tbl_lst[[2]] |> as.data.frame(),
    tbl_summary(trial, by = trt) |> as.data.frame() |> dplyr::slice(2)
  )

  # Third split contains all other rows
  expect_equal(
    tbl_lst[[3]] |> as.data.frame(),
    tbl_summary(trial, by = trt) |> as.data.frame() |> dplyr::slice(-c(1, 2, nrow(tbl$table_body)))
  )

  # Fourth split contains last row
  expect_equal(
    tbl_lst[[4]] |> as.data.frame(),
    tbl_summary(trial, by = trt) |> as.data.frame() |> dplyr::slice(nrow(tbl$table_body))
  )

  # no errors if the table does not have a 'variable' column
  expect_length(
    mtcars |>
      as_gtsummary() |>
      tbl_split_by_rows(row_numbers = 15),
    2L
  )
})

test_that("tbl_split_by_rows(row_numbers) throws errors", {
  # If row_numbers is not in range, an error is thrown
  expect_snapshot(
    error = TRUE,
    tbl_lst <- trial |>
      tbl_summary(by = trt) |>
      tbl_split_by_rows(row_numbers = -1)
  )

  # If row_numbers is not numeric, an error is thrown
  expect_snapshot(
    error = TRUE,
    tbl_lst <- trial |>
      tbl_summary(by = trt) |>
      tbl_split_by_rows(row_numbers = "grade")
  )

  # appropriate error when the table does not have a 'variable' column
  expect_snapshot(
    error = TRUE,
    mtcars |>
      as_gtsummary() |>
      tbl_split_by_rows(variables = 1L)
  )
})

test_that("tbl_split_by_rows(row_numbers, variables) throws an error", {
  # variables and row_numbers cannot be used together
  expect_snapshot(
    error = TRUE,
    tbl_lst <- trial |>
      tbl_summary(by = trt) |>
      tbl_split_by_rows(row_numbers = c(2), variables = grade)
  )
})
