
#'Footnotes types available
#'
#'@description
#'There are several 'notes' that go into the a summary table on {gtsummary}. It is important to know the tables #'header and body labels to avoid unclear, misplaced, or even misleading notes. To show header labels and the table #'body use;
#'`show_header_names()` and `tbl_0$table_body` respectively.
#'
#'@name function
#'@keywords internal
#'@section `show_header_names()`:
#'
#'
#'```r
#'tbl_0 <- trial |>
#'  tbl_summary(by = trt, include = grade, missing = "no")
#'
#'tbl_0
#'show_header_names(tbl_0)
#'```
#'
#'@name funtion
#' @keywords internal
#' @section `tbl_0$table_body`:
#'```r
#'tbl_0 <- trial |>
#'tbl_summary(by = trt, include = grade, missing = "no")
#'
#'tbl_0
#'tbl_0$table_body
#'```
#'
#'@name footnote
#'@keywords internal
#'@section `modify_spanning_header()``
#'
#' @description
#' Adds notes to the whole block title spanning across several 'stats' columns (stat_1, #'stat_2 above). e.g. the all treatment columns below.
#'
#'```r
#'tbl_0 |>
#'  modify_spanning_header(all_stat_cols() ~ "**Treatment**") |>
#'  modify_footnote_spanning_header(
#'    footnote = "This is a footnote spanning header",
#'    columns = all_stat_cols(),
#'    level = 1
#'  )
#'```
#'@name footnote
#'@keywords internal
#'@section `modify_footnote_header()`
#'
#'@description
#'Adds a note to the column header (level 1 of the table structure at the top of a #'column), to explaining  column-level notes that apply to an entire column e.g., “n is the number of patients on a #'specific drug.”
#'
#'```r
#'tbl_1 <- tbl_0 |>
#'  modify_spanning_header(all_stat_cols() ~ "**Treatment**") |>
#'  modify_footnote_spanning_header(
#'    footnote = "This is a footnote spanning header",
#'    columns = all_stat_cols(),
#'    level = 1
#'  ) |>
#'  modify_footnote_header(
#'    footnote = "This is a footnote header",
#'    columns = all_stat_cols(),
#'    replace = FALSE
#'  )
#'print(tbl_1)
#'```
#'
#'@name footnote
#'@keywords internal
#'@section `modify_footnote_body()`
#'
#'@description
#'Adds notes to a specific row or cell within the body of the table  i.e. body footnotes. #'It is linked to the label column, which contains variable names or categories, to clarify or qualify  values.
#'
#'```r
#'tbl_1 <- tbl_0 |>
#'  modify_spanning_header(all_stat_cols() ~ "**Treatment**") |>
#'  modify_footnote_spanning_header(
#'    footnote = "This is a footnote spanning header",
#'    columns = all_stat_cols(),
#'    level = 1
#'  ) |>
#'  modify_footnote_header(
#'    footnote = "This is a footnote header",
#'    columns = all_stat_cols(),
#'    replace = FALSE
#'  ) |>
#'  modify_footnote_body(
#'    footnote = "This is a footnote body",
#'    columns = "label",
#'    rows = variable == "grade" & row_type == "label"
#'  )
#'print(tbl_1)
#'```
#'@name footnote
#'@keywords internal
#'@section `modify_abbreviation()`
#'
#'@description
#'Adds footnotes that explain shortened forms of words/phrases (e.g., “CI” for confidence #'interval, “N” for sample size)
#'
#'```r
#'
#'tbl_1 <- tbl_0 |>
#'  modify_spanning_header(all_stat_cols() ~ "**Treatment**") |>
#'  modify_footnote_spanning_header(
#'    footnote = "This is a footnote spanning header",
#'    columns = all_stat_cols(),
#'    level = 1
#'  ) |>
#'  modify_footnote_header(
#'    footnote = "This is a footnote header",
#'    columns = all_stat_cols(),
#'    replace = FALSE
#'  ) |>
#'  modify_footnote_body(
#'    footnote = "This is a footnote body",
#'    columns = "label",
#'    rows = variable == "grade" & row_type == "label"
#'  ) |>
#'  modify_abbreviation(
#'    abbreviation = "This is an abbreviation"
#'  )
#'
#'print(tbl_1)
#'```
#'
#'@name footnote
#'@keywords internal
#'@section `modify_source_notes()`
#'
#'@description
#'
#'Adds source notes which specify where the data in the table came from to credit or cite the #'data source.
#'
#'```r
#'tbl_1 <- tbl_0 |>
#'  modify_spanning_header(all_stat_cols() ~ "**Treatment**") |>
#'  modify_footnote_spanning_header(
#'    footnote = "This is a footnote spanning header",
#'    columns = all_stat_cols(),
#'    level = 1
#'  ) |>
#'  modify_footnote_header(
#'    footnote = "This is a footnote header",
#'    columns = all_stat_cols(),
#'    replace = FALSE
#'  ) |>
#'  modify_footnote_body(
#'    footnote = "This is a footnote body",
#'    columns = "label",
#'    rows = variable == "grade" & row_type == "label"
#'  ) |>
#'  modify_abbreviation(
#'    abbreviation = "This is an abbreviation"
#'  ) |>
#'  modify_source_note(
#'    source_note = "This is a source note"
#'  )
#'
#'print(tbl_1)
#'```


