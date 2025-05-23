#' Split gtsummary table by rows and/or columns
#'
#' `r lifecycle::badge('experimental')`\cr
#' The `tbl_split_by_rows()` and `tbl_split_by_columns()` functions split a single
#' gtsummary table into multiple tables.
#' Both column-wise splitting (that is, splits by columns in
#' `x$table_body`) and row-wise splitting is possible.
#'
#' @param x (`gtsummary` or `list`)\cr
#'   gtsummary table.
#' @param variables ([`tidy-select`][dplyr::dplyr_tidy_select])\cr
#'   variables at which to split the gtsummary table rows (tables
#'   will be separated after each of these variables).
#' @param keys ([`tidy-select`][dplyr::dplyr_tidy_select])\cr
#'   columns to be repeated in each table split. It defaults to `first_column(x)`
#'   if missing (usually label column).
#' @param groups (list of `character` vectors)\cr
#'   list of column names that appear in `x$table_body`.
#'   Each group of column names represent a different table in the output list.
#' @param footnotes (`string`) `r lifecycle::badge('experimental')`\cr
#'   can be either `"first"`, `"all"`, or `"last"`, to locate global footnotes
#'   only on the first, in each, or in the last table, respectively. It defaults
#'   to `"all"`. Reference footnotes are always present wherever they appear.
#' @param caption (`string`) `r lifecycle::badge('experimental')`\cr
#'   similarly to footnotes, makes the titles, and subtitles appear only on the
#'   `"first"` or `"all"` table. It defaults to `"all"`.
#' @inheritParams rlang::args_dots_empty
#'
#' @return `tbl_split` object. If multiple splits are performed (e.g., both by
#'   row and columns), the output is returned a single level list.
#'
#' @details
#' Run [show_header_names()] to print all column names to split by.
#'
#' Footnotes and caption handling are experimental and may change in the future.
#'
#' @examplesIf identical(Sys.getenv("NOT_CRAN"), "true") || identical(Sys.getenv("IN_PKGDOWN"), "true")
#' # Example 1 ----------------------------------
#' # Split by rows
#' trial |>
#'   tbl_summary(by = trt) |>
#'   tbl_split_by_rows(variables = c(marker, grade)) |>
#'   tail(n = 1) # Print only last table for simplicity
#'
#' # Example 2 ----------------------------------
#' # Split by columns
#' trial |>
#'   tbl_summary(by = trt, include = c(death, ttdeath)) |>
#'   tbl_split_by_columns(groups = list("stat_1", "stat_2")) |>
#'   tail(n = 1) # Print only last table for simplicity
#'
#' # Example 3 ----------------------------------
#' # Both row and column splitting
#' trial |>
#'   tbl_summary(by = trt) |>
#'   tbl_split_by_rows(variables = c(marker, grade)) |>
#'   tbl_split_by_columns(groups = list("stat_1", "stat_2")) |>
#'   tail(n = 1) # Print only last table for simplicity
#'
#' # Example 4 ------------------------------
#' trial |>
#'   tbl_summary(by = trt, missing = "no") |>
#'   modify_footnote_header(
#'     footnote = "All but four subjects received both treatments in a crossover design",
#'     columns = all_stat_cols(),
#'     replace = FALSE
#'   ) |>
#'   modify_footnote_body(
#'     footnote = "Tumor grade was assessed _before_ treatment began",
#'     columns = "label",
#'     rows = variable == "grade" & row_type == "label"
#'   ) |>
#'   modify_spanning_header(
#'     c(stat_1, stat_2) ~ "**TRT**"
#'   ) |>
#'   modify_abbreviation("I = 1, II = 2, III = 3") |>
#'   modify_caption("_Some caption_") |>
#'   modify_footnote_spanning_header(
#'     footnote = "Treatment",
#'     columns = c(stat_1)
#'   ) |>
#'   modify_source_note("Some source note!") |>
#'   tbl_split_by_rows(variables = c(marker, stage, grade), footnotes = "last", caption = "first") |>
#'   tail(n = 2) |>
#'   head(n = 1) # Print only one but not last table for simplicity
#'
#' @name tbl_split_by
NULL

#' @export
#' @rdname tbl_split_by
tbl_split_by_rows <- function(x, variables,
                              footnotes = c("all", "first", "last"),
                              caption = c("all", "first")) {
  set_cli_abort_call()

  # list map -------------------------------------------------------------------
  if (inherits(x, "list")) {
    tbl_list <- map(
      .x = x,
      .f = tbl_split_by_rows,
      variables = {{ variables }}
    )

    return(
      tbl_list |>
        unlist(recursive = FALSE) |>
        structure(class = c("tbl_split", "list"))
    )
  }

  # check inputs ---------------------------------------------------------------
  check_class(x, "gtsummary")

  # process inputs -------------------------------------------------------------
  cards::process_selectors(
    data = scope_table_body(x$table_body),
    variables = {{ variables }}
  )
  # adding last variable
  variables <- variables |> union(dplyr::last(x$table_body$variable))

  # footnotes and caption check
  footnotes <- rlang::arg_match(footnotes)
  caption <- rlang::arg_match(caption)

  # merging split points -------------------------------------------------------
  # convert list of table_body into list of gtsummary objects
  tbl_list <- x$table_body %>%
    dplyr::left_join(
      dplyr::tibble(variable = variables, ..group.. = variables),
      by = "variable"
    ) |>
    tidyr::fill("..group..", .direction = "up") |>
    tidyr::nest(data = -"..group..") |>
    dplyr::pull("data") |>
    map(
      ~ list(.) |>
        set_names("table_body") |>
        c(utils::modifyList(x, val = list(table_body = NULL))) %>% # add the other parts of the gtsummary table
        `class<-`(class(x)) # add original class from `x`
    )

  # caption/footnotes handling -----------------------------------------------
  if (length(tbl_list) > 1) {
    tbl_list <- .modify_footnotes_caption(tbl_list, footnotes, caption)
  }

  # return list of tbls --------------------------------------------------------
  # assign class (can't assign gtsummary because of print definitions)
  tbl_list |>
    structure(class = c("tbl_split", "list"))
}

#' @export
#' @rdname tbl_split_by
tbl_split_by_columns <- function(x, keys, groups,
                                 footnotes = c("all", "first", "last"),
                                 caption = c("all", "first")) {
  set_cli_abort_call()

  # list map -------------------------------------------------------------------
  if (inherits(x, "list")) {
    keys <- maybe_missing(keys, default = first_column(x[[1]])) # refers only to first? better handling?
    tbl_list <- map(
      .x = x,
      .f = tbl_split_by_columns,
      keys = {{ keys }},
      groups = groups
    )
    return(
      tbl_list |>
        unlist(recursive = FALSE) |>
        structure(class = c("tbl_split", "list"))
    )
  }

  # check inputs ---------------------------------------------------------------
  check_class(x, "gtsummary")
  keys <- maybe_missing(keys, default = first_column(x))
  cards::process_selectors(x$table_body, keys = {{ keys }})
  check_class(groups, "list")
  cards::check_list_elements(
    groups,
    predicate = is.character,
    error_msg = "Each element of the {.arg groups} argument's list must be a {.cls character} vector."
  )

  # check all variables in groups appear in the table
  columns_do_not_exist <-
    unlist(groups) |>
    setdiff(x$table_styling$header$column)
  if (!is_empty(columns_do_not_exist)) {
    cli::cli_abort()
  }

  # check that every un-hidden column is present
  missing_cols <-
    x$table_styling$header[!x$table_styling$header$hide, ][["column"]] |>
    setdiff(c(keys, unlist(groups)))
  if (!is_empty(missing_cols)) {
    cli::cli_inform(
      c("The following columns were not listed in either {.arg keys} or {.arg groups} argument: {.val {missing_cols}}",
        "i" = "These columns have been added to the end of {.arg groups}.",
        "*" = "Run {.fun gtsummary::show_header_names} for a list of all column names."
      )
    )
    groups <- c(groups, list(missing_cols))
  }

  # footnotes and caption check
  footnotes <- rlang::arg_match(footnotes)
  caption <- rlang::arg_match(caption)

  # splitting table ------------------------------------------------------------
  tbl_list <- vector(mode = "list", length = length(groups))
  for (i in seq_along(groups)) {
    tbl_list[[i]] <- gtsummary::modify_column_hide(x, columns = -all_of(union(keys, groups[[i]])))
  }

  # caption/footnotes handling -----------------------------------------------
  if (length(tbl_list) > 1) {
    tbl_list <- .modify_footnotes_caption(tbl_list, footnotes, caption)
  }

  # return list of tbls --------------------------------------------------------
  tbl_list |>
    structure(class = c("tbl_split", "list"))
}

# helper function for handling caption/footnotes
.modify_footnotes_caption <- function(tbl_list, footnotes, caption) {
  which_to_modify <- if (footnotes == "first") {
    seq(2, length(tbl_list))
  } else if (footnotes == "last") {
    seq(1, length(tbl_list) - 1)
  } else {
    NULL
  }
  if (!is.null(which_to_modify)) {
    for (i in which_to_modify) {
      tbl_list[[i]] <- tbl_list[[i]] |>
        remove_source_note() |>
        remove_abbreviation()
    }
  }
  if (caption == "first") {
    # Changing the call_list does not affect the output, should it be trimmed?
    for (i in seq(2, length(tbl_list))) {
      tbl_list[[i]]$table_styling$caption <- NULL
    }
  }
  tbl_list
}

#' @export
#' @rdname tbl_split_by
first_column <- function(x) {
  set_cli_abort_call()
  check_class(x, "gtsummary")

  x$table_styling$header[!x$table_styling$header$hide, ][["column"]][1]
}

#' @export
#' @rdname tbl_split_by
print.tbl_split <- function(x, ...) {
  check_dots_empty()
  walk(x, print)
}
