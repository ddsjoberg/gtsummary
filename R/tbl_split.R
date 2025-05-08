#' Split gtsummary table bz rows and/or columns
#'
#' `r lifecycle::badge('experimental')`\cr
#' The `tbl_split_by_rows` and `tbl_split_by_columns`` functions split a single
#' gtsummary table into multiple tables.
#' Both horizontal (column-wise, (that is, splits by columns in
#' `x$table_body`) and vertical (row-wise) splits are possible.
#' Updates to the print method are expected.
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
#' @param footnotes (`string`)\cr
#'   can be either `"first"`, `"each"`, or `"last"`, to locate global footnotes
#'   only on the first, in each, or in the last table, respectively. It defaults
#'   to `"each"`. Reference footnotes are always present wherever they appear.
#' @param caption (`string`)\cr
#'   similarly to footnotes, makes the titles, and subtitles appear only on the
#'   `"first"` or `"each"` table. It defaults to `"each"`.
#' @inheritParams rlang::args_dots_empty
#'
#' @return `tbl_split` object. They can also be identified further by subclass
#'   definitions `tbl_split_by_rows` and `tbl_split_by_columns`.
#'
#' @details
#' Run `gtsummary::show_header_names()` to print all column names to split by.
#'
#' @examples
#' # create standard table
#' tbl <- tbl_summary(trial, by = trt)
#'
#' # split by rows
#' tbl_list_rows <- tbl |>
#'   tbl_split_by_rows(variables = c(marker, grade))
#'
#' # print column names
#' gtsummary::show_header_names(tbl)
#'
#' # split by columns
#' tbl_list_cols <- tbl |>
#'   tbl_split_by_columns(groups = list("stat_1", "stat_2"))
#'
#' # take the list of splitted rows and split again by columns
#' tbl_list_rows_cols <- tbl_list_rows |>
#'   tbl_split_by_columns(groups = list("stat_1", "stat_2"))
#'
#' # Handle footnotes
#'
#' @name tbl_split_by
NULL

#' @export
#' @rdname tbl_split_by
tbl_split_by_rows <- function(x, variables, footnotes = "each", caption = "each") {
  set_cli_abort_call()

  # list map -------------------------------------------------------------------
  if (inherits(x, "list")) {
    tbl_list <- map(
      .x = x,
      .f = tbl_split_by_rows,
      variables = {{ variables }}
    )
  } else {
    # check inputs ---------------------------------------------------------------
    check_class(x, "gtsummary")

    # process inputs -------------------------------------------------------------
    cards::process_selectors(
      data = scope_table_body(x$table_body),
      variables = {{ variables }}
    )
    # adding last variable
    variables <- variables |> union(dplyr::last(x$table_body$variable))

    footnotes <- arg_match(footnotes, values = c("each", "first", "last"))
    # if (missing(footnotes)) {
    #   footnotes <- get_theme_element("tbl_summary-arg:footnotes", default = translate_string(footnotes)) # styler: off
    # }
    check_string(footnotes)

    # caption check
    caption <- arg_match(caption, values = c("each", "first"))
    check_string(caption)

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
  }

  # return list of tbls --------------------------------------------------------
  # assign class (can't assign gtsummary because of print definitions)
  tbl_list |>
    structure(class = c("tbl_split", "tbl_split_by_rows", "list"))
}

#' @export
#' @rdname tbl_split_by
tbl_split_by_columns <- function(x, keys, groups, footnotes = "each", caption = "each") {
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
  } else {
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

    # footnotes check
    footnotes <- arg_match(footnotes, values = c("each", "first", "last"))
    # if (missing(footnotes)) {
    #   footnotes <- get_theme_element("tbl_summary-arg:footnotes", default = translate_string(footnotes)) # styler: off
    # }
    check_string(footnotes)

    # caption check
    caption <- arg_match(caption, values = c("each", "first"))
    check_string(caption)

    # splitting table ------------------------------------------------------------
    tbl_list <- vector(mode = "list", length = length(groups))
    for (i in seq_along(groups)) {
      tbl_list[[i]] <- gtsummary::modify_column_hide(x, columns = -all_of(union(keys, groups[[i]])))
    }

    # caption/footnotes handling -----------------------------------------------
    if (length(tbl_list) > 1) {
      tbl_list <- .modify_footnotes_caption(tbl_list, footnotes, caption)
    }
  }

  # return list of tbls --------------------------------------------------------
  tbl_list |>
    structure(class = c("tbl_split", "tbl_split_by_columns", "list"))
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
      tbl_list[[i]] |>
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
