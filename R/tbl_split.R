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
#' @param variables,row_numbers ([`tidy-select`][dplyr::dplyr_tidy_select] or `integer`)\cr
#'   variables or row numbers at which to split the gtsummary table rows (tables
#'   will be separated after each of these variables).
#' @param keys ([`tidy-select`][dplyr::dplyr_tidy_select])\cr
#'   columns to be repeated in each table split. It defaults to the first column
#'   if missing (usually label column).
#' @param groups (list of `character` vectors)\cr
#'   list of column names that appear in `x$table_body`.
#'   Each group of column names represent a different table in the output list.
#' @param footnotes,caption (`string`) `r lifecycle::badge('experimental')`\cr
#'   can be either `"first"`, `"all"`, or `"last"`, to locate global footnotes or
#'   caption only on the first, in each, or in the last table, respectively. It defaults
#'   to `"all"`. Reference footnotes are always present wherever they appear.
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
#' `row_numbers` indicates the row numbers at which to split the table. It means
#'   that the table will be split after each of these row numbers. If the last
#'   row is selected, the split will not happen as it is supposed to happen after
#'   the last row.
#'
#' @examplesIf identical(Sys.getenv("NOT_CRAN"), "true") || identical(Sys.getenv("IN_PKGDOWN"), "true")
#' # Example 1 ----------------------------------
#' # Split by rows
#' trial |>
#'   tbl_summary(by = trt) |>
#'   tbl_split_by_rows(variables = c(marker, grade)) |>
#'   dplyr::last() # Print only last table for simplicity
#'
#' # Example 2 ----------------------------------
#' # Split by rows with row numbers
#' trial |>
#'   tbl_summary(by = trt) |>
#'   tbl_split_by_rows(row_numbers = c(5, 7)) |>
#'   dplyr::last() # Print only last table for simplicity
#'
#' # Example 3 ----------------------------------
#' # Split by columns
#' trial |>
#'   tbl_summary(by = trt, include = c(death, ttdeath)) |>
#'   tbl_split_by_columns(groups = list("stat_1", "stat_2")) |>
#'   dplyr::last() # Print only last table for simplicity
#'
#' # Example 4 ----------------------------------
#' # Both row and column splitting
#' trial |>
#'   tbl_summary(by = trt) |>
#'   tbl_split_by_rows(variables = c(marker, grade)) |>
#'   tbl_split_by_columns(groups = list("stat_1", "stat_2")) |>
#'   dplyr::last() # Print only last table for simplicity
#'
#' # Example 5 ------------------------------
#' # Split by rows with footnotes and caption
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
#'   dplyr::nth(n = 2) # Print only one but not last table for simplicity
#'
#' @name tbl_split_by
NULL

#' @export
#' @rdname tbl_split_by
tbl_split_by_rows <- function(x,
                              variables = NULL,
                              row_numbers = NULL,
                              footnotes = c("all", "first", "last"),
                              caption = c("all", "first", "last")) {
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

  # if the table doesn't have a 'variable' column, user must specify `row_numbers`
  if (!"variable" %in% names(x$table_body) && is_empty(row_numbers)) {
    cli::cli_abort(
      "The {.arg row_numbers} argument must be specified when the passed table does
       not contain a {.val variable} column in {.code x$table_body}.",
      call = get_cli_abort_call()
    )
  }

  # process inputs -------------------------------------------------------------
  if ("variable" %in% names(x$table_body)) {
    cards::process_selectors(
      data = scope_table_body(x$table_body),
      variables = {{ variables }}
    )
  }

  # check that row_numbers is integerish and in bounds
  check_integerish(row_numbers, allow_empty = TRUE)

  # check that row_numbers are in bounds
  check_range(
    row_numbers,
    include_bounds = c(TRUE, TRUE),
    range = c(1L, nrow(x$table_body)),
    message = c("Argument {.arg row_numbers} is out of bounds.",
                i = "Must be between {.val {1}} and {.val {nrow(x$table_body)}}."
    ),
    envir = current_env(),
    allow_empty = TRUE
  )

  # only one of `variables` and `row_numbers` may be specified
  if (is_empty(variables) + is_empty(row_numbers) != 1L) {
    cli::cli_abort(
      "Please select only one and only one between {.arg row_numbers} and {.arg variables} arguments.",
      call = get_cli_abort_call()
    )
  }

  # adding last variable
  if (!is_empty(variables)) {
    variables <- variables |> union(dplyr::last(x$table_body$variable))
  }

  # footnotes and caption check
  footnotes <- arg_match(footnotes)
  caption <- arg_match(caption)

  # merging split points -------------------------------------------------------
  # convert list of table_body into list of gtsummary objects
  if (is_empty(row_numbers)) {
    # Create a new column to indicate split groups by variables
    tbl_body_with_groups <- x$table_body |>
      dplyr::left_join(
        dplyr::tibble(variable = variables, ..group.. = variables),
        by = "variable"
      )
  } else {
    # If last row is added as a split point nothing changes as split is after the last row
    row_numbers <- sort(unique(c(row_numbers, nrow(x$table_body))))

    # Create a new column to indicate split groups
    tbl_body_with_groups <- x$table_body |>
      dplyr::bind_cols(
        dplyr::tibble(
          row_number = seq_len(nrow(x$table_body)),
          ..group.. = dplyr::case_when(
            row_number %in% row_numbers ~ as.character(row_number),
            TRUE ~ NA_character_
          )
        )
      )
  }

  # Split the table body into groups and add decorations
  tbl_list <- tbl_body_with_groups |>
    tidyr::fill("..group..", .direction = "up") |>
    tidyr::nest(data = -"..group..") |>
    dplyr::pull("data") |>
    map(
      ~ list(.) |>
        set_names("table_body") |>
        c(utils::modifyList(x, val = list(table_body = NULL))) |> # add the other parts of the gtsummary table
        `class<-`(class(x)) # add original class from `x`
    )

  # caption/footnotes handling -----------------------------------------------
  if (length(tbl_list) > 1) {
    tbl_list <- .modify_split_footnotes_caption(tbl_list, footnotes, caption)
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
                                 caption = c("all", "first", "last")) {
  set_cli_abort_call()

  # list map -------------------------------------------------------------------
  if (inherits(x, "list")) {
    check_class(x[[1]], "gtsummary")
    keys <- maybe_missing(keys, default = .first_unhidden_column(x[[1]])) # refers only to first? better handling?
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
  keys <- maybe_missing(keys, default = .first_unhidden_column(x))
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
  footnotes <- arg_match(footnotes)
  caption <- arg_match(caption)

  # splitting table ------------------------------------------------------------
  tbl_list <- vector(mode = "list", length = length(groups))
  for (i in seq_along(groups)) {
    tbl_list[[i]] <- gtsummary::modify_column_hide(x, columns = -all_of(union(keys, groups[[i]])))
  }

  # caption/footnotes handling -----------------------------------------------
  if (length(tbl_list) > 1) {
    tbl_list <- .modify_split_footnotes_caption(tbl_list, footnotes, caption)
  }

  # return list of tbls --------------------------------------------------------
  tbl_list |>
    structure(class = c("tbl_split", "list"))
}

# helper function for handling caption/footnotes
.modify_split_footnotes_caption <- function(tbl_list, footnotes, caption) {
  # which splits to remove footnotes from
  which_footnotes_to_remove <- if (footnotes == "first") {
    seq(2, length(tbl_list))
  } else if (footnotes == "last") {
    seq(1, length(tbl_list) - 1)
  } else {
    NULL
  }

  # Remove footnotes from the specified splits
  if (!is.null(which_footnotes_to_remove)) {
    for (i in which_footnotes_to_remove) {
      tbl_list[[i]] <- tbl_list[[i]] |>
        remove_source_note() |>
        remove_abbreviation()
    }
  }

  # which splits to remove caption from
  which_caption_to_remove <- if (caption == "first") {
    seq(2, length(tbl_list))
  } else if (caption == "last") {
    seq(1, length(tbl_list) - 1)
  } else {
    NULL
  }

  # Remove caption from the specified splits
  if (!is.null(which_caption_to_remove)) {
    # Changing the call_list does not affect the output, should it be trimmed?
    for (i in which_caption_to_remove) {
      tbl_list[[i]]$table_styling$caption <- NULL
    }
  }

  tbl_list
}

#' @export
#' @rdname tbl_split_by
print.tbl_split <- function(x, ...) {
  check_dots_empty()
  walk(x, print)
}
