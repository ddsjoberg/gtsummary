#' Modify Footnotes
#'
#' @inheritParams modify
#' @param footnote (`string`)\cr
#'   a string
#' @param columns ([`tidy-select`][dplyr::dplyr_tidy_select])\cr
#'   columns to add footnote.
#'
#'   For `modify_footnote_spanning_header()`, pass a single column name where
#'   the spanning header begins. If multiple column names are passed, only
#'   the first is used.
#' @param rows (predicate `expression`)\cr
#'   Predicate expression to select rows in `x$table_body`.
#'   Review [rows argument details][rows_argument].
#' @param replace (scalar `logical`)\cr
#'   Logical indicating whether to replace any existing footnotes in the specified
#'   location with the specified footnote, or whether the specified should
#'   be added to the existing footnote(s) in the header/cell. Default
#'   is to replace existing footnotes.
#' @param level (`integer`)\cr
#'   An integer specifying which level to place the spanning header footnote.
#'
#' @return Updated gtsummary object
#' @name modify_footnote2
#'
#' @examplesIf identical(Sys.getenv("NOT_CRAN"), "true") || identical(Sys.getenv("IN_PKGDOWN"), "true")
#' # Example 1 ----------------------------------
#' tbl <- trial |>
#'   tbl_summary(by = trt, include = c(age, grade), missing = "no") |>
#'   modify_footnote_header(
#'     footnote = "All but four subjects received both treatments in a crossover design",
#'     columns = all_stat_cols(),
#'     replace = FALSE
#'   ) |>
#'   modify_footnote_body(
#'     footnote = "Tumor grade was assessed _before_ treatment began",
#'     columns = "label",
#'     rows = variable == "grade" & row_type == "label"
#'   )
#' tbl
#'
#' # Example 2 ----------------------------------
#' # remove all footnotes
#' tbl |>
#'   remove_footnote_header(columns = all_stat_cols()) |>
#'   remove_footnote_body(columns = label, rows = variable == "grade" & row_type == "label")
NULL

#' @export
#' @rdname modify_footnote2
modify_footnote_header <- function(x, footnote, columns, replace = TRUE, text_interpret = c("md", "html")) {
  set_cli_abort_call()
  updated_call_list <- c(x$call_list, list(modify_footnote_header = match.call()))

  # check inputs ---------------------------------------------------------------
  check_class(x, "gtsummary")
  check_string(footnote)
  check_scalar_logical(replace)
  text_interpret <- arg_match(text_interpret, error_call = get_cli_abort_call())

  # process columns ------------------------------------------------------------
  cards::process_selectors(
    scope_header(x$table_body, x$table_styling$header),
    columns = {{ columns }}
  )

  # evaluate the strings with glue ---------------------------------------------
  lst_footnotes <- .evaluate_string_with_glue(x, rep_named(columns, list(footnote)))

  # add updates to `x$table_styling$footnote_header` ---------------------------
  x <-
    .modify_footnote_header(
      x,
      lst_footnotes = lst_footnotes,
      text_interpret = text_interpret,
      replace = replace,
      remove = FALSE
    )

  # update call list and return table ------------------------------------------
  x$call_list <- updated_call_list
  x
}

#' @export
#' @rdname modify_footnote2
modify_footnote_body <- function(x, footnote, columns, rows, replace = TRUE, text_interpret = c("md", "html")) {
  set_cli_abort_call()
  updated_call_list <- c(x$call_list, list(modify_footnote_body = match.call()))

  # check inputs ---------------------------------------------------------------
  check_class(x, "gtsummary")
  check_string(footnote)
  check_scalar_logical(replace)
  text_interpret <- arg_match(text_interpret, error_call = get_cli_abort_call())
  .check_rows_input(x, {{ rows }})

  # process columns ------------------------------------------------------------
  cards::process_selectors(
    scope_header(x$table_body, x$table_styling$header),
    columns = {{ columns }}
  )

  # evaluate the strings with glue ---------------------------------------------
  lst_footnotes <- .evaluate_string_with_glue(x, rep_named(columns, list(footnote)))

  # add updates to `x$table_styling$footnote_body` -----------------------------
  x <-
    .modify_footnote_body(
      x,
      lst_footnotes = lst_footnotes,
      rows = {{ rows }},
      text_interpret = text_interpret,
      replace = replace,
      remove = FALSE
    )

  # update call list and return table ------------------------------------------
  x$call_list <- updated_call_list
  x
}

#' @export
#' @rdname modify_footnote2
modify_footnote_spanning_header <- function(x, footnote, columns,
                                            level = 1L, replace = TRUE,
                                            text_interpret = c("md", "html")) {
  set_cli_abort_call()
  updated_call_list <- c(x$call_list, list(modify_footnote_body = match.call()))

  # check inputs ---------------------------------------------------------------
  check_class(x, "gtsummary")
  check_string(footnote)
  check_scalar_integerish(level)
  if (level < 1) {
    cli::cli_abort(
      "The {.arg level} argument must be a positive integer.",
      call = get_cli_abort_call()
    )
  }
  check_scalar_logical(replace)
  text_interpret <- arg_match(text_interpret, error_call = get_cli_abort_call())

  # process columns ------------------------------------------------------------
  cards::process_selectors(
    scope_header(x$table_body, x$table_styling$header),
    columns = {{ columns }}
  )
  if (!is_empty(columns)) columns <- columns[1]
  check_scalar(columns)

  # evaluate the strings with glue ---------------------------------------------
  lst_footnotes <- .evaluate_string_with_glue(x, list(footnote) |> stats::setNames(columns))

  # add updates to `x$table_styling$footnote_body` -----------------------------
  x <-
    .modify_footnote_spanning_header(
      x,
      lst_footnotes = lst_footnotes,
      level = level,
      text_interpret = text_interpret,
      replace = replace
    )

  # update call list and return table ------------------------------------------
  x$call_list <- updated_call_list
  x
}

#' @export
#' @rdname modify_footnote2
remove_footnote_header <- function(x, columns) {
  set_cli_abort_call()
  updated_call_list <- c(x$call_list, list(remove_footnote_header = match.call()))

  # check inputs ---------------------------------------------------------------
  check_class(x, "gtsummary")

  # process columns ------------------------------------------------------------
  cards::process_selectors(
    scope_header(x$table_body, x$table_styling$header),
    columns = {{ columns }}
  )

  # add updates to `x$table_styling$footnote_header` ---------------------------
  x <-
    .modify_footnote_header(
      x,
      lst_footnotes = rep_named(columns, list(NA_character_)),
      remove = TRUE
    )

  # update call list and return table ------------------------------------------
  x$call_list <- updated_call_list
  x
}

#' @export
#' @rdname modify_footnote2
remove_footnote_body <- function(x, columns, rows) {
  set_cli_abort_call()
  updated_call_list <- c(x$call_list, list(remove_footnote_body = match.call()))

  # check inputs ---------------------------------------------------------------
  check_class(x, "gtsummary")
  .check_rows_input(x, {{ rows }})

  # process columns ------------------------------------------------------------
  cards::process_selectors(
    scope_header(x$table_body, x$table_styling$header),
    columns = {{ columns }}
  )

  # add updates to `x$table_styling$footnote_body` -----------------------------
  x <-
    .modify_footnote_body(
      x,
      lst_footnotes = rep_named(columns, list(NA_character_)),
      rows = {{ rows }},
      remove = TRUE
    )

  # update call list and return table ------------------------------------------
  x$call_list <- updated_call_list
  x
}

#' @export
#' @rdname modify_footnote2
remove_footnote_spanning_header <- function(x, columns, level) {
  set_cli_abort_call()
  updated_call_list <- c(x$call_list, list(remove_footnote_body = match.call()))

  # check inputs ---------------------------------------------------------------
  check_class(x, "gtsummary")
  check_scalar_integerish(level)
  if (level < 1) {
    cli::cli_abort(
      "The {.arg level} argument must be a positive integer.",
      call = get_cli_abort_call()
    )
  }

  # process columns ------------------------------------------------------------
  cards::process_selectors(
    scope_header(x$table_body, x$table_styling$header),
    columns = {{ columns }}
  )
  if (!is_empty(columns)) columns <- columns[1]
  check_scalar(columns)

  # add updates to `x$table_styling$footnote_body` -----------------------------
  x <-
    .modify_footnote_spanning_header(
      x,
      lst_footnotes = list(NA_character_) |> stats::setNames(columns),
      level = level,
      remove = TRUE
    )

  # update call list and return table ------------------------------------------
  x$call_list <- updated_call_list
  x
}

# this checks the rows argument evaluates to a lgl in `x$table_body`
.check_rows_input <- function(x, rows) {
  rows <- enquo(rows)
  # check rows evaluates to a logical
  rows_eval_error <-
    tryCatch(
      eval_tidy(rows, data = x$table_body) %>%
        {!is.logical(.)}, # styler: off
      error = function(e) TRUE
    )

  if (rows_eval_error) {
    cli::cli_abort(
      "The {.arg rows} argument must be an expression that evaluates to a logical vector in {.code x$table_body}.",
      call = get_cli_abort_call()
    )
  }

  invisible()
}

# modify_footnote_*() for internal use (no checking of inputs) -----------------
.modify_footnote_header <- function(x, lst_footnotes, text_interpret = "md",
                                    replace = TRUE, remove = FALSE) {
  # add updates to `x$table_styling$footnote_header` ---------------------------
  x$table_styling$footnote_header <- x$table_styling$footnote_header |>
    dplyr::bind_rows(
      dplyr::tibble(
        column = names(lst_footnotes),
        footnote = unlist(lst_footnotes) |> unname(),
        text_interpret = paste0("gt::", text_interpret),
        replace = replace,
        remove = remove
      )
    )

  # return table ---------------------------------------------------------------
  x
}

.modify_footnote_body <- function(x, lst_footnotes, rows, text_interpret = "md",
                                  replace = TRUE, remove = FALSE) {
  # add updates to `x$table_styling$footnote_body` -----------------------------
  x$table_styling$footnote_body <- x$table_styling$footnote_body |>
    dplyr::bind_rows(
      dplyr::tibble(
        column = names(lst_footnotes),
        rows = list(enquo(rows)),
        footnote = unlist(lst_footnotes) |> unname(),
        text_interpret = paste0("gt::", text_interpret),
        replace = replace,
        remove = remove
      )
    )

  # return table ---------------------------------------------------------------
  x
}


.modify_footnote_spanning_header <- function(x, lst_footnotes, level = 1L,
                                             text_interpret = "md",
                                             replace = TRUE, remove = FALSE) {
  # add updates to `x$table_styling$footnote_spanning_header` ------------------
  x$table_styling$footnote_spanning_header <-
    x$table_styling$footnote_spanning_header |>
    dplyr::bind_rows(
      dplyr::tibble(
        column = names(lst_footnotes),
        footnote = unlist(lst_footnotes) |> unname(),
        level = as.integer(level),
        text_interpret = paste0("gt::", text_interpret),
        replace = replace,
        remove = remove
      )
    )

  # return table ---------------------------------------------------------------
  x
}
