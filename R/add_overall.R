#' Add overall column
#'
#' Adds a column with overall summary statistics to tables
#' created by `tbl_summary()`, `tbl_svysummary()`, `tbl_continuous()` or
#' `tbl_custom_summary()`.
#'
#' @param x (`tbl_summary`, `tbl_svysummary`, `tbl_continuous`, `tbl_custom_summary`)\cr
#'   A stratified 'gtsummary' table
#' @param last (scalar `logical`)\cr
#'   Logical indicator to display overall column last in table.
#'   Default is `FALSE`, which will display overall column first.
#' @param col_label (`string`)\cr
#'   String indicating the column label. Default is `"**Overall**  \nN = {style_number(N)}"`
#' @param statistic ([`formula-list-selector`][syntax])\cr
#'   Override the statistic argument in initial `tbl_*` function
#'   call. Default is `NULL`.
#' @param digits ([`formula-list-selector`][syntax])\cr
#'   Override the digits argument in initial `tbl_*` function
#'   call. Default is `NULL`.
#' @inheritParams rlang::args_dots_empty
#'
#' @author Daniel D. Sjoberg
#' @name add_overall
#' @return A `gtsummary` of same class as `x`
#' @examples
#' # Example 1 ----------------------------------
#' trial |>
#'   tbl_summary(include = c(age, grade), by = trt) |>
#'   add_overall()
#'
#' # Example 2 ----------------------------------
#' trial |>
#'   tbl_summary(
#'     include = grade,
#'     by = trt,
#'     percent = "row",
#'     statistic = ~"{p}%",
#'     digits = ~1
#'   ) |>
#'   add_overall(
#'     last = TRUE,
#'     statistic = ~"{p}% (n={n})",
#'     digits = ~ c(1, 0)
#'   )
#'
#' # Example 3 ----------------------------------
#' trial |>
#'   tbl_continuous(
#'     variable = age,
#'     by = trt,
#'     include = grade
#'   ) |>
#'   add_overall(last = TRUE)
NULL

#' @rdname add_overall
#' @export
add_overall <- function(x, ...) {
  check_not_missing(x)
  check_class(x, "gtsummary")
  UseMethod("add_overall")
}

#' @rdname add_overall
#' @export
add_overall.tbl_summary <- function(x, last = FALSE, col_label = "**Overall**  \nN = {style_number(N)}",
                                    statistic = NULL, digits = NULL, ...) {
  set_cli_abort_call()
  check_dots_empty()

  # translating the col_label, if nothing passed by user
  if (missing(col_label)) {
    paste0("**", translate_string("Overall"), "**  \nN = {style_number(N)}")
  }

  add_overall_generic(
    x = x,
    last = last,
    col_label = col_label,
    statistic = statistic,
    digits = digits,
    call = c(x$call_list, list(add_overall = match.call())),
    calling_fun = names(x$call_list)[1]
  )
}

#' @rdname add_overall
#' @export
add_overall.tbl_continuous <- add_overall.tbl_summary

#' @rdname add_overall
#' @export
add_overall.tbl_svysummary <- add_overall.tbl_summary

#' @rdname add_overall
#' @export
add_overall.tbl_custom_summary <- add_overall.tbl_summary

#' @rdname add_overall
#' @export
add_overall.tbl_hierarchical <- add_overall.tbl_summary

#' @rdname add_overall
#' @export
add_overall.tbl_hierarchical_count <- function(x,
                                               last = FALSE,
                                               col_label = ifelse(rlang::is_empty(x$inputs$denominator),
                                                                  "**Overall**",
                                                                  "**Overall**  \nN = {style_number(N)}"),
                                               statistic = NULL,
                                               digits = NULL, ...) {

  add_overall.tbl_summary(x = x, last = last, col_label = col_label,
                          statistic = statistic, digits = digits, ...)
}

add_overall_generic <- function(x, last, col_label, statistic, digits, call, calling_fun) {
  check_scalar_logical(last)
  check_string(col_label, allow_empty = TRUE)

  # checking that input x has a by var
  if (is_empty(x$inputs[["by"]])) {
    cli::cli_abort(
      "Cannot run {.fun add_overall} when original table function is not statified with {.code {calling_fun}(by)}.",
      call = get_cli_abort_call()
    )
  }

  # save arguments to pass to original function without `by` stratified --------
  args_overall <- x$inputs |>
    utils::modifyList(list(by = NULL), keep.null = TRUE)

  # if overall row, already included in data -----------------------------------
  if (calling_fun == "tbl_custom_summary" && isTRUE(args_overall$overall_row)) {
    args_overall$overall_row <- FALSE
  }

  # update statistic/digit argument as needed ----------------------------------
  if (!is_empty(statistic)) {
    args_overall$statistic <- statistic
  }
  if (!is_empty(digits)) {
    args_overall$digits <- digits
  }

  # create overall table -------------------------------------------------------
  tbl_overall <- do.call(calling_fun, args_overall)

  # merging overall results
  x <- add_overall_merge(x, tbl_overall, last, col_label, calling_fun)

  x$call_list <- call
  x
}

add_overall_merge <- function(x, tbl_overall, last, col_label, calling_fun) {
  # checking the original tbl_summary and the added overall,
  # are the same before binding (excluding headers)
  if (!identical(
    select(x$table_body, c("row_type", "variable", "label")),
    select(tbl_overall$table_body, c("row_type", "variable", "label")) |> as_tibble()
  )) {
    cli::cli_abort(
      c(
        "An error occured in {.fun add_overall}, and the overall statistic cannot be added.",
        "Have variable labels changed since the original call to {.fun {calling_fun}}?"
      ),
      call = get_cli_abort_call()
    )
  }

  # adding the overall cards object to the output
  x[["cards"]][["add_overall"]] <- tbl_overall[["cards"]][[1]]

  # adding overall stat to the table_body data frame
  x$table_body <-
    dplyr::bind_cols(
      x$table_body,
      tbl_overall$table_body |> dplyr::select("stat_0")
    )

  # add the overall header row to the primary table
  x$table_styling$header <-
    dplyr::bind_rows(
      x$table_styling$header,
      tbl_overall$table_styling$header |>
        dplyr::filter(.data$column %in% "stat_0")
    )

  x$table_styling$header %>%
    dplyr::rows_update(
      tbl_overall$table_styling$header %>%
        dplyr::filter(.data$column %in% "stat_0"),
      by = "column"
    )

  if (last == FALSE) {
    x <- modify_table_body(x, dplyr::relocate, "stat_0", .before = "stat_1")
  }

  # updating table_style with footnote and column header
  x$table_styling$footnote_header <-
    dplyr::bind_rows(
      x$table_styling$footnote_header,
      tbl_overall$table_styling$footnote_header %>%
        dplyr::filter(.data$column %in% "stat_0")
    )
  x$table_styling$footnote_body <-
    dplyr::bind_rows(
      x$table_styling$footnote_body,
      tbl_overall$table_styling$footnote_body %>%
        dplyr::filter(.data$column %in% "stat_0")
    )

  # Add header to overall column
  x <- modify_header(x, stat_0 = col_label)

  x
}
