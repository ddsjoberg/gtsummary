#' Add overall column
#'
#' - [`add_overall.tbl_summary()`]
#'
#' @param x (`gtsummary`)\cr
#'   Object with class 'gtsummary'
#' @param ... Passed to other methods.
#' @keywords internal
#' @author Daniel D. Sjoberg
#' @export
#'
#' @seealso [`add_overall.tbl_summary()`]
add_overall <- function(x, ...) {
  check_not_missing(x)
  check_class(x, "gtsummary")
  UseMethod("add_overall")
}

#' Add overall column
#'
#' Adds a column with overall summary statistics to tables
#' created by `tbl_summary`, `tbl_svysummary`, `tbl_continuous` or
#' `tbl_custom_summary`.
#'
#' @param x (`tbl_summary`\`tbl_svysummary`\`tbl_continuous`\`tbl_custom_summary`)\cr
#'   A stratified 'gtsummary' table
#' @param last Logical indicator to display overall column last in table.
#' Default is `FALSE`, which will display overall column first.
#' @param col_label String indicating the column label. Default is `"**Overall**,  N = {N}"`
#' @param statistic Override the statistic argument in initial `tbl_*` function.
#' call. Default is `NULL`.
#' @param digits Override the digits argument in initial `tbl_*` function
#' call. Default is `NULL`.
#' @inheritParams rlang::args_dots_empty
#'
#' @author Daniel D. Sjoberg
#' @export
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
#' # TODO: Add this example after `tbl_continuous()`
#' # # Example 3 ----------------------------------
#' # tbl_overall_ex3 <-
#' #   trial %>%
#' #   tbl_continuous(
#' #     variable = age,
#' #     by = trt,
#' #     include = grade
#' #   ) %>%
#' #   add_overall(last = TRUE)
add_overall.tbl_summary <- function(x, last = FALSE, col_label = NULL,
                                    statistic = NULL, digits = NULL, ...) {
  set_cli_abort_call()
  check_dots_empty()

  add_overall_generic(
    x = x,
    last = last,
    col_label = col_label,
    statistic = statistic,
    digits = digits,
    call = c(x$call_list, list(add_overall = match.call())),
    calling_fun = "tbl_summary"
  )
}

add_overall_generic <- function(x, last, col_label, statistic, digits, call, calling_fun) {
  check_scalar_logical(last)
  check_string(col_label, allow_empty = TRUE)

  # checking that input x has a by var
  if (is.null(x$inputs[["by"]])) {
    cli::cli_abort(
      "Cannot run {.fun add_overall} when original table function is not statified with {.code {calling_fun}(by)}.",
      call = get_cli_abort_call()
    )
  }

  # save arguments to pass to original function without `by` stratified --------
  args_overall <- x$inputs |>
    utils::modifyList(list(by = NULL), keep.null = TRUE)

  # if overall row, already included in data -----------------------------------
  if (isTRUE(args_overall$overall_row)) {
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
    select(tbl_overall$table_body, c("row_type", "variable", "label")) |>  as_tibble()
  )) {
    cli::cli_abort(
      c("An error occured in {.fun add_overall}, and the overall statistic cannot be added.",
        "Have variable labels changed since the original call to {.fun {calling_fun}}?"),
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
  x$table_styling$footnote <-
    dplyr::bind_rows(
      x$table_styling$footnote,
      tbl_overall$table_styling$footnote %>%
        dplyr::filter(.data$column %in% "stat_0")
    )

  # use user-specified label
  if (!is_empty(col_label)) {
    x <- modify_header(x, stat_0 = col_label)
  }
  else {
    # if no header specified by user, removed bold marks from stat_0 header to match the others
    x$table_styling$header <-
      x$table_styling$header |>
      dplyr::mutate(
        label =
          ifelse(
            .data$column %in% "stat_0",
            paste0(
              "**", translate_text("Overall"), "**  \n",
              str_remove_all(.data$label, "\\*\\*")
            ),
            .data$label
          )
      )
  }


  x
}
