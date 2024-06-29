#' Create footnotes for individual p-values
#'
#' \lifecycle{questioning}\cr
#' The usual presentation of footnotes for p-values on a gtsummary table is
#' to have a single footnote that lists all statistical tests that were used to
#' compute p-values on a given table. The `separate_p_footnotes()` function
#' separates aggregated p-value footnotes to individual footnotes that denote
#' the specific test used for each of the p-values.
#'
#' @param x (`tbl_summary`, `tbl_svysummary`)\cr
#'   Object with class `"tbl_summary"` or `"tbl_svysummary"`
#'
#' @export
#'
#' @examples
#' # Example 1 ----------------------------------
#' trial |>
#'   tbl_summary(by = trt, include = c(age, grade)) |>
#'   add_p() |>
#'   separate_p_footnotes()
separate_p_footnotes <- function(x) {
  set_cli_abort_call()

  # check inputs ---------------------------------------------------------------
  check_class(x, cls = c("tbl_summary", "tbl_svysummary"))

  # check that `add_p` or `add_difference` has been run (but not both)
  if (!any(c("add_p", "add_difference") %in% names(x$call_list)) ||
      all(c("add_p", "add_difference") %in% names(x$call_list))) {
    cli::cli_abort(
      "One (and only one) of {.fun add_p} and {.fun add_difference} needs to be run before {.fun separate_p_footnotes}.",
      call = get_cli_abort_call()
    )
  }

  # remove p-value column footnote ---------------------------------------------
  x <- modify_footnote(x, any_of(c("p.value", "estimate", "conf.low", "conf.high")) ~ NA_character_)

  # extract footnote next for each variable ------------------------------------
  calling_fun <- names(x$call_list) |> intersect(c("add_p", "add_difference"))
  lst_footnotes <-
    unique(x$table_body$variable) |>
    map(
      function(variable) {
        # if an ARD object, then return the method row
        if (inherits(x$cards[[calling_fun]][[variable]], "card")) {
          footnote_i <-
            x$cards[[calling_fun]][[variable]] |>
            dplyr::filter(.data$stat_name %in% "method") |>
            dplyr::pull("stat") |>
            unlist()
          return(footnote_i)
        }
        # otherwise, return the method column (broom::tidy-style results)
        x$cards[[calling_fun]][[variable]][["method"]]
      }
    ) |>
    set_names(unique(x$table_body$variable)) |>
    compact()


  # adding footnotes to cells in table -----------------------------------------
  not_hidden_columns <-
    x$table_styling$header |>
    dplyr::filter(.data$hide == FALSE) |>
    dplyr::pull("column") |>
    intersect(c("p.value", "estimate", "conf.low", "conf.high"))

  for (variable in names(lst_footnotes)) {
    for (column in not_hidden_columns) {
      # only add footnote to non-NA cells
      footnote_cell <-
        x$table_body |>
        dplyr::filter(.data$variable %in% .env$variable & .data$row_type %in% "label") |>
        dplyr::pull(.data[[column]])

      if (!is.na(footnote_cell)) {
        x <-
          modify_table_styling(
            x,
            columns = any_of(column),
            rows = !!expr(.data$variable %in% !!variable & .data$row_type %in% "label"),
            footnote = lst_footnotes[[variable]]
          )
      }
    }
  }

  # return final object --------------------------------------------------------
  x
}
