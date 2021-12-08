#' Add column with overall summary statistics
#'
#' Adds a column with overall summary statistics to tables
#' created by `tbl_summary` or `tbl_svysummary`.
#'
#' @param x Object with class `tbl_summary` from the [tbl_summary] function or
#' object with class `tbl_svysummary` from the [tbl_svysummary] function.
#' @param last Logical indicator to display overall column last in table.
#' Default is `FALSE`, which will display overall column first.
#' @param col_label String indicating the column label. Default is `"**Overall**,  N = {N}"`
#' @param statistic Override the statistic argument in initial `tbl_summary()`
#' or `tbl_svysummary()` call. Default is `NULL`.
#' @param digits Override the digits argument in initial `tbl_summary()`
#' or `tbl_svysummary()` call. Default is `NULL`.
#' @family tbl_summary tools
#' @family tbl_svysummary tools
#' @author Daniel D. Sjoberg
#' @export
#' @return A `tbl_summary` object or a `tbl_svysummary` object
#' @examples
#' tbl_overall_ex <-
#'   trial[c("age", "grade", "trt")] %>%
#'   tbl_summary(by = trt) %>%
#'   add_overall()
#' @section Example Output:
#' \if{html}{\figure{tbl_overall_ex.png}{options: width=50\%}}
add_overall <- function(x, last, col_label, statistic, digits) {
  UseMethod("add_overall")
}

#' @rdname add_overall
#' @export
add_overall.tbl_summary <- function(x, last = FALSE, col_label = NULL,
                                    statistic = NULL, digits = NULL) {
  add_overall_generic(
    x = x, last = last, col_label = col_label,
    statistic = statistic, digits = digits,
    call = c(x$call_list, list(add_overall = match.call()))
  )
}


#' @rdname add_overall
#' @export
add_overall.tbl_svysummary <- add_overall.tbl_summary

#' @rdname add_overall
#' @export
add_overall.tbl_continuous <- add_overall.tbl_summary


#' @rdname add_overall
#' @export
add_overall.tbl_custom_summary <- add_overall.tbl_summary


add_overall_generic <- function(x, last, col_label, statistic, digits, call) {
  # checking that input x has a by var
  if (is.null(x$inputs[["by"]])) {
    paste("Cannot add Overall column when no `by=` variable in",
          "original summary table call.") %>%
      stop(call. = FALSE)
  }

  x_copy <- x

  # removing 'by' variable from data
  # (so it won't show up in the overall tbl_summary)
  # x_copy$inputs[["data"]] <-
  #   select(use_data_frame(x$inputs[["data"]]), -x[["by"]])
  x_copy$inputs$include <- x_copy$inputs$include %>% setdiff(x$inputs$by)

  # if overall row, already included in data -----------------------------------
  if (isTRUE(x$inputs$overall_row)) {
    x_copy$inputs$overall_row = FALSE
  }

  # evaluate statistic and digits args -----------------------------------------
  statistic <-
    .formula_list_to_named_list(
      x = statistic,
      data = use_data_frame(x_copy$inputs$data),
      var_info = x_copy$table_body,
      arg_name = "statistic"
    )
  x_copy$inputs$statistic <-
    .formula_list_to_named_list(
      x = x_copy$inputs$statistic,
      data = use_data_frame(x_copy$inputs$data),
      var_info = x_copy$table_body,
      arg_name = "statistic"
    )
  digits <-
    .formula_list_to_named_list(
      x = digits,
      data = use_data_frame(x_copy$inputs$data),
      var_info = x_copy$table_body,
      arg_name = "digits"
    )
  x_copy$inputs$digits <-
    .formula_list_to_named_list(
      x = x_copy$inputs$digits,
      data = use_data_frame(x_copy$inputs$data),
      var_info = x_copy$table_body,
      arg_name = "digits"
    )
  # if user passed updates statistics or digits, update the calls
  if (!is.null(statistic)) {
    x_copy$inputs$statistic <-
      switch(
        is.null(x_copy$inputs$statistic),
        statistic
      ) %||%
      purrr::list_modify(x_copy$inputs$statistic, !!!statistic)
  }
  if (!is.null(digits)) {
    x_copy$inputs$digits <-
      switch(
        is.null(x_copy$inputs$digits),
        digits
      ) %||%
      purrr::list_modify(x_copy$inputs$digits, !!!digits)
  }

  # replacing the function call by variable to NULL to get results overall
  x_copy$inputs[["by"]] <- NULL

  # calculating stats overall, and adding header row
  tbl_overall <- do.call(class(x)[1], x_copy$inputs)

  # merging overall results
  x <- add_overall_merge(x, tbl_overall, last, col_label)

  x$call_list <- call
  x
}


add_overall_merge <- function(x, tbl_overall, last, col_label) {
  # extracting table body from overall table
  overall <-
    tbl_overall %>%
    pluck("table_body")

  # checking the original tbl_summary and the added overall,
  # are the same before binding (excluding headers)
  if (!identical(
    select(x$table_body, c("row_type", "variable", "label")),
    select(overall, c("row_type", "variable", "label")) %>% as_tibble()
  )) {
    paste(
      "An error occured in `add_overall()`, and overall statistics cannot be merged.",
      "Has the variable label changed since the original call of `tbl_summary()`?"
    ) %>%
      stringr::str_wrap() %>%
      stop(call. = FALSE)
  }

  # adding the stat_0 row to to the df_stats tibbles
  x$meta_data$df_stats <-
    x$meta_data$variable %>%
    map(
      function(.x) {
        bind_rows(
          x$meta_data$df_stats[[which(x$meta_data$variable %in% .x)]],
          tbl_overall$meta_data$df_stats[[which(tbl_overall$meta_data$variable  %in% .x)]]
        ) %>%
          purrr::imap_dfc(
            function(vec, colname) {
              attributes(vec) <-
                attributes(x$meta_data$df_stats[[which(x$meta_data$variable  %in% .x)]][[colname]])
              vec
            }
          )
      }
    )

  # adding overall stat to the table_body data frame
  x$table_body <-
    bind_cols(
      x$table_body,
      overall %>% select(c("stat_0"))
    )

  if (last == FALSE) {
    x <- x %>%
      modify_table_body(
        dplyr::relocate,
        .data$stat_0,
        .before = .data$stat_1
      )
  }

  # updating table_style
  x <-
    modify_table_styling(
      x,
      columns = "stat_0",
      footnote = footnote_stat_label(x$meta_data),
      hide = FALSE
    ) %>%
    modify_header(
      stat_0 =
        col_label %||%
        paste0("**", translate_text("Overall"), "**, N = {style_number(N)}"),
    )

  x
}

