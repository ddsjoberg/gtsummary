#' Add column with overall summary statistics
#'
#' Adds a column with overall summary statistics to tables
#' created by `tbl_summary`, `tbl_svysummary`, `tbl_continuous` or
#' `tbl_custom_summary`.
#'
#' @param x Object with class `tbl_summary` from the [tbl_summary] function,
#' object with class `tbl_svysummary` from the [tbl_svysummary] function,
#' object with class `tbl_continuous` from the [tbl_continuous] function or
#' object with class `tbl_custom_summary` from the [tbl_custom_summary] function.
#' @param last Logical indicator to display overall column last in table.
#' Default is `FALSE`, which will display overall column first.
#' @param col_label String indicating the column label. Default is `"**Overall**,  N = {N}"`
#' @param statistic Override the statistic argument in initial `tbl_*` function.
#' call. Default is `NULL`.
#' @param digits Override the digits argument in initial `tbl_*` function
#' call. Default is `NULL`.
#' @param ... Not used
#' @family tbl_summary tools
#' @family tbl_svysummary tools
#' @family tbl_continuous tools
#' @family tbl_custom_summary tools
#' @author Daniel D. Sjoberg
#' @export
#' @return A `tbl_*` of same class as `x`
#' @examples
#' \donttest{
#' # Example 1 ----------------------------------
#' tbl_overall_ex1 <-
#'   trial %>%
#'   tbl_summary(include = c(age, grade), by = trt) %>%
#'   add_overall()
#'
#' # Example 2 ----------------------------------
#' tbl_overall_ex2 <-
#'   trial %>%
#'   tbl_summary(
#'     include = grade,
#'     by = trt,
#'     percent = "row",
#'     statistic = ~"{p}%",
#'     digits = ~1
#'   ) %>%
#'   add_overall(
#'     last = TRUE,
#'     statistic = ~"{p}% (n={n})",
#'     digits = ~ c(1, 0)
#'   )
#'
#' # Example 3 ----------------------------------
#' tbl_overall_ex3 <-
#'   trial %>%
#'   tbl_continuous(
#'     variable = age,
#'     by = trt,
#'     include = grade
#'   ) %>%
#'   add_overall(last = TRUE)
#' }
#' @section Example Output:
#' \if{html}{Example 1}
#'
#' \if{html}{\out{
#' `r man_create_image_tag(file = "tbl_overall_ex1.png", width = "55")`
#' }}
#'
#' \if{html}{Example 2}
#'
#' \if{html}{\out{
#' `r man_create_image_tag(file = "tbl_overall_ex2.png", width = "55")`
#' }}
#'
#' \if{html}{Example 3}
#'
#' \if{html}{\out{
#' `r man_create_image_tag(file = "tbl_overall_ex3.png", width = "55")`
#' }}
add_overall <- function(x, ...) {
  UseMethod("add_overall")
}

#' @rdname add_overall
#' @export
add_overall.tbl_summary <- function(x, last = FALSE, col_label = NULL,
                                    statistic = NULL, digits = NULL, ...) {
  check_dots_empty(error = function(e) inform(c(e$message, e$body)))
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
    paste(
      "Cannot add Overall column when no `by=` variable in",
      "original summary table call."
    ) %>%
      stop(call. = FALSE)
  }

  x_copy <- x

  # removing 'by' variable from data
  # (so it won't show up in the overall tbl_summary)
  # x_copy$inputs[["data"]] <-
  #   select(.extract_data_frame(x$inputs[["data"]]), -x[["by"]])
  x_copy$inputs$include <- x_copy$inputs$include %>% setdiff(x$inputs$by)

  # if overall row, already included in data -----------------------------------
  if (isTRUE(x$inputs$overall_row)) {
    x_copy$inputs$overall_row <- FALSE
  }

  # evaluate statistic and digits args -----------------------------------------
  statistic <-
    .formula_list_to_named_list(
      x = statistic,
      data = .extract_data_frame(x_copy$inputs$data),
      var_info = x_copy$table_body,
      arg_name = "statistic",
      type_check = chuck(type_check, "is_character", "fn"),
      type_check_msg = chuck(type_check, "is_character", "msg")
    )
  x_copy$inputs$statistic <-
    .formula_list_to_named_list(
      x = x_copy$inputs$statistic,
      data = .extract_data_frame(x_copy$inputs$data),
      var_info = x_copy$table_body,
      arg_name = "statistic"
    )
  digits <-
    .formula_list_to_named_list(
      x = digits,
      data = .extract_data_frame(x_copy$inputs$data),
      var_info = x_copy$table_body,
      arg_name = "digits",
      type_check = chuck(type_check, "digits", "fn"),
      type_check_msg = chuck(type_check, "digits", "msg")
    )
  x_copy$inputs$digits <-
    .formula_list_to_named_list(
      x = x_copy$inputs$digits,
      data = .extract_data_frame(x_copy$inputs$data),
      var_info = x_copy$table_body,
      arg_name = "digits"
    )
  # if user passed updates statistics or digits, update the calls
  if (!is.null(statistic)) {
    x_copy$inputs$statistic <-
      switch(is.null(x_copy$inputs$statistic),
             statistic
      ) %||%
      purrr::list_modify(x_copy$inputs$statistic, !!!statistic)
  }
  if (!is.null(digits)) {
    x_copy$inputs$digits <-
      switch(is.null(x_copy$inputs$digits),
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
          tbl_overall$meta_data$df_stats[[which(tbl_overall$meta_data$variable %in% .x)]]
        ) %>%
          purrr::imap_dfc(
            function(vec, colname) {
              attributes(vec) <-
                attributes(x$meta_data$df_stats[[which(x$meta_data$variable %in% .x)]][[colname]])
              vec
            }
          )
      }
    )

  # adding overall stat to the table_body data frame
  x <-
    x %>%
    modify_table_body(~ bind_cols(.x, overall %>% select(c("stat_0"))))

  # fill in the Ns in the header table modify_stat_* columns
  x$table_styling$header <-
    x$table_styling$header %>%
    dplyr::rows_update(
      tbl_overall$table_styling$header %>%
        dplyr::filter(.data$column %in% "stat_0"),
      by = "column"
    )
  x <- .fill_table_header_modify_stats(x)

  if (last == FALSE) {
    x <- x %>%
      modify_table_body(
        dplyr::relocate,
        "stat_0",
        .before = "stat_1"
      )
  }

  # updating table_style with footnote and column header
  x$table_styling$footnote <-
    dplyr::bind_rows(
      x$table_styling$footnote,
      tbl_overall$table_styling$footnote %>%
        dplyr::filter(.data$column %in% "stat_0")
    )

  # use defult header for new column
  if (is.null(col_label)) {
    x$table_styling$header <-
      x$table_styling$header %>%
      mutate(
        label =
          ifelse(
            .data$column %in% "stat_0",
            paste0(
              "**", translate_text("Overall"), "**, ",
              stringr::str_remove_all(.data$label, pattern = stringr::fixed("**"))
            ),
            .data$label
          )
      )
  }
  # use user-specified label
  else {
    x <- modify_header(x, stat_0 = col_label)
  }


  x
}
