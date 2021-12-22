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
add_overall <- function(x, ...) {
  UseMethod("add_overall")
}

#' @rdname add_overall
#' @export
add_overall.tbl_summary <- function(x, last = FALSE, col_label = NULL, ...) {
  updated_call_list <- c(x$call_list, list(add_overall = match.call()))
  # checking that input x has a by var
  if (is.null(x$inputs[["by"]])) {
    stop(
      "Cannot add Overall column when no 'by' variable in original tbl_summary"
    )
  }

  x_copy <- x

  # removing 'by' variable from data
  # (so it won't show up in the overall tbl_summary)
  x_copy$inputs[["data"]] <- select(x$inputs[["data"]], -x[["by"]])
  x_copy$inputs$include <- x_copy$inputs$include %>% setdiff(x[["by"]])

  # replacing the function call by variable to NULL to get results overall
  x_copy$inputs[["by"]] <- NULL

  # calculating stats overall, and adding header row
  tbl_overall <- do.call(tbl_summary, x_copy$inputs)

  # merging overall results
  x <- add_overall_merge(x, tbl_overall, last, col_label)

  x$call_list <- updated_call_list
  x
}


#' @rdname add_overall
#' @export
add_overall.tbl_svysummary <- function(x, last = FALSE, col_label = NULL) {
  updated_call_list <- c(x$call_list, list(add_overall = match.call()))
  # checking that input x has a by var
  if (is.null(x$inputs[["by"]])) {
    stop(
      "Cannot add Overall column when no 'by' variable in original tbl_svysummary"
    )
  }

  x_copy <- x

  # removing 'by' variable from data
  # (so it won't show up in the overall tbl_summary)
  x_copy$inputs$data$variables <- select(x$inputs$data$variables, -x$by)
  x_copy$inputs$include <- x_copy$inputs$include %>% setdiff(x[["by"]])

  # replacing the function call by variable to NULL to get results overall
  x_copy$inputs[["by"]] <- NULL

  # calculating stats overall, and adding header row
  tbl_overall <- do.call(tbl_svysummary, x_copy$inputs)

  # merging overall results
  x <- add_overall_merge(x, tbl_overall, last, col_label)

  x$call_list <- updated_call_list
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
