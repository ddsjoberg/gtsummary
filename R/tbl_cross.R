#' Create a cross table of summary statistics
#'
#' Wrapper for [tbl_summary] to cross tabulate two variables
#'
#' @param data A data frame
#' @param row A column name (quoted or unquoted) in data to be used for columns
#' of cross table
#' @param col A column name (quoted or unquoted) in data to be used for rows
#' of cross table
#' @param label List of formulas specifying row and column variables labels,
#' e.g. `list(response ~ "Drug Response", stage ~ "Path T Stage")`. If a
#' variable's label is not specified here, the label attribute
#' (`attr(data$age, "label")`) is used.  If
#' attribute label is `NULL`, the variable name will be used.
#' @param statistic A statistic name in curly brackets to
#' be replaced with the numeric statistic (see glue::glue).
#' The default is `{n}`. If percent argument is `"column"`, `"row"`, or `"cell"`,
#' default is `{n} ({p}%)`.
#' @param percent Indicates the type of percentage to return if using the.
#' `{p}` statistic. Must be one of
#' `"column"`, `"row"`, or `"cell"`. Default is `"column"` when `{p}` is
#' specified as `statistic`.
#' @param missing Indicates whether to include counts of `NA` values in the table.
#' Allowed values are `"no"` (never display NA values),
#' `"ifany"` (only display if any NA values), and `"always"`
#' (includes NA count row for all variables). Default is `"ifany"`.
#' @family tbl_summary tools
#' @author Karissa Whiting
#' @export
#' @return A `tbl_summary` object
#' @examples
#' tbl_cross_ex <-
#'   trial[c("response", "trt")] %>%
#'   tbl_cross(row = trt, col = response)
#'
#' @section Example Output:
#' \if{html}{\figure{tbl_cross_ex.png}{options: width=50\%}}
#'
tbl_cross <- function(data,
                      row = NULL,
                      col = NULL,
                      label = NULL,
                      statistic = NULL,
                      percent = c("none", "column", "row", "cell"),
                      missing = c("ifany", "always", "no"),
                      missing_text = "Unknown") {
  row_str <- var_input_to_string(
    data = data,
    select_input = !!rlang::enquo(row),
    arg_name = "row_str",
    select_single = TRUE
  )

  col_str <- var_input_to_string(
    data = data,
    select_input = !!rlang::enquo(col),
    arg_name = "col_str",
    select_single = TRUE
  )

  # matching arguments ---------------------------------------------------------
  missing <- match.arg(missing)
  percent <- match.arg(percent)


  # if no x and y provided, default to first two col_strumns of data -----------
  if (is.null(row_str) & is.null(col_str)) {
    row_str <- names(data[, 1])
    col_str <- names(data[, 2])
  }

  row_str <- row_str %||% names(select(data, -all_of(col_str)))[1]
  col_str <- col_str %||% names(select(data, -all_of(row_str)))[1]

  # create new dummy col for tabulating column totals in cross table
  data <- data %>%
    dplyr::select(all_of(c(row_str, col_str))) %>%
    mutate(Total = 1)

  # get col label (for tab spanner)--------------------------------------------
  label <- tidyselect_to_list(data, label) %||% NULL

  col_label <- label[[col_str]] %||%
    attr(data[[col_str]], "label") %||% col_str

  # statistic argument ---------------------------------------------------------

  # if no user-defined stat, default to {n} if percent is "none"
  statistic <- statistic %||% ifelse(percent == "none", "{n}", "{n} ({p}%)")
  statistic_list <- rep(statistic, 3) %>%
    as.list() %>%
    set_names(names(data))

  # omit missing data, or factorize missing level ------------------------------
  lbls <- purrr::map(data, ~ attr(.x, "label"))

  data <- data %>%
    mutate_at(vars(row_str, col_str), ~ as.factor(.x)) %>%
    mutate_at(
      vars(row_str, col_str),
      ~ switch(
        missing,
        "no" = .,
        "ifany" = forcats::fct_explicit_na(., missing_text),
        "always" = forcats::fct_explicit_na(., missing_text) %>%
          forcats::fct_expand(missing_text)
      )
    )

  if (missing == "no") {
    data <- na.omit(data)

    message(glue(
      "{sum(is.na(data))} observations missing data have been removed."
    ))
  }

  # re-applying labels
  for (i in names(lbls)) {
    attr(data[[i]], "label") <- lbls[[i]]
  }


  # create main table ----------------------------------------------------------
  x <- data %>%
    tbl_summary(
      by = col_str,
      statistic = statistic_list,
      percent = switch((percent == "none") + 1, percent, NULL),
      label = label,
      missing_text = missing_text
    ) %>%
    add_overall(last = TRUE) %>%
    bold_labels() %>%
    modify_header(
      stat_by = "{level}",
      stat_0 = " "
    )

  footnote_text <- footnote_stat_label(x$meta_data)

  # gt function calls ------------------------------------------------------------
  # quoting returns an expression to be evaluated later
  x$gt_calls[["tab_spanner"]] <-
    glue(
      "gt::tab_spanner(",
      "label = gt::md('**{col_label}**'), ",
      "columns = contains('stat_')) %>%",
      "gt::tab_spanner(label = gt::md('**Total**'), ",
      "columns = vars(stat_0))"
    )

  x$gt_calls[["tab_footnote"]] <-
    glue(
      "gt::tab_footnote(footnote = '{footnote_text}', ",
      "locations = ",
      "gt::cells_column_labels(columns = gt::vars(label)))"
    )

  return(x)
}


