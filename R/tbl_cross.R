
#' Create a cross table of summary statistics
#'
#' Wrapper for [tbl_summary] to cross tabulate two variables
#'
#' @param data A data frame
#' @param row A column name (quoted or unquoted) in data to be used for columns
#' of cross table
#' @param col A column name (quoted or unquoted) in data to be used for rows
#' of cross table
#' @param statistic A statistic name in curly brackets to
#' be replaced with the numeric statistic (see glue::glue).
#' The default is `{n}`.
#' @param label List of formulas specifying row and column variables labels,
#' e.g. `list(age ~ "Age, yrs", stage ~ "Path T Stage")`. If a
#' variable's label is not specified here, the label attribute
#' (`attr(data$age, "label")`) is used.  If
#' attribute label is `NULL`, the variable name will be used.
#' @param missing Indicates whether to include counts of `NA` values in the table.
#' Allowed values are `"no"` (never display NA values),
#' `"ifany"` (only display if any NA values), and `"always"`
#' (includes NA count row for all variables). Default is `"ifany"`.
#' @param percent Indicates the type of percentage to return if using the.
#' `{p}` statistic Must be one of
#' `"column"`, `"row"`, or `"cell"`. Default is `"column"`.
#' @family tbl_summary tools
#' @author Karissa Whiting
#' @export
#' @return A `tbl_summary` object
#' @examples
#' tbl_summary_ex1 <-
#'   trial[c("response", "grade)] %>%
#'   tbl_cross()
#'
#' @section Example Output:
#' \if{html}{\figure{tbl_overall_ex.png}{options: width=50\%}}
#'
#'
tbl_cross <- function(data,
                      row = NULL,
                      col = NULL,
                      percent = c(NULL, "column", "row", "cell"),
                      statistic = NULL,
                      label = NULL,
                      missing = c("ifany", "always", "no"),
                      missing_text = "Unknown") {

  row <- var_input_to_string(
    data = data,
    select_input = !!rlang::enquo(row),
    arg_name = "row",
    select_single = TRUE
  )

  col <- var_input_to_string(
    data = data,
    select_input = !!rlang::enquo(col),
    arg_name = "col",
    select_single = TRUE
  )

  # matching arguments ---------------------------------------------------------
  missing <- match.arg(missing)
  percent <- match.arg(percent)


  # if no x and y provided, default to first two columns of data ---------------
  if (is.null(row) & is.null(col)) {
    row <- names(data[, 1])
    col <- names(data[, 2])
  }

  row <- row %||% names(select(data, -col))[1]
  col <- col %||% names(select(data, -row))[1]

  # create column for tabulating column total
  data <- data %>%
    dplyr::select(row, col) %>%
    mutate(Total = 1)

  # get column label and user-passed labels ------------------------------------
  label <- tidyselect_to_list(data, label) %||% NULL
  col_label <- label[[col]] %||% attr(data[[col]], "label") %||% col

  # statistic argument ---------------------------------------------------------

  # if no user-defined stat, default to {n} if percent is none
  statistic <- statistic %||% ifelse(percent == "none", "{n}", "{n} ({p}%)")
  statistic_list <- rep(statistic, 3) %>% as.list()
  names(statistic_list) <- names(data)


  # omit missing data, or factorize missing level ------------------------------
  lbls <- purrr::map(data, ~ attr(.x, "label"))

  data <- data %>%
    mutate_at(vars(row, col), ~as.factor(.x)) %>%
    mutate_at(vars(row,col),
              ~switch(
                missing,
                "no" = .,
                "ifany" = forcats::fct_explicit_na(., missing_text),
                "always" = forcats::fct_explicit_na(., missing_text) %>%
                  forcats::fct_expand(missing_text)))

  # re-applying labels
  for (i in names(lbls)) {
    attr(data[[i]], "label") <- lbls[[i]] }

  x <- data %>%
    tbl_summary(
      by = col,
      statistic = statistic_list,
      percent = percent,
      label = label,
      missing_text = missing_text) %>%
    add_overall(last = TRUE) %>%
    bold_labels() %>%
    modify_header(
      stat_by = "{level}",
      stat_0 = " ")

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

  return(x)

}

