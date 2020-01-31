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
                      percent = c("none", "column", "row", "cell"),
                      statistic = ifelse(percent == "none", "{n}", "{n} ({p}%)"),
                      label = NULL,
                      missing = c("ifany", "always", "no"),
                      ) {
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

  if (is.null(row)) row <- names(data[, 1])
  if (is.null(col)) col <- names(data[, 1])

  data <- data %>%
    dplyr::select(row, col)

  # set labels -----------------------------------------------------------------

  # check for labels
  lbls <- map(data, ~ attr(.x, "label"))

  # overide labels if label argument exists
  if (!is.null(label)) {
    lbl_replace <- tidyselect_to_list(data,
      label,
      arg_name = "label"
    )

    missing_lbls <- setdiff(names(lbls), names(lbl_replace))
    lbls <- c(lbl_replace, lbls[missing_lbls])
  }

  # if no labels and no label arg, use column names (needed for col tab_spanner)
  lbls <- imap(lbls, ~ ifelse(is.null(.x), .y, .x))

  data <- data %>%
    set_names("row", "col")

  # omit missing data, or factorize Unknown level ------------------------------
  if (missing == "no") {
    if (sum(is.na(data$row)) > 0) {
      message(glue::glue(
        "{sum(is.na(data$row))} observations missing `{row}` have been removed."
      ))
    }

    if (sum(is.na(data$col)) > 0) {
      message(glue::glue(
        "{sum(is.na(data$col))} observations missing `{col}` have been removed."
      ))
    }
    data <- data %>%
      na.omit()

  } else if (missing == "ifany") {
    data <- data %>%
      mutate(col = forcats::fct_explicit_na(col, "Unknown"))
  } else if (missing == "always") {
    data <- data %>%
      mutate(col = case_when(
        is.na(col) ~ "Unknown",
        TRUE ~ as.character(col)
      )) %>%
      mutate(col = forcats::fct_expand(col, "Unknown"))
  }

  # create tbl_summary() ---------------

  # add column to compute totals
  data <- data %>%
    mutate(Total = 1)

  # get label of by variable for use in tab_spanner
  # col_label <- labels[[col]]
  # if (is.null(col_label)) col_label <- col


  x <- data %>%
    tbl_summary(
      by = col,
      statistic = list(
        row = statistic,
        Total = statistic
      ),
      type = list(
        row = "categorical",
        Total = "dichotomous"
      ),
      missing = missing,
      label = list(
        row = labels[[row]],
        Total = "Total"
      )
    ) %>%
    add_overall(last = TRUE) %>%
    bold_labels() %>%
    modify_header(
      stat_by = "{level}",
      stat_0 = " "
    )


  x$table_body <- x$table_body %>%
    filter(!(variable == "Total" & label == "Unknown"))


  # gt function calls ------------------------------------------------------------
  # quoting returns an expression to be evaluated later
  x$gt_calls[["tab_spanner"]] <-
    glue(
      "gt::tab_spanner(",
      "label = gt::md('**{labels[[col]]}**'), ",
      "columns = contains('stat_')) %>%",
      "gt::tab_spanner(label = gt::md('**Total**'), ",
      "columns = vars(stat_0))"
    )

  return(x)

}

attr(starwars, "label")
r <- tbl_cross(starwars, row = gender, col = hair_color, missing = "always")
r

tbl_cross(trial,
  row = response, col = trt,
  label = list(trt = "XX"),
  missing = "ifany"
)
r

r <- tbl_cross(trial, row = response, col = trt, missing = "no")
r

r <- tbl_cross(trial, row = grade, col = trt, missing = "no")
# r$gt_calls
r

# Iris
iris[1, "Species"] <- NA_character_

r <- tbl_cross(iris, row = Species, col = Species, missing = "always")
r

r <- tbl_cross(trial, row = response, col = trt, missing = "ifany")
r

r <- tbl_cross(trial, row = response, col = trt, missing = "no")
r

r <- tbl_cross(trial, row = grade, col = trt, missing = "no")
# r$gt_calls
r
