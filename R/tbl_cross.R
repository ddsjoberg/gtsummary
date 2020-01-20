#' Create a cross table of summary statistics
#'
#' Wrapper for `tbl_summary` to create cross tables
#'
#' @param x Object with class `tbl_summary` from the [tbl_summary] function
#' @param last Logical indicator to display overall column last in table.
#' Default is `FALSE`, which will display overall column first.
#' @family tbl_summary tools
#' @author Daniel D. Sjoberg
#' @export
#' @return A `tbl_summary` object
#' @examples
#' tbl_overall_ex <-
#'   trial[c("age", "response", "grade", "trt")] %>%
#'   tbl_summary(by = trt) %>%
#'   add_overall()
#' @section Example Output:
#' \if{html}{\figure{tbl_overall_ex.png}{options: width=50\%}}
#'
#'
tbl_cross <- function(data,
                      row = NULL,
                      col = NULL,
                      statistic = "{n}",
                      label = NULL,
                      missing = c("ifany", "always", "no"),
                      percent = c("none", "column", "row", "cell")) {


  row <- var_input_to_string(data = data,
                             select_input = !!rlang::enquo(row),
                             arg_name = "row",
                             select_single = TRUE)

  col <- var_input_to_string(data = data,
                             select_input = !!rlang::enquo(col),
                             arg_name = "col",
                             select_single = TRUE)

  # matching arguments ---------------------------------------------------------
  missing <- match.arg(missing)
  percent <- match.arg(percent)

  # if no x and y provided, default to first two columns of data ---------------
  if(is.null(row) & is.null(col)) {
    row <- names(data[, 1])
    col <- names(data[, 2])
  }

  if(is.null(row)) row <- names(data[, 1])
  if(is.null(col)) col <- names(data[, 1])

  data <- data %>%
    dplyr::select(row, col)

  # if any labels, grab them
  labels <- map(data, ~attr(.x,"label"))

  data <- data %>%
    set_names("row", "col")

  # omit or factorize NAs   ---------------

  if (missing == "no") {

    if(sum(complete.cases(data) == FALSE) > 0) {
      message(glue::glue(
        "{sum(complete.cases(data) == FALSE)} observations with missing data have been removed. "))
    }

    data <- data %>%
      na.omit()

  } else if (missing == "ifany") {

    data <- data %>%
      mutate_all(~fct_explicit_na(.x, "Unknown"))

  } else if (missing == "always") {

    data <- data %>%
      mutate_all( ~case_when(
        is.na(.x) ~ "Unknown",
        TRUE ~ as.character(.x)
      )) %>%
      mutate_all( ~fct_expand(.x, "Unknown"))


  }

  # create tbl_summary() ---------------

  # add column to compute totals
  data <- data %>%
    mutate(Total = 1)

  # get label of by variable for use in tab_spanner
  col_label <- labels[[col]]
  if(is.null(col_label)) col_label <- col


  x <- data %>%
    tbl_summary(by = col,
                statistic = list(row = statistic,
                                 Total = statistic),
                type = list(row = "categorical",
                            Total = "dichotomous"),
                missing = missing,
                label = list(
                  row = labels[[row]],
                  Total = "Total"
                )) %>%
    add_overall(last = TRUE) %>%
    bold_labels() %>%
    modify_header(stat_by = "{level}",
                  stat_0 = " ")


  # x %>%
  #   as_gt() %>%
  #   gt::tab_spanner(label = gt::md(glue("**{labels[[col]]}**")),
  #                   columns = contains("stat_"))  %>%
  #   gt::tab_spanner(., label = gt::md("**Total**"),
  #                   columns = vars(stat_0))

  # gt function calls ------------------------------------------------------------
  # quoting returns an expression to be evaluated later
  x$gt_calls[["cross_tab_spanner"]] <-
    glue("gt::tab_spanner(.,",
         "label = gt::md('**{labels[[col]]}**'), ",
         "columns = contains('stat_'))"
    )

  x$gt_calls[["cross_tab_spanner_total"]] <-
    glue("gt::tab_spanner(., label = gt::md('**Total**'), ",
         "columns = vars(stat_0))"
    )

  return(x)

}

#r <- tbl_cross(trial, row = response, col = trt, missing = "always")
r <- tbl_cross(trial, row = grade, col = trt, missing = "always")
# r$gt_calls
r

