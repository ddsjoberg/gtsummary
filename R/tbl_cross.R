#' Create a cross table of summary statistics
#'
#' \Sexpr[results=rd, stage=render]{lifecycle::badge("experimental")}
#' The function creates a cross table of two categorical variables.
#'
#' @param data A data frame
#' @param row A column name in data to be used for columns
#' of cross table.
#' @param col A column name in data to be used for rows
#' of cross table.
#' @param statistic A string with the statistic name in curly brackets to
#' be replaced with the numeric statistic (see glue::glue).
#' The default is `{n}`. If percent argument is `"column"`, `"row"`, or `"cell"`,
#' default is `{n} ({p}%)`.
#' @param percent Indicates the type of percentage to return.
#' Must be one of "none", "column", "row", or "cell". Default is "cell" when
#' `{N}` or `{p}` is used in statistic.
#' @param margin_text Text to display for margin totals. Default is `"Total"`
#' @inheritParams tbl_summary
#'
#' @family tbl_cross tools
#' @author Karissa Whiting, Daniel D. Sjoberg
#' @export
#' @return A `tbl_cross` object
#' @examples
#' tbl_cross_ex1 <-
#'   trial %>%
#'   tbl_cross(row = trt, col = response)
#'
#' tbl_cross_ex2 <-
#'   trial %>%
#'   tbl_cross(row = stage, col = trt) %>%
#'   add_p()
#'
#' @section Example Output:
#' \if{html}{Example 1}
#'
#' \if{html}{\figure{tbl_cross_ex1.png}{options: width=50\%}}
#'
#' \if{html}{Example 2}
#'
#' \if{html}{\figure{tbl_cross_ex2.png}{options: width=50\%}}

tbl_cross <- function(data,
                      row = NULL,
                      col = NULL,
                      label = NULL,
                      statistic = NULL,
                      percent = c("none", "column", "row", "cell"),
                      missing = c("ifany", "always", "no"),
                      missing_text = "Unknown",
                      margin_text = "Total") {

  # checking data input --------------------------------------------------------
  if (!is.data.frame(data) || nrow(data) == 0 || ncol(data) < 2) {
    stop("`data=` argument must be a data frame with at least one row and two columns.",
         call. = FALSE
    )
  }

  # converting inputs to string ------------------------------------------------
  row <- var_input_to_string(
    data = data, select_input = !!rlang::enquo(row),
    arg_name = "row", select_single = TRUE
  )

  col <- var_input_to_string(
    data = data, select_input = !!rlang::enquo(col),
    arg_name = "col", select_single = TRUE
  )

  # matching arguments ---------------------------------------------------------
  missing <- match.arg(missing)
  percent <- match.arg(percent)

  # if no col AND no row provided, default to first two columns of data --------
  if (is.null(row) && is.null(col)) {
    row <- names(data)[1]
    col <- names(data)[2]
  }

  # saving function intputs
  tbl_cross_inputs <- as.list(environment())

  # if only one of col/row provided, error
  if (sum(is.null(row), is.null(col)) == 1) {
    stop("Please specify which columns to use for both `col=` and `row=` arguments",
         call. = FALSE
    )
  }
  if ("..total.." %in% c(row, col)) {
    stop("Arguments `row=` and `col=` cannot be named '..total..'", call. = FALSE)
  }

  # create new dummy col for tabulating column totals in cross table
  data <- data %>%
    select(any_of(c(row, col))) %>%
    mutate(..total.. = 1)

  # get labels -----------------------------------------------------------------
  label <- tidyselect_to_list(data, label)
  new_label <- list()

  new_label[[row]] <- label[[row]] %||% attr(data[[row]], "label") %||% row
  new_label[[col]] <- label[[col]] %||% attr(data[[col]], "label") %||% col
  new_label[["..total.."]] <- margin_text

  # statistic argument ---------------------------------------------------------

  # if no user-defined stat, default to {n} if percent is "none"
  statistic <- statistic %||% ifelse(percent == "none", "{n}", "{n} ({p}%)")
  if (!rlang::is_string(statistic)) {
    stop("`statistic=` argument must be a string of length one.", call. = FALSE)
  }

  # omit missing data, or factorize missing level ------------------------------
  data <- data %>%
    mutate_at(vars(row, col), as.factor) %>%
    mutate_at(
      vars(row, col),
      ~ switch(
        missing,
        "no" = .,
        "ifany" = forcats::fct_explicit_na(., missing_text),
        "always" = forcats::fct_explicit_na(., missing_text) %>%
          forcats::fct_expand(missing_text)
      )
    )

  if (missing == "no") {
    n_missing <- !stats::complete.cases(data) %>% sum()
    data <- stats::na.omit(data)

    message(glue("{n_missing} observations with missing data have been removed."))
  }

  # create main table ----------------------------------------------------------
  x <- data %>%
    select(any_of(c(row, col, "..total.."))) %>%
    tbl_summary(
      by = col,
      statistic = stats::as.formula(glue("everything() ~ '{statistic}'")),
      percent = ifelse(percent == "none", "cell", percent),
      label = new_label,
      missing_text = missing_text
    ) %>%
    add_overall(last = TRUE) %>%
    bold_labels() %>%
    modify_header(
      stat_by = "{level}",
      stat_0 = paste0("**", margin_text, "**")
    )

  # clear all existing footnotes
  x$table_header$footnote <- NA

  # add spanning header
  x$table_header <-
    x$table_header %>%
    mutate(
      spanning_header = ifelse(startsWith(.data$column, "stat_") & .data$column != "stat_0",
                               paste0("**", new_label[[col]], "**"),
                               .data$spanning_header)
    )

  # update inputs and call list in return
  x[["call_list"]] <- list(tbl_cross = match.call())
  x[["inputs"]] <- tbl_cross_inputs

  class(x) <- c("tbl_cross", "tbl_summary", "gtsummary")

  # returning results ----------------------------------------------------------
  x
}
