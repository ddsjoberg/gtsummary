#' Create a cross table of summary statistics
#'
#' \Sexpr[results=rd, stage=render]{lifecycle::badge("experimental")}
#' Wrapper for [tbl_summary] to cross tabulate two variables
#'
#' @param data A data frame
#' @param row A column name (quoted or unquoted) in data to be used for columns
#' of cross table
#' @param col A column name (quoted or unquoted) in data to be used for rows
#' of cross table
#' @param statistic A statistic name in curly brackets to
#' be replaced with the numeric statistic (see glue::glue).
#' The default is `{n}`. If percent argument is `"column"`, `"row"`, or `"cell"`,
#' default is `{n} ({p}%)`.
#' @inheritParams tbl_summary
#'
#' @family `tbl_summary`
#' @author Karissa Whiting
#' @export
#' @return A `tbl_cross` object
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

  tbl_cross_inputs <- as.list(environment())

  # if no col AND no row provided, default to first two columns of data --------
  if (is.null(row) & is.null(col)) {
    row <- names(data[, 1])
    col <- names(data[, 2])
  }

  # if only one of col/row provided, error
  if(sum(is.null(row), is.null(col)) > 0) {
    stop("Please specify which columns to use for both `col` and `row` arguments",
         call. = FALSE)
  }

  # create new dummy col for tabulating column totals in cross table
  data <- data %>%
     mutate(..total.. = 1)

  # get labels --------------------------------------------
  label <- tidyselect_to_list(data, label)
  new_label <-  list()

  new_label[[row]] <- label[[row]] %||% attr(data[[row]], "label") %||% row
  new_label[[col]] <- label[[col]] %||% attr(data[[col]], "label") %||% col
  new_label[["..total.."]] <- "Total"

  # statistic argument ---------------------------------------------------------
  # if no user-defined stat, default to {n} if percent is "none"
  statistic <- statistic %||% ifelse(percent == "none", "{n}", "{n} ({p}%)")

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
    data <- stats::na.omit(data)

    message(glue(
      "{sum(is.na(data))} observations with missing data have been removed."
    ))
  }

  # create main table ----------------------------------------------------------
  x <- data %>%
    select(row, col, ..total..) %>%
    tbl_summary(
      by = col,
      statistic = stats::as.formula(glue::glue("everything() ~ '{statistic}'")),
      percent = switch(percent != "none", percent),
      label = new_label,
      missing_text = missing_text
    ) %>%
    add_overall(last = TRUE) %>%
    bold_labels() %>%
    modify_header(
      stat_by = "{level}",
      stat_0 = " "
    )

  # get text for gt source note
  source_note_text <- footnote_stat_label(x$meta_data)

  # clear existing tbl_summary footnote
  x$table_header$footnote <- list(NULL)
  x <- update_calls_from_table_header(x)

  # update inputs and call list in return
  x[["call_list"]] <- list(tbl_cross = match.call())
  x[["inputs"]] <- tbl_cross_inputs

  class(x) <- c("tbl_cross")

  # gt function calls ------------------------------------------------------------
  # quoting returns an expression to be evaluated later
  x$gt_calls[["tab_spanner"]] <-
    glue(
      "gt::tab_spanner(",
      "label = gt::md('**{new_label[[col]]}**'), ",
      "columns = contains('stat_')) %>%",
      "gt::tab_spanner(label = gt::md('**Total**'), ",
      "columns = vars(stat_0))"
    )

  x$gt_calls[["tab_source_note"]] <-
    glue(
      "gt::tab_source_note(source_note = '{source_note_text}') "
    )

  x

}

