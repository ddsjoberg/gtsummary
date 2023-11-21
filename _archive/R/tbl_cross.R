#' Create a cross table of summary statistics
#'
#' The function creates a cross table of two categorical variables.
#'
#' @param data A data frame
#' @param row A column name in `data=` to be used for the rows of cross table.
#' @param col A column name in `data=` to be used for the columns of cross table.
#' @param statistic A string with the statistic name in curly brackets to
#' be replaced with the numeric statistic (see glue::glue).
#' The default is `{n}`. If percent argument is `"column"`, `"row"`, or `"cell"`,
#' default is `"{n} ({p}%)"`.
#' @param percent Indicates the type of percentage to return.
#' Must be one of "none", "column", "row", or "cell". Default is "cell" when
#' `{N}` or `{p}` is used in statistic.
#' @param margin Indicates which margins to add to the table.
#' Default is `c("row", "column")`. Use `margin  = NULL` to suppress both
#' row and column margins.
#' @param margin_text Text to display for margin totals. Default is `"Total"`
#' @param digits Specifies the number of decimal
#' places to round the summary statistics. By default integers are shown
#' to the zero decimal places, and percentages are formatted with `style_percent()`.
#' If you would like to modify either of these, pass a vector of integers
#' indicating the number of decimal places to round the statistics.
#' For example, if the
#' statistic being calculated is `"{n} ({p}%)"` and you want the percent rounded
#' to 2 decimal places use `digits = c(0, 2)`. User
#' may also pass a styling function: `digits = style_sigfig`
#' @inheritParams tbl_summary
#'
#' @family tbl_cross tools
#' @seealso Review [list, formula, and selector syntax][syntax] used throughout gtsummary
#' @author Karissa Whiting, Daniel D. Sjoberg
#' @export
#' @return A `tbl_cross` object
#' @examples
#' \donttest{
#' # Example 1 ----------------------------------
#' tbl_cross_ex1 <-
#'   trial %>%
#'   tbl_cross(row = trt, col = response) %>%
#'   bold_labels()
#'
#' # Example 2 ----------------------------------
#' tbl_cross_ex2 <-
#'   trial %>%
#'   tbl_cross(row = stage, col = trt, percent = "cell") %>%
#'   add_p() %>%
#'   bold_labels()
#' }
#' @section Example Output:
#' \if{html}{Example 1}
#'
#' \if{html}{\out{
#' `r man_create_image_tag(file = "tbl_cross_ex1.png", width = "50")`
#' }}
#'
#' \if{html}{Example 2}
#'
#' \if{html}{\out{
#' `r man_create_image_tag(file = "tbl_cross_ex2.png", width = "60")`
#' }}

tbl_cross <- function(data,
                      row = NULL,
                      col = NULL,
                      label = NULL,
                      statistic = NULL,
                      digits = NULL,
                      percent = c("none", "column", "row", "cell"),
                      margin = c("column", "row"),
                      missing = c("ifany", "always", "no"),
                      missing_text = "Unknown",
                      margin_text = "Total") {
  # checking data input --------------------------------------------------------
  if (!is.data.frame(data) || nrow(data) == 0 || ncol(data) < 2) {
    stop("`data=` argument must be a data frame with at least one row and two columns.",
      call. = FALSE
    )
  }

  # ungrouping data ------------------------------------------------------------
  data <- data %>% ungroup()

  # converting inputs to string ------------------------------------------------
  row <-
    .select_to_varnames(
      select = {{ row }},
      data = data,
      arg_name = "row",
      select_single = TRUE
    )

  col <-
    .select_to_varnames(
      select = {{ col }},
      data = data,
      arg_name = "col",
      select_single = TRUE
    )

  # matching arguments ---------------------------------------------------------
  missing <- match.arg(missing)
  percent <- match.arg(percent)
  if (!is.null(margin)) {
    margin <- match.arg(margin, several.ok = TRUE)
  }

  # if no col AND no row provided, default to first two columns of data --------
  if (is.null(row) && is.null(col)) {
    row <- names(data)[1]
    col <- names(data)[2]
  }

  # saving function inputs
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
    select(any_of(c(row, col)))

  # adding row total if requested
  if ("row" %in% margin) {
    data <- data %>%
      mutate(..total.. = 1)
  }

  # get labels -----------------------------------------------------------------
  label <-
    .formula_list_to_named_list(
      x = label,
      data = data,
      arg_name = "label",
      type_check = chuck(type_check, "is_string", "fn"),
      type_check_msg = chuck(type_check, "is_string", "msg")
    )
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
    mutate_at(vars(any_of(c(row, col))), as.factor) %>%
    mutate_at(
      vars(any_of(c(row, col))),
      ~ switch(missing,
        "no" = .,
        "ifany" = if (any(is.na(.))) forcats::fct_na_value_to_level(., level = missing_text) else .,
        "always" = forcats::fct_na_value_to_level(., level = missing_text) %>%
          forcats::fct_expand(missing_text)
      )
    )

  if (missing == "no") {
    n_missing <- !stats::complete.cases(data) %>% sum()
    data <- stats::na.omit(data)

    message(glue("{n_missing} observations with missing data have been removed."))
  }

  # create main table ----------------------------------------------------------
  x <-
    data %>%
    select(any_of(c(row, col, "..total.."))) %>%
    tbl_summary(
      by = any_of(col),
      statistic = ~ glue("{statistic}"),
      digits = switch(!is.null(digits),
        everything() ~ digits
      ),
      percent = ifelse(percent == "none", "cell", percent),
      label = new_label,
      missing_text = missing_text,
      type = list("categorical") %>% rlang::set_names(row)
    ) %>%
    modify_header(all_stat_cols(FALSE) ~ "{level}", label = "") %>%
    modify_footnote(everything() ~ NA_character_) %>%
    modify_spanning_header(all_stat_cols(FALSE) ~ new_label[[col]])


  # adding column margin
  if ("column" %in% margin) {
    x <-
      add_overall(x, last = TRUE) %>%
      modify_header(update = list(stat_0 ~ margin_text)) %>%
      modify_footnote(stat_0 ~ NA_character_)
  }

  # returning results ----------------------------------------------------------
  # update inputs and call list in return
  x[["call_list"]] <- list(tbl_cross = match.call())
  x[["inputs"]] <- tbl_cross_inputs
  x[["tbl_data"]] <- data # this is the data frame that was passed to `tbl_summary()`

  class(x) <- c("tbl_cross", "tbl_summary", "gtsummary")
  x
}
