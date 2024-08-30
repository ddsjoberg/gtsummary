#' Cross table
#'
#' The function creates a cross table of categorical variables.
#'
#' @param data (`data.frame`)\cr A data frame.
#' @param row ([`tidy-select`][dplyr::dplyr_tidy_select])\cr
#'   Column name in `data` to be used for the rows of cross table.
#'   Default is the first column in `data`.
#' @param col ([`tidy-select`][dplyr::dplyr_tidy_select])\cr
#'   Column name in `data` to be used for the columns of cross table.
#'   Default is the second column in `data`.
#' @param statistic (`string`)\cr
#'   A string with the statistic name in curly brackets to
#'   be replaced with the numeric statistic (see glue::glue).
#'   The default is `{n}`. If percent argument is `"column"`, `"row"`, or `"cell"`,
#'   default is `"{n} ({p}%)"`.
#' @param percent (`string`)\cr
#'   Indicates the type of percentage to return.
#'   Must be one of "none", "column", "row", or "cell". Default is "cell" when
#'   `{N}` or `{p}` is used in statistic.
#' @param margin (`character`)\cr
#'   Indicates which margins to add to the table.
#'   Default is `c("row", "column")`. Use `margin  = NULL` to suppress both
#'   row and column margins.
#' @param margin_text (`string`)\cr
#'   Text to display for margin totals. Default is `"Total"`
#' @param digits (`numeric`/`list`/`function`)\cr
#'   Specifies the number of decimal
#'   places to round the summary statistics.
#'   This argument is passed to `tbl_summary(digits = ~digits)`.
#'   By default integers are shown
#'   to the zero decimal places, and percentages are formatted with `style_percent()`.
#'   If you would like to modify either of these, pass a vector of integers
#'   indicating the number of decimal places to round the statistics.
#'   For example, if the
#'   statistic being calculated is `"{n} ({p}%)"` and you want the percent rounded
#'   to 2 decimal places use `digits = c(0, 2)`. User
#'   may also pass a styling function: `digits = style_sigfig`
#' @param missing (`string`)\cr
#'   Must be one of `c("ifany", "no", "always")`.
#'
#' @param missing_text (`string`)\cr
#'   String indicating text shown on missing row. Default is `"Unknown"`
#' @inheritParams tbl_summary
#'
#' @author Karissa Whiting, Daniel D. Sjoberg
#' @export
#'
#' @return A `tbl_cross` object
#' @examples
#' # Example 1 ----------------------------------
#' trial |>
#'   tbl_cross(row = trt, col = response) |>
#'   bold_labels()
#'
#' # Example 2 ----------------------------------
#' trial |>
#'   tbl_cross(row = stage, col = trt, percent = "cell") |>
#'   add_p() |>
#'   bold_labels()
tbl_cross <- function(data,
                      row = 1L,
                      col = 2L,
                      label = NULL,
                      statistic = ifelse(percent == "none", "{n}", "{n} ({p}%)"),
                      digits = NULL,
                      percent = c("none", "column", "row", "cell"),
                      margin = c("column", "row"),
                      missing = c("ifany", "always", "no"),
                      missing_text = "Unknown",
                      margin_text = "Total") {
  set_cli_abort_call()

  # check inputs ---------------------------------------------------------------
  check_not_missing(data)
  if (!missing(row) + !missing(col) == 1L) {
    cli::cli_abort(
      "Specify both {.arg row} and {.arg col} arguments or neither.",
      call = get_cli_abort_call()
    )
  }
  percent <- arg_match(percent)
  if (!is_empty(margin)) margin <- arg_match(margin, multiple = TRUE)
  missing <- arg_match(missing)
  check_string(missing_text)
  check_string(margin_text)
  check_string(statistic)

  # process inputs -------------------------------------------------------------
  cards::process_selectors(data, col = {{ col }}, row = {{ row }})
  check_scalar(
    col,
    allow_empty = TRUE,
    message = c("The {.arg {arg_name}} argument must be length {.val {1}}.",
                i = "Use {.fun tbl_strata} for more than one {.arg col} variable."
    )
  )
  check_scalar(row)
  if ("..total.." %in% c(row, col)) {
    cli::cli_abort(
      "Columns selected with {.arg row} and {.arg col} arguments cannot be named {.val ..total..}.",
      call = get_cli_abort_call()
    )
  }

  cards::process_formula_selectors(
    data = data[c(row, col)],
    label = label
  )

  # saving function inputs
  tbl_cross_inputs <- as.list(environment())

  # adding row total if requested
  if ("row" %in% margin) {
    data <- data |> mutate(..total.. = TRUE)
    attr(data$..total.., "label") <- margin_text
  }

  # updating label
  label <-
    map(
      intersect(c(row, col, "..total.."), names(data)),
      ~label[[.x]] %||% attr(data[[.x]], "label") %||% .x
    ) |>
    set_names(intersect(c(row, col, "..total.."), names(data)))

  # omit missing data, or factorize missing level ------------------------------
  data <- data |>
    dplyr::mutate(
      across(all_of(c(row, col)), as.factor),
      across(
        all_of(c(row, col)),
        ~ switch(
          missing,
          "no" = .,
          "ifany" =
            case_switch(
              any(is.na(.)) ~ fct_na_value_to_level(., level = missing_text),
              .default = .
            ),
          "always" =
            fct_na_value_to_level(., level = missing_text)
        )
      )
    )

  if (missing == "no") {
    n_missing <- !stats::complete.cases(data[c(row, col)]) %>% sum()
    data <- tidyr::drop_na(data, all_of(c(row, col)))
    if (n_missing > 0L)
      cli::cli_inform("{.val {n_missing}} observations with missing data have been removed.")
  }

  # create main table ----------------------------------------------------------
  x <-
    data |>
    tbl_summary(
      by = any_of(col),
      include = any_of(c(row, col, "..total..")),
      statistic = ~ statistic,
      digits =
        case_switch(
          !is_empty(digits) ~ (everything() ~ digits),
          .default = NULL
        ),
      percent = ifelse(percent == "none", "cell", percent),
      label = label,
      missing_text = missing_text,
      type = list(all_of(row) ~ "categorical", any_of("..total..") ~ "dichotomous")
    ) |>
    modify_header(all_stat_cols(FALSE) ~ "{level}", label = "") |>
    modify_footnote(everything() ~ NA_character_) |>
    modify_spanning_header(all_stat_cols(FALSE) ~ label[[col]])

  # adding column margin
  if ("column" %in% margin) {
    x <- x |>
      add_overall(last = TRUE) |>
      modify_header(stat_0 = margin_text) |>
      modify_footnote(stat_0 = NA_character_)
  }

  # returning results ----------------------------------------------------------
  # update inputs and call list in return
  x[["call_list"]] <- list(tbl_cross = match.call())
  x[["inputs"]] <- tbl_cross_inputs
  x[["tbl_data"]] <- data # this is the data frame that was passed to `tbl_summary()`

  class(x) <- c("tbl_cross", "tbl_summary", "gtsummary")
  x
}
