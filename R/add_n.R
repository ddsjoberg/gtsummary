#' Add column with N
#'
#' For each variable in a `tbl_summary` table, the `add_n` function adds a column with the
#' total number of non-missing (or missing) observations
#'
#' @param x Object with class `tbl_summary` from the [tbl_summary] function
#' @param statistic String indicating the statistic to report. Default is the
#' number of non-missing observation for each variable, `statistic = "{n}"`.
#' Other statistics available to report include:
#' * `"{N}"` total number of observations,
#' * `"{n}"` number of non-missing observations,
#' * `"{n_miss}"` number of missing observations,
#' * `"{p}"` percent non-missing data,
#' * `"{p_miss}"` percent missing data
#' The argument uses [glue::glue] syntax and multiple statistics may be reported,
#' e.g. `statistic = "{n} / {N} ({p}%)"`
#' @param col_label String indicating the column label.  Default is `"**N**"`
#' @param footnote Logical argument indicating whether to print a footnote
#' clarifying the statistics presented. Default is `FALSE`
#' @param last Logical indicator to include N column last in table.
#' Default is `FALSE`, which will display N column first.
#' @param missing DEPRECATED. Logical argument indicating whether to print N
#' (`missing = FALSE`), or N missing (`missing = TRUE`).  Default is `FALSE`
#' @family tbl_summary tools
#' @author Daniel D. Sjoberg
#' @export
#' @return A `tbl_summary` object
#' @examples
#' tbl_n_ex <-
#'   trial %>%
#'   dplyr::select(trt, age, grade, response) %>%
#'   tbl_summary(by = trt) %>%
#'   add_n()
#' @section Example Output:
#' \if{html}{\figure{tbl_n_ex.png}{options: width=50\%}}

add_n <- function(x, statistic = "{n}", col_label = "**N**", footnote = FALSE,
                  last = FALSE, missing = NULL) {
  # checking that input is class tbl_summary
  if (class(x) != "tbl_summary") stop("x must be class 'tbl_summary'")

  # defining function to round percentages -------------------------------------
  percent_fun <- getOption("gtsummary.tbl_summary.percent_fun",
                           default = style_percent)

  # DEPRECATED specifying statistic via missing argument -----------------------
  if (!is.null(missing)) {
    lifecycle::deprecate_warn("1.2.2",
                              "gtsummary::add_n(missing = )",
                              "gtsummary::add_n(statistic = )")
    if (identical(missing, TRUE)) {
      statistic = "{n_miss}"
      col_label = "**N Missing**"
    }
  }

  # counting non-missing N (or missing N) --------------------------------------
  counts <-
    x$meta_data %>%
    select(c("variable")) %>%
    mutate(
      row_type = "label",
      N = nrow(x$inputs$data),
      n = purrr::map_int(.data$variable, ~ sum(!is.na(x$inputs$data[[.x]]))),
      n_miss = purrr::map_int(.data$variable, ~ sum(is.na(x$inputs$data[[.x]]))),
      p = percent_fun(.data$n / .data$N),
      p_miss = percent_fun(.data$n_miss / .data$N),
      statistic = glue(statistic) %>% as.character()
    ) %>%
    select(.data$variable, .data$row_type, n = .data$statistic)

  # DEPRECATED specifying column name via `missing` argument -------------------
  if (identical(missing, TRUE)) {
    counts <- rename(counts, n_missing = .data$n)
  }

  # merging result with existing tbl_summary -----------------------------------
  if (last == FALSE) {
    table_body <-
      x$table_body %>%
      select(c("variable", "row_type", "label")) %>%
      left_join(counts, by = c("variable", "row_type")) %>%
      left_join(x$table_body, by = c("variable", "row_type", "label"))
  }
  else if (last == TRUE) {
    table_body <-
      x$table_body %>%
      left_join(counts, by = c("variable", "row_type"))
  }

  # replacing old table_body with new ------------------------------------------
  x$table_body <- table_body

  x$table_header <-
    tibble(column = names(table_body)) %>%
    left_join(x$table_header, by = "column") %>%
    table_header_fill_missing()

  # Adding footnote if requested -----------------------------------------------
  if (footnote == TRUE) {
    x$table_header <-
      x$table_header %>%
      mutate(
        footnote= map2(
          .data$column, .data$footnote,
          function(x1, y1) {
            if (x1 %in% c("n"))
              return(c(y1, paste("Statistics presented:", stat_to_label(statistic))))
            return(y1)
          }
        )
      )
  }

  # updating header ------------------------------------------------------------
  # DEPRECATED specifying column name via `missing` argument -------------------
  if (identical(missing, TRUE)) {
    x <- modify_header_internal(x, n_missing = col_label)
  }
  else {
    x <- modify_header_internal(x, n = col_label)
  }

  # updating gt and kable calls with data from table_header
  x <- update_calls_from_table_header(x)

  # adding indicator to output that add_n was run on this data
  x$call_list <- c(x$call_list, list(add_n = match.call()))

  # returning tbl_summary object
  return(x)
}

stat_to_label <- function(x) {
  x <- stringr::str_replace_all(x, fixed("{N}"), fixed("Total N"))
  x <- stringr::str_replace_all(x, fixed("{n}"), fixed("N non-Missing"))
  x <- stringr::str_replace_all(x, fixed("{n_miss}"), fixed("N Missing"))
  x <- stringr::str_replace_all(x, fixed("{p}%"), fixed("% non-Missing"))
  x <- stringr::str_replace_all(x, fixed("{p}"), fixed("% non-Missing"))
  x <- stringr::str_replace_all(x, fixed("{p_miss}%"), fixed("% Missing"))
  x <- stringr::str_replace_all(x, fixed("{p_miss}"), fixed("% Missing"))

  x
}
