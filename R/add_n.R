#' Adds column with N to gtsummary table
#'
#' @param x Object created from a gtsummary function
#' @param ... Additional arguments passed to other methods.
#' @author Daniel D. Sjoberg
#' @seealso [add_n.tbl_summary], [add_n.tbl_svysummary], [add_n.tbl_survfit]
#' @export
add_n <- function(x, ...) {
  UseMethod("add_n")
}


#' Add column with N
#'
#' For each variable in a `tbl_summary` table, the `add_n` function adds a column with the
#' total number of non-missing (or missing) observations
#'
#' @param x Object with class `tbl_summary` from the [tbl_summary] function or
#' with class `tbl_svysummary` from the [tbl_svysummary] function
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
#' @param ... Not used
#' @family tbl_summary tools
#' @family tbl_svysummary tools
#' @author Daniel D. Sjoberg
#' @export
#' @rdname add_n.tbl_summary
#' @return A `tbl_summary` or `tbl_svysummary` object
#' @examples
#' tbl_n_ex <-
#'   trial[c("trt", "age", "grade", "response")] %>%
#'   tbl_summary(by = trt) %>%
#'   add_n()
#' @section Example Output:
#' \if{html}{\figure{tbl_n_ex.png}{options: width=50\%}}

add_n.tbl_summary <- function(x, statistic = "{n}", col_label = "**N**", footnote = FALSE,
                              last = FALSE, missing = NULL, ...) {
  # checking that input is class tbl_summary
  if (!(inherits(x, "tbl_summary") | inherits(x, "tbl_svysummary")))
    stop("`x` must be class 'tbl_summary' or 'tbl_svysummary'")

  # defining function to round percentages -------------------------------------
  percent_fun <-
    get_theme_element("tbl_summary-fn:percent_fun") %||%
    getOption("gtsummary.tbl_summary.percent_fun", default = style_percent)
  N_fun <-
    get_theme_element("tbl_summary-fn:N_fun",
                      default = style_number)

  # DEPRECATED specifying statistic via missing argument -----------------------
  if (!is.null(missing)) {
    lifecycle::deprecate_warn(
      "1.2.2",
      "gtsummary::add_n(missing = )",
      "gtsummary::add_n(statistic = )"
    )
    if (identical(missing, TRUE)) {
      statistic <- "{n_miss}"
      col_label <- "**N Missing**"
    }
  }

  # counting non-missing N (or missing N) --------------------------------------
  # directly from x$meta_data$df_stats where it is already there
  variable_by_chr <- c("variable", switch(!is.null(x$by), "by"))
  counts <-
    map_dfr(
      x$meta_data$df_stats,
      ~select(.x, any_of(c("by", "variable", "N_miss", "N_obs",
                           "p_miss", "N_nonmiss", "p_nonmiss")))
    ) %>%
    dplyr::distinct_at(variable_by_chr, .keep_all = TRUE) %>%
    dplyr::group_by(.data$variable) %>%
    dplyr::summarise(
      row_type = "label",
      n = N_fun(sum(.data$N_nonmiss)),
      n_miss = N_fun(sum(.data$N_miss)),
      N = N_fun(sum(.data$N_nonmiss, .data$N_miss)),
      p = percent_fun(sum(.data$N_nonmiss) / sum(.data$N_nonmiss, .data$N_miss)),
      p_miss = percent_fun(sum(.data$N_miss) / sum(.data$N_nonmiss, .data$N_miss)),
      statistic = glue(statistic) %>% as.character(),
      .groups = "drop_last"
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
        footnote = ifelse(.data$column == "n",
                          paste("Statistics presented:", stat_to_label(statistic)),
                          .data$footnote)
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

#' @export
#' @rdname add_n.tbl_summary
add_n.tbl_svysummary <- add_n.tbl_summary

#' Add column with number of observations
#'
#' For each `survfit()` object summarized with `tbl_survfit()` this function
#' will add the total number of observations in a new column.
#'
#' @param x object of class "`tbl_survfit`"
#' @param ... Not used
#' @export
#' @family tbl_survfit tools
#' @examples
#' library(survival)
#' fit1 <- survfit(Surv(ttdeath, death) ~ 1, trial)
#' fit2 <- survfit(Surv(ttdeath, death) ~ trt, trial)
#'
#' # Example 1 ----------------------------------
#' add_n.tbl_survfit_ex1 <-
#'   list(fit1, fit2) %>%
#'   tbl_survfit(times = c(12, 24)) %>%
#'   add_n()
#' @section Example Output:
#' \if{html}{Example 1}
#'
#' \if{html}{\figure{add_n.tbl_survfit_ex1.png}{options: width=64\%}}

add_n.tbl_survfit <- function(x, ...) {

  # adding N to the table_body -------------------------------------------------
  x$table_body <-
    purrr::map2_dfr(
      x$meta_data$survfit,
      x$meta_data$variable,
      function(suvfit, variable) {
        #extracting survfit call
        survfit_call <- suvfit$call %>% as.list()
        # index of formula and data
        call_index <- names(survfit_call) %in% c("formula", "data") %>% which()

        # converting call into a survdiff call
        model.frame_call <- rlang::call2(rlang::expr(stats::model.frame), !!!survfit_call[call_index], ...)

        # returning number of rows in data frame
        tibble(
          variable = variable,
          row_type = "label",
          N = eval(model.frame_call) %>% nrow()
        )
      }
    ) %>%
    {left_join(
      x$table_body, .,
      by = c("variable", "row_type")
    )} %>%
    select(any_of(c("variable", "row_type", "label", "N")), everything())

  # adding N to table_header and assigning header label ------------------------
  x$table_header <-
    table_header_fill_missing(
      x$table_header,
      x$table_body
    ) %>%
    table_header_fmt_fun(N = style_number)

  x <- modify_header_internal(x, N = "**N**")

  # adding indicator to output that add_n was run on this data
  x$call_list <- c(x$call_list, list(add_n = match.call()))

  x
}
