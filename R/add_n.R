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
#' @family tbl_summary tools
#' @family tbl_svysummary tools
#' @author Daniel D. Sjoberg
#' @export
#' @return A `tbl_summary` or `tbl_svysummary` object
#' @examples
#' tbl_n_ex <-
#'   trial[c("trt", "age", "grade", "response")] %>%
#'   tbl_summary(by = trt) %>%
#'   add_n()
#' @section Example Output:
#' \if{html}{\figure{tbl_n_ex.png}{options: width=50\%}}

add_n <- function(x, statistic = "{n}", col_label = "**N**", footnote = FALSE,
                  last = FALSE, missing = NULL) {
  # checking that input is class tbl_summary
  if (!(inherits(x, "tbl_summary") | inherits(x, "tbl_svysummary")))
    stop("`x` must be class 'tbl_summary' or 'tbl_svysummary'")

  # DEPRECATED specifying statistic via missing argument -----------------------
  if (!is.null(missing)) {
    lifecycle::deprecate_stop(
      "1.2.2",
      "gtsummary::add_n(missing = )",
      "gtsummary::add_n(statistic = )"
    )
  }

  # grabbing summary counts ----------------------------------------------------
  df_stats <-
    x$meta_data$df_stats %>%
    map_dfr(
      function(.x) {
        df_stats <-
          # selecting all columns with count data
          select(.x, any_of(c("variable", "by", "N_obs", "N_miss", "N_nonmiss", "p_miss",
                              "p_nonmiss", "N_obs_unweighted", "N_miss_unweighted",
                              "N_nonmiss_unweighted", "p_miss_unweighted",
                              "p_nonmiss_unweighted"))) %>%
          distinct()  %>%
          # summing counts within by variable within by levels
          dplyr::group_by_at(c("variable", "by") %>% intersect(names(.))) %>%
          mutate_at(vars(-any_of(c("variable", "by"))), sum) %>%
          select(-any_of("by")) %>%
          distinct()

        # styling the statistics -----------------------------------------------
        for (v in (names(df_stats) %>% setdiff("variable"))) {
          df_stats[[v]] <- df_stats[[v]] %>% attr(.x[[v]], "fmt_fun")()
        }

        # returning formatted df -----------------------------------------------
        df_stats %>%
          # adding these cols for backwards compatibility
          mutate(
            N = .data$N_obs,
            n = .data$N_nonmiss,
            n_miss = .data$N_miss,
            p = .data$p_nonmiss
          )
      }
    ) %>%
    # making the row that will be merged into table_body -----------------------
    mutate(
      statistic = glue(.env$statistic) %>% as.character(),
      row_type = "label"
    ) %>%
    select(.data$variable, .data$row_type, n = .data$statistic)

  # merging result with existing tbl_summary -----------------------------------
  x$table_body <-
    left_join(x$table_body, df_stats, by = c("variable", "row_type"))
  if (last == FALSE) {
    x$table_body <-
      select(x$table_body, any_of(c("variable", "row_type", "label", "n")), everything())
  }

  # updating table_header ------------------------------------------------------
  x$table_header <-
    table_header_fill_missing(
      x$table_header,
      x$table_body
    )
  x <- modify_header_internal(x, n = col_label)

  # Adding footnote if requested -----------------------------------------------
  if (footnote == TRUE) {
    x$table_header <-
      x$table_header %>%
      mutate(
        footnote = ifelse(.data$column == "n",
                          translate_text("Statistics presented",
                                         get_theme_element("pkgwide-str:language", default = "en")) %>%
                            paste0(": ", stat_to_label(statistic)),
                          .data$footnote)
      )
  }

  # adding indicator to output that add_n was run on this data
  x$call_list <- c(x$call_list, list(add_n = match.call()))

  # returning tbl_summary object
  return(x)
}

stat_to_label <- function(x) {
  language <- get_theme_element("pkgwide-str:language", default = "en")
  df_statistic_names <-
    tibble::tribble(
      ~stat, ~name,
      "{N}", "Total N",
      "{n}", "N not Missing",
      "{n_miss}", "N Missing",
      "{p}%", "% not Missing",
      "{p}", "% not Missing",
      "{p_miss}%", "% Missing",
      "{N_obs}", "Total N",
      "{N_miss}", "N Missing",
      "{N_nonmiss}", "N not Missing",
      "{p_nonmiss}", "% not Missing",
      "{N_obs_unweighted}", "Total N (unweighted)",
      "{N_miss_unweighted}", "N Missing (unweighted)",
      "{N_nonmiss_unweighted}", "N not Missing (unweighted)",
      "{p_miss_unweighted}", "% Missing (unweighted)",
      "{p_nonmiss_unweighted}", "% not Missing (unweighted)"
    ) %>%
    mutate(name = map_chr(.data$name, ~translate_text(.x, language)))

  for (i in seq_len(nrow(df_statistic_names))) {
    x <- stringr::str_replace_all(
      x,
      fixed(df_statistic_names$stat[i]),
      fixed(df_statistic_names$name[i])
    )
  }

  x
}
