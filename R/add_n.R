#' Adds column with N to gtsummary table
#'
#' @param x Object created from a gtsummary function
#' @param ... Additional arguments passed to other methods.
#' @author Daniel D. Sjoberg
#' @seealso [add_n.tbl_summary()], [add_n.tbl_svysummary()], [add_n.tbl_survfit()],
#' [add_n.tbl_regression], [add_n.tbl_uvregression]
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
#' # Example 1 ----------------------------------
#' tbl_n_ex <-
#'   trial[c("trt", "age", "grade", "response")] %>%
#'   tbl_summary(by = trt) %>%
#'   add_n()
#' @section Example Output:
#'
#' \if{html}{Example 1}
#'
#' \if{html}{\figure{tbl_n_ex.png}{options: width=50\%}}

add_n.tbl_summary <- function(x, statistic = "{n}", col_label = "**N**", footnote = FALSE,
                              last = FALSE, missing = NULL, ...) {
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

        # correcting percentages -----------------------------------------------
        if ("p_miss" %in% names(df_stats))
          p_nonmiss <- mutate(df_stats, p_miss = .data$N_miss / .data$N_obs)
        if ("p_miss" %in% names(df_stats))
          df_stats <- mutate(df_stats, p_nonmiss = .data$N_nonmiss / .data$N_obs)
        if ("p_miss_unweighted" %in% names(df_stats))
          df_stats <- mutate(df_stats, p_miss_unweighted = .data$N_miss_unweighted / .data$N_obs_unweighted)
        if ("p_nonmiss_unweighted" %in% names(df_stats))
          df_stats <- mutate(df_stats, p_nonmiss_unweighted = .data$N_nonmiss_unweighted / .data$N_obs_unweighted)

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

  # updating table_styling -----------------------------------------------------
  x <-
    .update_table_styling(x) %>%
    modify_header(n = col_label)

  # Adding footnote if requested -----------------------------------------------
  if (footnote == TRUE) {
    x <- modify_footnote(x, n ~ stat_to_label(statistic))
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
      "{p_miss}", "% Missing",
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

#' @export
#' @rdname add_n.tbl_summary
add_n.tbl_svysummary <- add_n.tbl_summary

#' Add column with number of observations
#'
#' \lifecycle{experimental}
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

  # adding styling data for N column -------------------------------------------
  x <-
    modify_table_styling(
      x,
      columns = "N",
      label = "**N**",
      fmt_fun = style_number,
      hide = FALSE
    )

  # adding indicator to output that add_n was run on this data
  x$call_list <- c(x$call_list, list(add_n = match.call()))

  x
}


#' Add N to regression table
#'
#' @param x a `tbl_regression` or `tbl_uvregression` table
#' @param location location to place Ns. When `"label"` total Ns are placed
#' on each variable's label row. When `"level"` level counts are placed on the
#' variable level for categorical variables, and total N on the variable's label
#' row for continuous.
#' @param ... Not used
#'
#' @name add_n_regression
#' @examples
#' # Example 1 ----------------------------------
#' add_n.tbl_regression_ex1 <-
#'   trial %>%
#'   select(response, age, grade) %>%
#'   tbl_uvregression(
#'     y = response,
#'     method = glm,
#'     method.args = list(family = binomial),
#'     hide_n = TRUE
#'   ) %>%
#'   add_n(location = "label")
#'
#' # Example 2 ----------------------------------
#' add_n.tbl_regression_ex2 <-
#'   glm(response ~ age + grade, trial, family = binomial) %>%
#'   tbl_regression(exponentiate = TRUE) %>%
#'   add_n(location = "level")
#' @section Example Output:
#' \if{html}{Example 1}
#'
#' \if{html}{\figure{add_n.tbl_regression_ex1.png}{options: width=64\%}}
#'
#' \if{html}{Example 2}
#'
#' \if{html}{\figure{add_n.tbl_regression_ex2.png}{options: width=64\%}}
NULL

#' @rdname add_n_regression
#' @export
add_n.tbl_regression <- function(x, location = NULL, ...) {
  location <- match.arg(location, choices = c("label", "level"), several.ok = TRUE)

  if ("level" %in% location && !"n_obs" %in% x$table_styling$header$column)
    abort("Reporting N on level rows is not available for this model type.")
  if ("label" %in% location && !"N_obs" %in% x$table_styling$header$column)
    abort("Reporting N on label rows is not available for this model type.")

  x$table_body$stat_n <- NA_integer_
  if ("N_obs" %in% names(x$table_body))
    x$table_body$stat_n <- ifelse(x$table_body$row_type == "label",
                                  x$table_body$N_obs %>% as.integer(),
                                  x$table_body$stat_n)
  if ("n_obs" %in% names(x$table_body))
    x$table_body$stat_n <- ifelse(x$table_body$row_type == "level",
                                  x$table_body$n_obs %>% as.integer(),
                                  x$table_body$stat_n)
  x %>%
    modify_table_body(
      mutate,
      stat_n =
        case_when(
          !"level" %in% .env$location & .data$row_type %in% "level" ~ NA_integer_,
          !"label" %in% .env$location & .data$row_type %in% "label" &
            .data$var_type %in% c("categorical", "dichotomous") ~ NA_integer_,
          TRUE ~ .data$stat_n
        )
    ) %>%
    modify_table_body(
      dplyr::relocate,
      .data$stat_n,
      .after = .data$label
    ) %>%
    modify_header(stat_n ~ "**N**")
}

#' @export
#' @rdname add_n_regression
add_n.tbl_uvregression <- add_n.tbl_regression
