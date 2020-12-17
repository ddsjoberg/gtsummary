#' Add glance statistics
#'
#' \lifecycle{experimental}
#' Add the statistics returned in `broom::glance()` as a table source note.
#'
#' @param x 'tbl_regression' object
#' @param include tidyselect list of statistics to include. Default is `everything()`
#' @param label use to update statistic labels
#' @param fmt_fun use to update default formatting function. Default is
#' `everything() ~ purrr::partial(style_sigfig, digits = 3)`
#' @param glance_fun function to calculate and return glance statistics.
#' Default is `broom::glance()`
#' @param sep1 Separator between statistic name and statistic.
#' Default is `" = "`, e.g. `"R2 = 0.456"`
#' @param sep2 Separator between statistics. Default is `"; "`
#' @param ... additional arguments passed to `broom::glance()`
#'
#' @export
#' @section Default Labels:
#' The following statistics have set default labels when being printed.
#' When there is no default, the label is the column name from `broom::glance()`.
#'
#' ```{r, echo = FALSE}
#' df_default_glance_labels %>%
#'   select(`Statistic Name` = statistic_name, `Default Label` = label) %>%
#'   knitr::kable()
#' ```
#'
#' @examples
#' # Example 1 ----------------------------------
#' add_glance_source_note_ex1 <-
#'   lm(age ~ marker + grade, trial) %>%
#'   tbl_regression() %>%
#'   add_glance_source_note(
#'     label = list(df  ~ "Degrees of Freedom", sigma ~ "\U03C3"),
#'     fmt_fun = df ~ style_number,
#'     include = c(r.squared, AIC, sigma, df)
#'   )
#' @section Example Output:
#' \if{html}{Example 1}
#'
#' \if{html}{\figure{add_glance_source_note_ex1.png}{options: width=64\%}}

add_glance_source_note <- function(x, include = everything(), label = NULL,
                                   fmt_fun = NULL, glance_fun = broom::glance,
                                   sep1 = " = ", sep2 = "; ", ...) {
  # checking inputs ------------------------------------------------------------
  if (!inherits(x, "tbl_regression"))
    stop("`x=` must be class 'tbl_regression'")

  # prepping table -------------------------------------------------------------
  df_glance_orig <- glance_fun(x$model_obj, ...)
  df_glance <-
    df_glance_orig %>%
    tidyr::pivot_longer(cols = everything(),
                        names_to = "statistic_name",
                        values_to = "statistic")

  language <- get_theme_element("pkgwide-str:language", default = "en")

  df_results <-
    df_glance %>%
    mutate(
      label = .data$statistic_name,
      fmt_fun = list(purrr::partial(style_sigfig, digits = 3))
    ) %>%
    # adding default labels
    dplyr::rows_update(
      # keeping labels in current glance
      inner_join(df_default_glance_labels,
                 select(df_glance, .data$statistic_name),
                 by = "statistic_name"),
      by = "statistic_name"
    ) %>%
    # translating statistic names
    mutate(label = map_chr(.data$label, ~translate_text(.x, language)))

  # evaluating tidyselects -----------------------------------------------------
  include <-
    .select_to_varnames(
      select = {{ include }},
      data = df_glance_orig,
      arg_name = "include"
    )

  label <-
    .formula_list_to_named_list(
      x = label,
      data = df_glance_orig,
      arg_name = "label"
    )

  if (rlang::is_function(fmt_fun)) fmt_fun <- everything() ~ fmt_fun
  fmt_fun <-
    .formula_list_to_named_list(
      x = fmt_fun,
      data = df_glance_orig,
      arg_name = "fmt_fun"
    )

  # updating df_results with new information -----------------------------------
  if (!is.null(label)) {
    df_results <-
      df_results %>%
      dplyr::rows_update(
        unlist(label) %>% tibble::enframe(name = "statistic_name", value = "label"),
        by = "statistic_name"
      )
  }

  if (!is.null(fmt_fun)) {
    df_results <-
      df_results %>%
      dplyr::rows_update(
        tibble::enframe(fmt_fun, name = "statistic_name", value = "fmt_fun"),
        by = "statistic_name"
      )
  }

  # putting stats in order selected, and only keeping those selected
  df_results <-
    inner_join(
      tibble(statistic_name = include),
      df_results,
      by = "statistic_name"
    )

  # constructing source note ---------------------------------------------------
  x$list_output$source_note <-
    df_results %>%
    mutate(
      statistic_fmt = map2_chr(.data$fmt_fun, .data$statistic, ~.x(.y)),
      statistic_label = paste0(.data$label, sep1, .data$statistic_fmt)
    ) %>%
    pull(.data$statistic_label) %>%
    paste(collapse = sep2)

  # return gtsummary object ----------------------------------------------------
  x
}

# default statistic labels
df_default_glance_labels <-
  tibble::tribble(
    ~statistic_name, ~label,
    "r.squared", "R\U00B2",
    "adj.r.squared", "Adjusted R\U00B2",
    "p.value", "p-value",
    "logLik", "log-likelihood",
    "statistic", "Statistic",
    "df.residual", "Residual df",
    "null.deviance", "Null deviance",
    "df.null", "Null df",
    "nevent", "N events",
    "concordance", "c-index",
    "std.error.concordance", "c-index SE"
  )


