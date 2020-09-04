#' Add glance statistics
#'
#' \Sexpr[results=rd, stage=render]{lifecycle::badge("experimental")}
#' Add the statistics returned in `broom::glance()` as a table source note.
#'
#' @param x 'tbl_regression' object
#' @param include tidyselect list of statistics to include. Default is `everything()`
#' @param label use to update statistic labels
#' @param fmt_fun use to update default formatting function. Default is
#' `everything() ~ purrr::partial(style_sigfig, digits = 3)`
#' @param sep1 Separator between statistic name and statistic. Default is `"="`
#' @param sep2 Separator between statistics. Default is `"; "`
#' @param ... additional argument passed to `broom::glance()`
#'
#' @export
#'
#' @examples
#' add_glance_source_note_ex1 <-
#'   lm(age ~ marker + grade, trial) %>%
#'   tbl_regression() %>%
#'   add_glance_source_note(
#'     label = r.squared ~ "R^2",
#'     include = c(r.squared, AIC)
#'   )
#' @section Example Output:
#' \if{html}{Example 1}
#'
#' \if{html}{\figure{add_glance_source_note_ex1.png}{options: width=64\%}}

add_glance_source_note <- function(x, include = everything(), label = NULL,
                                   fmt_fun = NULL, sep1 = "=", sep2 = "; ", ...) {
  # checking inputs ------------------------------------------------------------
  if (!inherits(x, "tbl_regression"))
    stop("`x=` must be class 'tbl_regression'")

  # prepping table -------------------------------------------------------------
  df_results <-
    broom::glance(x$model_obj, ...) %>%
    tidyr::pivot_longer(cols = everything(),
                        names_to = "statistic_name",
                        values_to = "statistic") %>%
    mutate(
      label = statistic_name,
      fmt_fun = list(purrr::partial(style_sigfig, digits = 3))
    ) %>%
    dplyr::rows_update(
      df_default_glance_labels,
      by = "statistic_name"
    )

  # evaluating tidyselects -----------------------------------------------------
  include <- var_input_to_string(
    data = vctr_2_tibble(df_results$statistic_name), arg_name = "include",
    select_single = FALSE, select_input = {{include}}
  )

  label <-
    tidyselect_to_list(vctr_2_tibble(df_results$statistic_name), label, arg_name = "label")

  if (rlang::is_function(fmt_fun)) fmt_fun <- everything() ~ fmt_fun
  fmt_fun <-
    tidyselect_to_list(vctr_2_tibble(df_results$statistic_name), fmt_fun, arg_name = "fmt_fun")


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

  df_results <- filter(df_results, statistic_name %in% include)

  # constructing source note ---------------------------------------------------
  x$list_output$source_note <-
    df_results %>%
    dplyr::rowwise() %>%
    mutate(
      statistic_fmt = do.call(fmt_fun, list(statistic)),
      statistic_label = paste(label, sep1, statistic_fmt)
    ) %>%
    pull(statistic_label) %>%
    paste(collapse = sep2)

  # return gtsummary object ----------------------------------------------------
  x
}

# default statistic labels
df_default_glance_labels <-
  tibble::tribble(
    ~statistic_name, ~label,
    "r.squared", "R2",
    "adj.r.squared", "Adjusted R2",
    "p.value", "p-value",
    "logLik", "log likelihood",
    "nobs", "N"
  )


