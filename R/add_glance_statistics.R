#' Add Model Statistics
#'
#' \lifecycle{experimental}
#' Add model statistics returned from `broom::glance()`
#'
#' @param x 'tbl_regression' object
#' @param include list of statistics to include in output. Must be column
#' names of the tibble returned by `broom::glance()`. The include argument
#' can also be used to specify the order the statistics appear in the table.
#' @param label List of formulas specifying statistic labels,
#' e.g. `list(r.squared ~ "R2", p.value ~ "P")`
#' @param fmt_fun List of formulas where the LHS is a statistic and the RHS
#' is a function to format/round the statistics. The default is
#' `style_sigfig(x, digits = 3)`
#' @param location Location to place statistics.
#' Must be one of `c("table", "source_note")`
#' @param glance_fun function that returns model statistics. Default is
#' `broom::glance()`. Custom functions must return a single row tibble.
#' @param sep1 Used when `location = "source_note"`.
#' Separator between statistic name and statistic.
#' Default is `" = "`, e.g. `"R2 = 0.456"`
#' @param sep2 Used when `location = "source_note"`.
#' Separator between statistics. Default is `"; "`
#' @param text_interpret Used when `location = "source_note"`.
#' String indicates whether text will be interpreted with
#' [gt::md()] or [gt::html()]. Must be `"md"` (default) or `"html"`.
#'
#' @return gtsummary table
#' @export
#'
#' @examples
#' # Example 1 ----------------------------------
#' add_glance_statistics_ex1 <-
#'   lm(age ~ marker + grade, trial) %>%
#'   tbl_regression() %>%
#'   add_glance_statistics(
#'     label = list(sigma ~ "\U03C3"),
#'     include = c(r.squared, AIC, sigma)
#'   )
#' @section Example Output:
#' \if{html}{Example 1}
#'
#' \if{html}{\figure{add_glance_statistics_ex1.png}{options: width=64\%}}
add_glance_statistics <- function(x, include = everything(), label = NULL,
                                  fmt_fun = NULL, location = c("table", "source_note"),
                                  glance_fun = broom::glance,
                                  sep1 = " = ", sep2 = "; ",
                                  text_interpret = c("md", "html")) {
  # checking inputs ------------------------------------------------------------
  if (!inherits(x, "tbl_regression"))
    stop("`x=` must be class 'tbl_regression'")
  glance_fun <- gts_mapper(glance_fun, "add_glance_statistics(glance_fun=)")
  location <- match.arg(location)
  text_interpret <- match.arg(text_interpret)

  # prepping glance table ------------------------------------------------------
  df_glance_orig <- glance_fun(x$model_obj)
  include <- broom.helpers::.select_to_varnames({{ include }}, data = df_glance_orig)
  df_glance_orig <- df_glance_orig %>% select(all_of(include))

  # adding user-specified labels -----------------------------------------------
  label <-
    .formula_list_to_named_list(
      x = label,
      data = df_glance_orig,
      arg_name = "label"
    )

  if (!is.null(label)) df_label <- unlist(label) %>% tibble::enframe("variable", "label")
  else df_label <- tibble::tibble(variable = character(), label = character())

  # prepping data frame to be appended to `x$table_body` -----------------------
  language <- get_theme_element("pkgwide-str:language", default = "en")
  df_glance <-
    df_glance_orig %>%
    tidyr::pivot_longer(cols = everything(),
                        names_to = "variable",
                        values_to = "estimate") %>%
    # adding default labels
    left_join(df_default_glance_labels, by = c("variable" = "statistic_name")) %>%
    mutate(label = map_chr(.data$label, ~translate_text(.x, language = language))) %>%
    # updating table with user-specified labels
    dplyr::rows_update(df_label, by = "variable") %>%
    mutate(
      label = dplyr::coalesce(.data$label, .data$variable),
      row_type = "glance_statistic",
      var_label = .data$label
    )

  # parsing fmt_fun instructions -----------------------------------------------
  fmt_fun <-
    .formula_list_to_named_list(
      x = fmt_fun,
      data = df_glance_orig,
      arg_name = "fmt_fun"
    )

  df_fmt_fun <-
    tibble(
      glance_statistic = df_glance$variable,
      fmt_fun = list(purrr::partial(style_sigfig, digits = 3))
    )

  if (!is.null(fmt_fun)) {
    df_fmt_fun <-
      df_fmt_fun %>%
      dplyr::rows_update(
        tibble::enframe(fmt_fun, name = "glance_statistic", value = "fmt_fun"),
        by = "glance_statistic"
      )
  }

  # return stats as source note, if requested ----------------------------------
  if (location == "source_note") {
    x$table_styling$source_note <-
      df_glance %>%
      left_join(df_fmt_fun, by = c("variable" = "glance_statistic")) %>%
      rowwise() %>%
      mutate(
        fmt_stat = do.call(fmt_fun, list(estimate)) %>%
          {paste0(.data$label, sep1, .)}
      ) %>%
      pull(.data$fmt_stat) %>%
      paste(collapse = sep2)
    attr(x$table_styling$source_note, "text_interpret") <- text_interpret
    return(x)
  }

  # add instructions to print horizontal line ----------------------------------
  x$table_styling$horizontal_line_above <-
    expr(.data$row_type == "glance_statistic" & .data$variable %in% !!df_glance$variable[1])

  # updating regression gtsummary object ---------------------------------------
  # appending stats to table_body
  x$table_body <-
    x$table_body %>%
    bind_rows(df_glance)

  # creating modify_fmt_fun calls, and running them
  df_modify_fmt_fun <-
    df_fmt_fun %>%
    nest(glance_statistic = .data$glance_statistic) %>%
    rowwise() %>%
    mutate(
      glance_statistic = unlist(.data$glance_statistic) %>% unname() %>% list(),
    )

  # evaluating modify_fmt_fun calls on gtsumary object
  x <-
    map2(df_modify_fmt_fun$fmt_fun, df_modify_fmt_fun$glance_statistic,
         ~expr(
           modify_fmt_fun(
             update = list(estimate ~ !!.x),
             rows = .data$row_type == "glance_statistic" & .data$variable %in% !!.y
           )
         )) %>%
    reduce(function(x, y) expr(!!x %>% !!y), .init = expr(x)) %>%
    eval()

  # returning gtsummary table --------------------------------------------------
  x
}

# default statistic labels
df_default_glance_labels <-
  tibble::tribble(
    ~statistic_name, ~label,
    "r.squared", "R\U00B2",
    "adj.r.squared", "Adjusted R\U00B2",
    "p.value", "p-value",
    "logLik", "Log-likelihood",
    "statistic", "Statistic",
    "df.residual", "Residual df",
    "null.deviance", "Null deviance",
    "df.null", "Null df",
    "nevent", "N events",
    "concordance", "c-index",
    "std.error.concordance", "c-index SE",
    "nobs", "No. Obs.",
    "deviance", "Deviance",
    "sigma", "Sigma"
  )
