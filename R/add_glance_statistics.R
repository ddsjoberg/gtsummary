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
#' @param glance_fun function that returns model statistics. Default is
#' `broom::glance()`. Custom functions must return a single row tibble.
#' @param ... Additional arguments passed to `glance_fun=`
#'
#' @return gtsummary table
#' @export
#'
#' @examples
#' # add example
add_glance_statistics <- function(x, include = everything(), label = NULL,
                                  fmt_fun = NULL, glance_fun = broom::glance, ...) {
  # checking inputs ------------------------------------------------------------
  if (!inherits(x, "tbl_regression"))
    stop("`x=` must be class 'tbl_regression'")
  glance_fun <- gts_mapper(glance_fun, "add_glance_statistics(glance_fun=)")

  # prepping glance table ------------------------------------------------------
  df_glance_orig <- glance_fun(x$model_obj, ...)
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
  df_glance <-
    df_glance_orig %>%
    tidyr::pivot_longer(cols = everything(),
                        names_to = "variable",
                        values_to = "estimate") %>%
    # adding default labels
    left_join(df_default_glance_labels, by = c("variable" = "statistic_name")) %>%
    # updating table with user-specified labels
    dplyr::rows_update(df_label, by = "variable") %>%
    mutate(
      label = dplyr::coalesce(.data$label, .data$variable),
      row_type = "glance_statistic",
      var_label = .data$label
    )

  # add instructions to print horizontal line ----------------------------------
  x$list_output$horizontal_line_above <-
    expr(.data$row_type == "glance_statistic" & .data$variable %in% !!df_glance$variable[1])

  language <- get_theme_element("pkgwide-str:language", default = "en")

  # updating regression gtsummary object ---------------------------------------
  # appending stats to table_body
  x$table_body <-
    x$table_body %>%
    bind_rows(df_glance)

  # adding fmt_fun instructions
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
