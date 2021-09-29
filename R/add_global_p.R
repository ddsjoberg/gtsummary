#' Add the global p-values
#'
#' This function uses `car::Anova(type = "III")` to calculate global p-values variables.
#' Output from `tbl_regression` and `tbl_uvregression` objects supported.
#'
#' @param x Object with class `tbl_regression` from the
#' [tbl_regression] function
#' @param keep Logical argument indicating whether to also retain the individual
#' p-values in the table output for each level of the categorical variable.
#' Default is `FALSE`
#' @param include Variables to calculate global p-value for. Input may be a vector of
#' quoted or unquoted variable names. Default is `everything()`
#' @param quiet Logical indicating whether to print messages in console. Default is
#' `FALSE`
#' @param terms DEPRECATED.  Use `include=` argument instead.
#' @param type Type argument passed to `car::Anova`. Default is `"III"`
#' @param ... Additional arguments to be passed to `car::Anova`
#' @author Daniel D. Sjoberg
#' @export
#' @examples
#' # Example 1 ----------------------------------
#' if (requireNamespace("car")) {
#'   tbl_lm_global_ex1 <-
#'     lm(marker ~ age + grade, trial) %>%
#'     tbl_regression() %>%
#'     add_global_p()
#' }
#'
#' # Example 2 ----------------------------------
#' if (requireNamespace("car")) {
#'   tbl_uv_global_ex2 <-
#'     trial[c("response", "trt", "age", "grade")] %>%
#'     tbl_uvregression(
#'       method = glm,
#'       y = response,
#'       method.args = list(family = binomial),
#'       exponentiate = TRUE
#'     ) %>%
#'     add_global_p()
#' }
#' @family tbl_uvregression tools
#' @family tbl_regression tools
#' @section Example Output:
#' \if{html}{Example 1}
#'
#' \if{html}{\figure{tbl_lm_global_ex1.png}{options: width=45\%}}
#'
#' \if{html}{Example 2}
#'
#' \if{html}{\figure{tbl_uv_global_ex2.png}{options: width=50\%}}

add_global_p <- function(x, ...) {
  # must have car package installed to use this function
  assert_package("car", "add_global_p()")
  UseMethod("add_global_p")
}

#' @name add_global_p
#' @export
add_global_p.tbl_regression <- function(x, include = everything(), type = NULL,
                                        anova_fun = NULL,  keep = FALSE,
                                        quiet = NULL, ..., terms = NULL) {
  updated_call_list <- c(x$call_list, list(add_global_p = match.call()))
  # deprecated arguments -------------------------------------------------------
  if (!is.null(terms)) {
    lifecycle::deprecate_stop(
      "1.2.5", "gtsummary::add_global_p.tbl_regression(terms = )",
      "add_global_p.tbl_regression(include = )"
    )
  }

  # setting defaults -----------------------------------------------------------
  quiet <- quiet %||% get_theme_element("pkgwide-lgl:quiet") %||% FALSE
  type <- type %||% get_theme_element("add_global_p-str:type", default = "III")

  # converting to character vector ---------------------------------------------
  include <-
    .select_to_varnames(
      select = {{ include }},
      var_info = x$table_body,
      arg_name = "include"
    )

  # if no terms are provided, stop and return x
  if (length(include) == 0) {
    if (quiet == FALSE) {
      inform("No terms were selected, and no global p-values were added to the table.")
    }
    return(x)
  }

  # vetted model geeglm not supported here.
  if (inherits(x$inputs$x, "geeglm")) {
    rlang::abort(paste(
      "Model class `geeglm` not supported by `car::Anova()`,",
      "and function could not calculate requested p-value."
    ))
  }

  # printing analysis performed
  if (!is.null(anova_fun)) {
    if (quiet == FALSE) {
      expr_car <-
        rlang::expr(car::Anova(x$model_obj, type = !!type, !!!list(...))) %>%
        deparse()

      paste(
        "add_global_p: Global p-values for variable(s)",
        glue("`add_global_p(include = {deparse(include) %>% paste(collapse = '')})`"),
        glue("were calculated with")
      ) %>%
        stringr::str_wrap() %>%
        paste(glue("`{expr_car}`"), sep = "\n  ") %>%
        rlang::inform()
    }

    # calculating global pvalues
    tryCatch(
      {
        car_Anova <-
          x$model_obj %>%
          car::Anova(type = type, ...) %>%
          {suppressWarnings(broom::tidy(.))}
      },
      error = function(e) {
        paste0(
          "{.code add_global_p()} uses ",
          "{.code car::Anova() %>% broom::tidy()} to calculate the global p-value,\n",
          "and the function returned an error while calculating the p-values.\n",
          "Is your model type supported by {.code car::Anova()}?"
        ) %>%
          cli_alert_danger()
        stop(e)
      }
    )
  }
  # performing Anova + tidying with user-specified function
  else {
    tryCatch(
      car_Anova <- anova_fun(x$model_obj)
    )
  }

  # cleaning up data frame with global p-values before merging -----------------
  global_p <-
    car_Anova %>%
    mutate(
      variable = broom.helpers::.clean_backticks(.data$term),
      row_type = "label"
    ) %>%
    filter(.data$variable %in% .env$include) %>%
    select(any_of(c("variable", "row_type", "p.value"))) %>%
    set_names(c("variable", "row_type", "p.value_global"))


  # merging in global pvalue ---------------------------------------------------
  # adding p-value column, if it is not already there
  if (!"p.value" %in% names(x$table_body)) {
    # adding p.value to table_body
    x$table_body <- mutate(x$table_body, p.value = NA_real_)
    x <-
      modify_table_styling(
        x,
        columns = "p.value",
        label = "**p-value**",
        hide = FALSE,
        fmt_fun = x$inputs$pvalue_fun %||% getOption("gtsummary.pvalue_fun", default = style_pvalue)
      )
  }
  # adding global p-values
  x$table_body <-
    x$table_body %>%
    left_join(
      global_p,
      by = c("row_type", "variable")
    ) %>%
    mutate(
      p.value = coalesce(.data$p.value_global, .data$p.value)
    ) %>%
    select(-c("p.value_global"))

  # if keep == FALSE, then deleting variable-level p-values
  if (keep == FALSE) {
    x$table_body <-
      x$table_body %>%
      mutate(
        p.value = if_else(.data$variable %in% !!include & .data$row_type == "level",
          NA_real_, .data$p.value
        )
      )
  }

  x$call_list <- updated_call_list
  x
}

#' @name add_global_p
#' @export
add_global_p.tbl_uvregression <- function(x, type = NULL, include = everything(),
                                          keep = FALSE, quiet = NULL, ...) {
  updated_call_list <- c(x$call_list, list(add_global_p = match.call()))
  # setting defaults -----------------------------------------------------------
  quiet <- quiet %||% get_theme_element("pkgwide-lgl:quiet") %||% FALSE
  type <- type %||% get_theme_element("add_global_p-str:type", default = "III")

  # converting to character vector ---------------------------------------------
  include <-
    .select_to_varnames(
      select = {{ include }},
      var_info = x$table_body,
      arg_name = "include"
    )

  # capturing dots in expression
  dots <- rlang::enexprs(...)

  # printing analysis performed
  if (quiet == FALSE) {
    expr_car <-
      rlang::expr(car::Anova(mod = x$model_obj, type = !!type, !!!list(...))) %>%
      deparse()

    paste(
      "add_global_p: Global p-values for variable(s)",
      glue("`add_global_p(include = {deparse(include) %>% paste(collapse = '')})`"),
      glue("were calculated with")
    ) %>%
      stringr::str_wrap() %>%
      paste(glue("`{expr_car}`"), sep = "\n  ") %>%
      rlang::inform()
  }

  # calculating global pvalues
  global_p <-
    imap_dfr(
      x$tbls[include],
      function(x, y) {
        tryCatch(
          {
            car_Anova <-
              rlang::call2(
                car::Anova,
                mod = x[["model_obj"]], type = type, !!!dots
              ) %>%
              rlang::eval_tidy()
          },
          error = function(e) {
            paste0(
              "{.code add_global_p()} uses ",
              "{.code car::Anova()} to calculate the global p-value,\n",
              "and the function returned an error while calculating the p-value ",
              "for {.val {y}}."
            ) %>%
              cli_alert_danger()
            stop(e)
          }
        )

        car_Anova %>%
          {suppressWarnings(broom::tidy(.))} %>%
          mutate(
            variable = broom.helpers::.clean_backticks(.data$term),
            row_type = "label"
          ) %>%
          filter(.data$variable %in% .env$include) %>%
          select(any_of(c("variable", "row_type", "p.value"))) %>%
          set_names(c("variable", "row_type", "p.value_global"))
      }
    ) %>%
    select(c("variable", "p.value_global"))

  # adding global p-value to meta_data object
  x$meta_data <-
    x$meta_data %>%
    left_join(
      global_p,
      by = "variable"
    )

  # merging in global pvalue ---------------------------------------------------
  # adding p-value column, if it is not already there
  if (!"p.value" %in% names(x$table_body)) {
    # adding p.value to table_body
    x$table_body <- mutate(x$table_body, p.value = NA_real_)
    x <-
      modify_table_styling(
        x,
        columns = "p.value",
        label = "**p-value**",
        hide = FALSE,
        fmt_fun = x$inputs$pvalue_fun %||% getOption("gtsummary.pvalue_fun", default = style_pvalue)
      )
  }
  # adding global p-values
  x$table_body <-
    x$table_body %>%
    left_join(
      global_p %>% mutate(row_type = "label"),
      by = c("row_type", "variable")
    ) %>%
    mutate(
      p.value = coalesce(.data$p.value_global, .data$p.value)
    ) %>%
    select(-c("p.value_global"))

  # if keep == FALSE, then deleting variable-level p-values
  if (keep == FALSE) {
    x$table_body <-
      x$table_body %>%
      mutate(
        p.value = if_else(.data$variable %in% !!include & .data$row_type == "level",
          NA_real_, .data$p.value
        )
      )
  }

  x$call_list <- updated_call_list
  x
}
