#' Add the global p-values
#'
#' This function uses `car::Anova()` to calculate global p-values
#' for model covariates.
#' Output from `tbl_regression` and `tbl_uvregression` objects supported.
#'
#' @param x Object with class `'tbl_regression'` or `'tbl_uvregression'`
#' @param keep Logical argument indicating whether to also retain the individual
#' p-values in the table output for each level of the categorical variable.
#' Default is `FALSE`.
#' @param anova_fun Function that will be used in place of `car::Anova()`
#' when specified to calculate the global p-values.
#' - function must return a tibble matching the output of
#' `car::Anova() %>% broom::tidy()` including a columns called `"term"` and `"p.values"`
#' - function must accept arguments `anova_fun(x, ...)`, where `x` is a model object
#' - arguments passed in `...` will be passed to `anova_fun(...)`
#' - the `add_global_p(type=)` argument is _ignored_ in `anova_fun=`
#' - a common function used here is `tidy_wald_test()`, a wrapper for `aod::wald.test()`
#' @param include Variables to calculate global p-value for. Input may be a vector of
#' quoted or unquoted variable names. Default is `everything()`
#' @param quiet Logical indicating whether to print messages in console. Default is
#' `FALSE`
#' @param type Type argument passed to `car::Anova(type=)`. Default is `"III"`
#' @param ... Additional arguments to be passed to `car::Anova`,
#' `aod::wald.test()` or `anova_fun` (if specified)
#' @author Daniel D. Sjoberg
#' @export
#' @seealso Review [list, formula, and selector syntax][syntax] used throughout gtsummary
#'
#' @examplesIf broom.helpers::.assert_package("car", pkg_search = "gtsummary", boolean = TRUE)
#' \donttest{
#' # Example 1 ----------------------------------
#' tbl_lm_global_ex1 <-
#'   lm(marker ~ age + grade, trial) %>%
#'   tbl_regression() %>%
#'   add_global_p()
#'
#' # Example 2 ----------------------------------
#' tbl_uv_global_ex2 <-
#'   trial[c("response", "trt", "age", "grade")] %>%
#'   tbl_uvregression(
#'     method = glm,
#'     y = response,
#'     method.args = list(family = binomial),
#'     exponentiate = TRUE
#'   ) %>%
#'   add_global_p()
#' }
#' @family tbl_uvregression tools
#' @family tbl_regression tools
#' @section Example Output:
#' \if{html}{Example 1}
#'
#' \if{html}{\out{
#' `r man_create_image_tag(file = "tbl_lm_global_ex1.png", width = "45")`
#' }}
#'
#' \if{html}{Example 2}
#'
#' \if{html}{\out{
#' `r man_create_image_tag(file = "tbl_uv_global_ex2.png", width = "50")`
#' }}

add_global_p <- function(x, ...) {
  # must have car package installed to use this function
  assert_package("car", "add_global_p()")
  UseMethod("add_global_p")
}

#' @name add_global_p
#' @export
add_global_p.tbl_regression <- function(x,
                                        include = everything(),
                                        type = NULL,
                                        keep = FALSE,
                                        anova_fun = NULL,
                                        quiet = NULL,
                                        ...) {
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

  # if no terms are provided, stop and return x
  if (length(include) == 0) {
    if (quiet == FALSE) {
      inform("No terms were selected, and no global p-values were added to the table.")
    }
    return(x)
  }

  # calculating global p-values ------------------------------------------------
  car_Anova <- .run_anova(x = x$model_obj, type = type, anova_fun = anova_fun, ...)

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
        fmt_fun = x$inputs$pvalue_fun %||% .get_deprecated_option("gtsummary.pvalue_fun", default = style_pvalue)
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
                                          keep = FALSE, anova_fun = NULL,
                                          quiet = NULL, ...) {
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

  # calculating global pvalues
  global_p <-
    imap_dfr(
      x$tbls[include],
      function(tbl, tbl_name) {
        # return empty tibble if variable not included
        if (!tbl_name %in% include) {
          return(tibble(
            variable = character(0L),
            row_type = character(0L),
            p.value_global = numeric(0L)
          ))
        }
        car_Anova <-
          .run_anova(
            x = tbl[["model_obj"]], type = type,
            anova_fun = anova_fun, variable = tbl_name, ...
          )

        car_Anova %>%
          mutate(
            variable = broom.helpers::.clean_backticks(.data$term),
            row_type = "label"
          ) %>%
          filter(
            (!is.null(x$inputs$y) & .data$variable %in% .env$tbl_name) |
              (!is.null(x$inputs$x) & .data$variable %in% x$inputs$x)
          ) %>%
          mutate(variable = .env$tbl_name) %>% # required when using `tbl_uvregression(x=)`
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
        fmt_fun = x$inputs$pvalue_fun %||% .get_deprecated_option("gtsummary.pvalue_fun", default = style_pvalue)
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

tidy_car_anova <- function(x, type, tbl, ...) {
  car::Anova(mod = x, type = type, ...) %>%
    {
      suppressWarnings(broom::tidy(.))
    }
}

# this function runs anova_fun if specified.
# if anova_fun is NULL, then we use car::Anova,
.run_anova <- function(x, type, anova_fun, variable = NULL, ...) {
  dots <- rlang::dots_list(...)

  # running custom function passed by user
  if (!is.null(anova_fun)) {
    result <-
      tryCatch(
        do.call(anova_fun, args = c(list(x = x), dots)),
        error = function(e) {
          ifelse(
            !is.null(variable),
            "There was an error running {.code anova_fun()} for {.val {variable}}",
            "There was an error running {.code anova_fun()}"
          ) %>%
            cli::cli_alert_danger()
          stop(e)
        }
      )
    return(result)
  }

  # running car::Anova()
  tryCatch(
    tidy_car_anova(x = x, type = type, ...),

    # trying `add_global_p(anova_fun = gtsummary::tidy_wald_test)` if `car::Anova()` fails
    error = function(e) {
      ifelse(
        !is.null(variable),
        "There was an error running {.code car::Anova()} for {.val {variable}}, ",
        "There was an error running {.code car::Anova()}, "
      ) %>%
        paste0(
          "likely due to this model type not being supported. ",
          "The results displayed are based on {.code add_global_p(anova_fun = gtsummary::tidy_wald_test)}"
        ) %>%
        cli::cli_alert_danger()

      tryCatch(
        do.call(tidy_wald_test, args = c(list(x = x), dots)),
        error = function(e) {
          paste0("{.code add_global_p(anova_fun = gtsummary::tidy_wald_test)} failed...") %>%
            cli::cli_alert_danger()
          stop(e, call. = FALSE)
        }
      )
    }
  )
}
