#' Adds the global p-value for a categorical variables
#'
#' This function uses [car::Anova] with argument
#' `type = "III"` to calculate global p-values for categorical variables.
#' Output from `tbl_regression` and `tbl_uvregression` objects supported.
#'
#' @section Note:
#' If a needed class of model is not supported by
#' [car::Anova], please create an
#' [issue](https://github.com/ddsjoberg/gtsummary/issues) to request support.
#'
#' @param x `tbl_regression` or `tbl_uvregression` object
#' @param ... Further arguments passed to or from other methods.
#' @seealso \code{\link{add_global_p.tbl_regression}},
#' \code{\link{add_global_p.tbl_uvregression}}
#' @author Daniel D. Sjoberg
#' @export

add_global_p <- function(x, ...) {
  # must have car package installed to use this function
  if (!requireNamespace("car", quietly = TRUE)) {
    stop(paste0(
      "The 'car' package is required for 'add_global_p'.\n",
      "Install with install.packages('car')"), call. = FALSE)
  }
  UseMethod("add_global_p")
}

#' Adds the global p-value for categorical variables
#'
#' This function uses [car::Anova] with argument
#' `type = "III"` to calculate global p-values for categorical variables.
#'
#' @section Note:
#' If a needed class of model is not supported by
#' [car::Anova], please create an
#' [issue](https://github.com/ddsjoberg/gtsummary/issues) to request support.
#'
#'
#' @param x Object with class `tbl_regression` from the
#' [tbl_regression] function
#' @param terms Character vector of terms for which to add global p-values.  Default
#' is `NULL` which will add global p-values for all categorical variables
#' @param keep Logical argument indicating whether to also retain the individual
#' p-values in the table output for each level of the categorical variable.
#' Default is `FALSE`
#' @param ... Additional arguments to be passed to [car::Anova]
#' @author Daniel D. Sjoberg
#' @family tbl_regression tools
#' @examples
#' tbl_lm_global_ex1 <-
#'   lm(marker ~ age + grade, trial) %>%
#'   tbl_regression() %>%
#'   add_global_p()
#' @export
#' @return A `tbl_regression` object
#' @section Example Output:
#' \if{html}{\figure{tbl_lm_global_ex1.png}{options: width=50\%}}

add_global_p.tbl_regression <- function(x, terms = NULL, keep = FALSE, ...) {

  # fetching categorical variables from model
  model_terms <- x %>%
    pluck("table_body") %>%
    select(c("var_type", "variable")) %>%
    filter(!!parse_expr('var_type == "categorical"')) %>%
    distinct() %>%
    pull("variable")

  # if not terms supplied, getting list of all categorical terms in model
  if (is.null(terms)) terms <- model_terms

  # if no terms are provided, stop and return x
  if (length(terms) == 0) {
    message("No terms were selected, and no global p-values added to table")
    return(x)
  }

  # check that terms selected appear in model.
  if (!all(terms %in% model_terms)) {
    stop(glue(
      "Terms selected are not categorical terms from model: ",
      "{paste(terms[!(terms %in% model_terms)], collpase = ', ')}"
    ))
  }

  # calculating global pvalues
  tryCatch({
    car_Anova <-
      x$model_obj %>%
      car::Anova(type = "III", ...)
  },
  error = function(e) {
    usethis::ui_oops(paste0(
      "{usethis::ui_code('add_global_p()')} uses ",
      "{usethis::ui_code('car::Anova()')} to calculate the global p-value,\n",
      "and the function returned an error while calculating the p-values."
    ))
    stop(e)
  })

  global_p <-
    car_Anova %>%
    as.data.frame() %>%
    tibble::rownames_to_column(var = "variable") %>%
    filter(!!parse_expr("variable %in% terms")) %>%
    select(c("variable", starts_with("Pr(>"))) %>% # selecting the pvalue column
    set_names(c("variable", "p.value_global")) %>%
    mutate(row_type = "label")

  # merging in global pvalue
  x$table_body <-
    x$table_body %>%
    left_join(
      global_p,
      by = c("row_type", "variable")
    ) %>%
    mutate(
      p.value = coalesce(.data$p.value, .data$p.value_global)
    ) %>%
    select(-c("p.value_global"))

  # if keep == FALSE, then deleting variable-level p-values
  if (keep == FALSE) {
    x$table_body <-
      x$table_body %>%
      mutate(
        p.value = if_else(.data$variable %in% terms & .data$row_type == "level",
                          NA_real_, .data$p.value)
      )
  }

  x$call_list <- c(x$call_list, list(add_global_p = match.call()))

  return(x)
}

#' Adds the global p-value for categorical variables
#'
#' This function uses [car::Anova] with argument
#' `type = "III"` to calculate global p-values for categorical variables.
#'
#' @param x Object with class `tbl_uvregression` from the
#' [tbl_uvregression] function
#' @param ... Additional arguments to be passed to [car::Anova].
#' @author Daniel D. Sjoberg
#' @family tbl_uvregression tools
#' @examples
#' tbl_uv_global_ex2 <-
#'   trial %>%
#'   dplyr::select(response, trt, age, grade) %>%
#'   tbl_uvregression(
#'     method = glm,
#'     y = response,
#'     method.args = list(family = binomial),
#'     exponentiate = TRUE
#'   ) %>%
#'   add_global_p()
#' @export
#' @return A `tbl_uvregression` object
#' @section Example Output:
#' \if{html}{\figure{tbl_uv_global_ex2.png}{options: width=50\%}}
#'
add_global_p.tbl_uvregression <- function(x, ...) {

  # converting the passed ... to a list, OR if nothing passed to NULL
  if (length(list(...)) == 0) {
    passed_dots <- NULL
  } else {
    passed_dots <- list(...)
  }

  # calculating global pvalues
  global_p <-
    imap_dfr(
      x$tbl_regression_list,
      function(x, y) {
          tryCatch({
            car_Anova <-
              do.call(
                car::Anova,
                c(list(mod = x[["model_obj"]], type = "III"), passed_dots)
              )
          },
          error = function(e) {
            usethis::ui_oops(paste0(
              "{usethis::ui_code('add_global_p()')} uses ",
              "{usethis::ui_code('car::Anova()')} to calculate the global p-value,\n",
              "and the function returned an error while calculating the p-value ",
              "for {usethis::ui_value(y)}."
            ))
            stop(e)
          })

        car_Anova %>%
          as.data.frame() %>%
          tibble::rownames_to_column(var = "variable") %>%
          filter(.data$variable == y) %>%
          select(c(
            "variable", starts_with("Pr(>")
          )) %>% # selecting the pvalue column
          set_names(c("variable", "p.value_global"))
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

  # merging in global pvalue
  x$table_body <-
    x$table_body %>%
    select(-c("p.value")) %>%
    left_join(
      global_p %>%
        set_names(c("variable", "p.value")) %>%
        mutate(row_type = "label"),
      by = c("row_type", "variable")
    )

  x$call_list <- c(x$call_list, list(add_global_p = match.call()))

  return(x)
}
