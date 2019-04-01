#' Adds the global p-value for a categorical variables
#'
#' This function uses \code{\link[car]{Anova}} from the `car` package with `type = "III"` to calculate global p-values.
#' If a needed class of model is not supported by `car::`\code{\link[car]{Anova}}, please put in an
#' \href{https://github.com/ddsjoberg/gtsummary/issues}{issue} to request support.
#' Output from `tbl_regression` and `tbl_uvregression` objects supported.
#'
#' @param x `tbl_regression` or `tbl_uvregression` object
#' @param ... further arguments passed to or from other methods.
#' @seealso \code{\link{add_global.tbl_regression}}, \code{\link{add_global.tbl_uvregression}}
#' @author Daniel Sjoberg
#' @export
add_global <- function(x, ...) UseMethod("add_global")


#' Adds the global p-value for a categorical variables in `tbl_regression` objects
#'
#' This function uses \code{\link[car]{Anova}} from the `car` package with `type = "III"` to calculate global p-values.
#' If a needed class of model is not supported by \code{\link[car]{Anova}}, please put in an
#' issue at https://github.com/ddsjoberg/gtsummary/issues to request support.
#'
#' @param x object with class `tbl_regression` from the \code{\link{tbl_regression}} function
#' @param terms Character vector of terms for which to add global p-values.  Default
#' is `NULL` which will add global p-values for all categorical variables
#' @param keep logical argument whether to keep the individual p-values for the
#' levels of the categorical variable. Default is `FALSE`
#' @param ... arguments to be passed to \code{\link[car]{Anova}}.  Adding `test.statistic = `
#' can change the type of test (e.g. Likelihood-ratio, Wald, etc.).
#' @author Daniel Sjoberg
#' @family tbl_regression
#' @examples
#' tbl_lm <- lm(marker ~ stage + grade, trial) %>% tbl_regression() %>% add_global()
#' @export

add_global.tbl_regression <- function(x, terms = NULL, keep = FALSE, ...) {

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
  global_p <-
    x$model_obj %>%
    car::Anova(type = "III", ...) %>%
    as.data.frame() %>%
    tibble::rownames_to_column(var = "variable") %>%
    filter(!!parse_expr("variable %in% terms")) %>%
    select(c("variable", starts_with("Pr(>"))) %>% # selecting the pvalue column
    set_names(c("variable", "pvalue_global")) %>%
    mutate(row_type = "label")

  # merging in global pvalue
  x$table_body <-
    x$table_body %>%
    left_join(
      global_p,
      by = c("row_type", "variable")
    ) %>%
    mutate(
      pvalue = coalesce(.data$pvalue, .data$pvalue_global)
    ) %>%
    select(-c("pvalue_global"))

  # if keep == FALSE, then deleting variable-level p-values
  if (keep == FALSE) {
    x$table_body <-
      x$table_body %>%
      mutate(
        pvalue = if_else(.data$variable %in% terms & .data$row_type == "level", NA_real_, .data$pvalue)
      )
  }

  x$call_list <- c(x$call_list, list(add_global = match.call()))

  return(x)
}

#' Adds the global p-value for a categorical variables in `tbl_uvregression` objects
#'
#' This function uses \code{\link[car]{Anova}} from the `car` package with `type = "III"` to calculate global p-values.
#' If a needed class of model is not supported by \code{\link[car]{Anova}}, please put in an
#' issue at https://github.com/ddsjoberg/gtsummary/issues to request support.
#'
#' @param x object with class `tbl_uvregression` from the \code{\link{tbl_uvregression}} function
#' @param ... arguments to be passed to \code{\link[car]{Anova}}.  Adding `test.statistic = `
#' can change the type of test (e.g. Likelihood-ratio, Wald, etc.).
#' @author Daniel Sjoberg
#' @family tbl_uvregression
#' @examples
#' tbl_uv <-
#'   tbl_uvregression(
#'     trial,
#'     method = glm,
#'     y = response,
#'     method.args = list(family = binomial),
#'     exponentiate = TRUE
#'   ) %>%
#'   add_global()
#' @export

add_global.tbl_uvregression <- function(x, ...) {

  # calculating global pvalues
  global_p <-
    imap_dfr(
      x$tbl_regression_list,
      ~ car::Anova(.x[["model_obj"]], type = "III") %>%
        as.data.frame() %>%
        tibble::rownames_to_column(var = "variable") %>%
        filter(variable == .y) %>%
        select(c("variable", starts_with("Pr(>"))) %>% # selecting the pvalue column
        set_names(c("variable", "pvalue_global"))
    ) %>%
    select(c("variable", "pvalue_global"))

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
    select(-c("pvalue")) %>%
    left_join(
      global_p %>%
        set_names(c("variable", "pvalue")) %>%
        mutate(row_type = "label"),
      by = c("row_type", "variable")
    )

  x$call_list <- c(x$call_list, list(add_global = match.call()))

  return(x)
}
