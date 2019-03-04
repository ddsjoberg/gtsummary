#' Turn a regression model object into a markdown-ready tibble.
#'
#' This function uses \code{broom::tidy} from the `broom` or `broom.mixed` packages
#' to perform the initial model formatting. Review the `tbl_regression`
#' \href{http://www.danieldsjoberg.com/gtsummary/articles/tbl_regression.html}{vignette}
#' for detailed examples.
#'
#' @param x regression model object
#' @param exponentiate logical argument passed directly to
#' `tidy` function. Default is `FALSE`
#' @param label list of labels to write in the output. `list(age60 = "Age > 60")`
#' @param include names of variables to include in output.  Default is all variables.
#' @param conf.level confidence level passed directly to `tidy` function. Default is 0.95.
#' @param intercept logical argument indicates whether to include the intercept
#' in the output.  Default is `FALSE`
#' @param show_yesno Vector of names of categorical and factor variables that
#' are `c("No", "Yes")`, `c("no", "yes")`, or `c("NO", "YES")` default to dichotomous printing
#' (i.e. only Yes shown). To force both levels to be shown include the column
#' name in `show_yesno`, e.g. `show_yesno = c("highgrade", "female")`
#' @param coef_fun function to round and format beta coefficients.  Default is \code{\link{fmt_beta}}
#' @param pvalue_fun function to round and format p-values.  Default is \code{\link{style_pvalue}}
#' @author Daniel Sjoberg
#' @export
#' @examples
#' mod1 <- lm(hp ~ mpg + factor(cyl), mtcars)
#' tbl_regression(mod1)
#'
#' mod2 <- glm(response ~ age + grade + stage, trial, family = binomial(link = "logit"))
#' tbl_regression(mod2, exponentiate = TRUE)
#'
#' library(lme4)
#' mod_glmer <- glmer(am ~ hp + (1 | gear), mtcars, family = binomial)
#' tbl_regression(mod_glmer, exponentiate = TRUE)
tbl_regression <- function(x, exponentiate = FALSE, label = NULL,
                           include = names(stats::model.frame(x)),
                           show_yesno = NULL,
                           conf.level = 0.95, intercept = FALSE,
                           coef_fun = fmt_beta, pvalue_fun = style_pvalue) {
  # will return call, and all object passed to in tbl_regression call
  # the object func_inputs is a list of every object passed to the function
  func_inputs <- as.list(environment())

  # using broom and broom.mixed to tidy up regression results, and
  # then reversing order of data frame
  tidy_model <-
    tidy_wrap(x, exponentiate, conf.level) %>%
    map_df(rev) # reverses order of data frame

  # parsing the terms from model and variable names
  # outputing a named list--one entry per variable
  mod_list <- parse_terms(x, tidy_model, show_yesno)

  # keeping intercept if requested
  # Note, include = names(stats::model.frame(mod_nlme))
  # has an error for nlme because it is "reStruct"
  if (intercept == TRUE) include <- c(include, "(Intercept)")

  # keeping variables indicated in `include`
  if ((names(mod_list) %in% include) %>% any() == FALSE) {
    stop(glue::glue(
      "'include' must be in '{paste(names(mod_list), collapse = ', ')}'"
    ))
  }
  mod_list <- mod_list[names(mod_list) %in% include]

  # model N
  n = stats::model.frame(x) %>% nrow()

  # putting all results into tibble
  table_body <-
    tibble(variable = names(mod_list)) %>%
    mutate_(
      estimates = ~mod_list,
      var_type = ~ map_chr(estimates, ~ ifelse(nrow(.x) > 1, "categorical", "continuous")),
      var_label = ~ map_chr(
        variable, ~ label[[.x]] %||% attr(stats::model.frame(x)[[.x]], "label") %||% .x
      ),
      estimates = ~ pmap(
        list(var_type, estimates, var_label, variable),
        ~ add_label(..1, ..2, ..3, ..4)
      ),
      N = ~n
    ) %>%
    tidyr::unnest_("estimates") %>%
    select(c(
      "variable", "var_type", "row_type", "label", "N",
      "estimate", "conf.low", "conf.high", "p.value"
    )) %>%
    set_names(c(
      "variable", "var_type", "row_type", "label", "N",
      "coef", "ll", "ul", "pvalue"
    ))

  results <- list()
  results[["table_body"]] <- table_body
  results[["n"]] <- n
  results[["model_obj"]] <- x
  results[["inputs"]] <- func_inputs
  results[["call_list"]] <- list(tbl_summary = match.call())

  # returning all gt calls in a list
  # first call to the gt function
  results[["gt_calls"]][["gt"]] <- "gt::gt(data = x$table_body)"
  # label column indented and left just
  results[["gt_calls"]][["cols_align"]] <- glue(
    "gt::cols_align(align = 'center') %>% ",
    "gt::cols_align(align = 'left', columns = gt::vars(label))"
  )
  # do not print columns variable or row_type columns
  results[["gt_calls"]][["cols_hide"]] <-
    "gt::cols_hide(columns = gt::vars(variable, row_type, var_type, N))"
  # NAs do not show in table
  results[["gt_calls"]][["fmt_missing"]] <-
    "gt::fmt_missing(columns = gt::everything(), missing_text = '')"
  # # Show "---" for reference groups
  # results[["gt_calls"]][["fmt_missing"]] <-
  #   "gt::fmt_missing(columns = gt::everything(), rows = row_type == 'level', missing_text = '---')"

  # column headers
  results[["gt_calls"]][["cols_label"]] <- glue(
    "gt::cols_label(",
    "label = gt::md('**N = {n}**'), ",
    "coef = gt::md('**Coefficient**'), ",
    "ll = gt::md('**Confidence Interval**'), ",
    "pvalue = gt::md('**p-value**')",
    ")"
  )
  # adding p-value formatting (evaluate the expression with eval() function)
  results[["gt_calls"]][["fmt:pvalue"]] <-
    "gt::fmt(columns = gt::vars(pvalue), rows = !is.na(pvalue), fns = x$inputs$pvalue_fun)"
  # ceof and confidence interval formatting
  results[["gt_calls"]][["fmt:coef"]] <-
    "gt::fmt(columns = gt::vars(coef, ll, ul), rows = !is.na(coef), fns = x$inputs$coef_fun)"
  # combining ll and ul to print confidence interval
  results[["gt_calls"]][["cols_merge:ci"]] <-
    "gt::cols_merge(col_1 = gt::vars(ll), col_2 = gt::vars(ul), pattern = '{1}, {2}')"
  # indenting levels and missing rows
  results[["gt_calls"]][["tab_style:text_indent"]] <- glue(
    "gt::tab_style(",
    "style = gt::cells_styles(text_indent = gt::px(10), text_align = 'left'),",
    "locations = gt::cells_data(",
    "columns = gt::vars(label),",
    "rows = row_type != 'label'",
    "))"
  )


  # assigning a class of tbl_regression (for special printing in Rmarkdown)
  class(results) <- "tbl_regression"

  results
}



