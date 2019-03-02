#' Turn a regression model object into a markdown-ready tibble.
#'
#' This function uses \code{broom::tidy} from the `broom` or `broom.mixed` packages
#' to perform the initial model formatting. Review the `tbl_regression`
#' \href{http://www.danieldsjoberg.com/clintable/articles/tbl_regression.html}{vignette}
#' for detailed examples.
#'
#' @param x regression model object
#' @param exponentiate logical argument passed directly to
#' `tidy` function
#' Default is `FALSE`
#' @param label list of labels to write in the output. `list(age60 = "Age > 60")`
#' @param include names of variables to include in output.  Default is all variables.
#' @param conf.level confidence level passed directly to `tidy` function. Default is 0.95.
#' @param intercept logical argument indicates whether to include the intercept
#' in the output.  Default is `FALSE`
#' @param show_yesno Vector of names of categorical and factor variables that
#' are `c("No", "Yes")`, `c("no", "yes")`, or `c("NO", "YES")` default to dichotomous printing
#' (i.e. only Yes shown). To force both levels to be shown include the column
#' name in `show_yesno`, e.g. `show_yesno = c("highgrade", "female")`
#' @param beta_fun function to round and format beta coefficients.  Default is \code{\link{fmt_beta}}
#' @param pvalue_fun function to round and format p-values.  Default is \code{\link{fmt_pvalue}}
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
                           beta_fun = fmt_beta, pvalue_fun = fmt_pvalue) {
  # will return call, and all object passed to in table1 call
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

  # putting all results into tibble
  raw_results <-
    data_frame(variable = names(mod_list)) %>%
    mutate_(
      estimates = ~mod_list,
      var_type = ~ map_chr(estimates, ~ ifelse(nrow(.x) > 1, "categorical", "continuous")),
      var_label = ~ map_chr(
        variable, ~ label[[.x]] %||% attr(stats::model.frame(x)[[.x]], "label") %||% .x
      )
    )

  # number obs.
  n <- stats::model.frame(x) %>% nrow()

  # formatting raw results
  model_tbl <-
    raw_results %>%
    mutate_(
      # formatting stats
      estimates = ~ map(estimates, ~ fmt_estimates(.x, beta_fun, pvalue_fun)),
      # adding label
      estimates = ~ pmap(
        list(var_type, estimates, var_label, variable),
        ~ add_label(..1, ..2, ..3, ..4)
      ),
      N = ~n
    ) %>%
    tidyr::unnest_("estimates")

  # removing intercept term if indicated
  if (intercept == FALSE) {
    # removing the last row if it's called (Intercept)
    model_tbl <- model_tbl %>% filter(!(label == "(Intercept)" & row_number() == n()))
  }

  results <- list()
  results[["gt"]][["table_body"]] <-
    model_tbl %>%
    select(c(
      "row_type", "var_type", "variable", "label", "N",
      "est", "ll", "ul", "ci", "pvalue_exact", "pvalue", "p_pvalue"
    ))
  results[["n"]] <- n
  results[["model_obj"]] <- x
  results[["inputs"]] <- func_inputs

  # assigning a class of tbl_regression (for special printing in Rmarkdown)
  class(results) <- "tbl_regression"

  return(results)
}

# this function adds the label to the estimates tibble
# for continuous variables this means adding a column with the label.
# But for categorical, we add a row on top with the label
add_label <- function(var_type, estimates, var_label, variable) {
  case_when(
    var_type == "continuous" ~ list(estimates %>% mutate_(row_type = ~"label", label = ~var_label)),
    var_type == "categorical" ~ list(
      bind_rows(
        data_frame(row_type = "label", label = var_label),
        estimates %>% mutate_(
          row_type = ~"level",
          label = ~ stringr::str_replace(term, stringr::fixed(variable), "")
        )
      )
    )
  ) %>%
    pluck(1)
}

# little function that will round statistics,
# and add "Ref." for a reference categorical variable
fmt_estimates <- function(x, beta_fun, pvalue_fun) {
  x %>%
    mutate_(
      est = ~ ifelse(is.na(estimate), "Ref.", beta_fun(estimate)),
      ll = ~ beta_fun(conf.low),
      ul = ~ beta_fun(conf.high),
      ci = ~ ifelse(is.na(estimate), NA_character_, paste0(ll, ", ", ul)),
      pvalue_exact = ~p.value,
      pvalue = ~ pvalue_fun(p.value),
      p_pvalue = ~ case_when(
        is.na(pvalue) ~ NA_character_,
        stringr::str_sub(pvalue, end = 1L) %in% c("<", ">") ~ paste0("p", pvalue),
        TRUE ~ paste0("p=", pvalue)
      )
    )
}

