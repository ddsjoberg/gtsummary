#' Turn a regression model object into a markdown-ready tibble.
#'
#' This function uses \code{broom::tidy} from the `broom` or `broom.mixed` packages
#' to perform the initial model formatting. Review the `fmt_regression`
#' \href{http://www.danieldsjoberg.com/clintable/articles/fmt_regression.html}{vignette}
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
#' fmt_regression(mod1)
#'
#' mod2 <- glm(response ~ age + grade + stage, trial, family = binomial(link = "logit"))
#' fmt_regression(mod2, exponentiate = TRUE)
#'
#' library(lme4)
#' mod_glmer <- glmer(am ~ hp + (1 | gear), mtcars, family = binomial)
#' fmt_regression(mod_glmer, exponentiate = TRUE)
fmt_regression <- function(x, exponentiate = FALSE, label = NULL,
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
    purrr::map_df(rev) # reverses order of data frame

  # parsing the terms from model and variable names
  # outputing a named list--one entry per variable
  mod_list <- parse_terms(x, tidy_model, show_yesno)

  # keeping intercept if requested
  # Note, include = names(stats::model.frame(mod_nlme)) has an error for nlme because it is "reStruct"
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
    dplyr::data_frame(variable = names(mod_list)) %>%
    dplyr::mutate_(
      estimates = ~mod_list,
      var_type = ~ purrr::map_chr(estimates, ~ ifelse(nrow(.x) > 1, "categorical", "continuous")),
      var_label = ~ purrr::map_chr(
        variable, ~ label[[.x]] %||% attr(stats::model.frame(x)[[.x]], "label") %||% .x
      )
    )

  # number obs.
  n <- stats::model.frame(x) %>% nrow()

  # formatting raw results
  model_tbl <-
    raw_results %>%
    dplyr::mutate_(
      # formatting stats
      estimates = ~ purrr::map(estimates, ~ fmt_estimates(.x, beta_fun, pvalue_fun)),
      # adding label
      estimates = ~ purrr::pmap(
        list(var_type, estimates, var_label, variable),
        ~ add_label(..1, ..2, ..3, ..4)
      ),
      N = ~n
    ) %>%
    tidyr::unnest_("estimates")


  # adding default header
  model_tbl <- default_header_fmt_regression(x, model_tbl, exponentiate, conf.level, n)

  # removing intercept term if indicated
  if (intercept == FALSE) {
    # removing the last row if it's called (Intercept)
    model_tbl <- model_tbl %>% dplyr::filter(!(label == "(Intercept)" & dplyr::row_number() == dplyr::n()))
  }

  results <- list()
  results[["model_tbl"]] <-
    model_tbl %>%
    dplyr::select(dplyr::one_of(c(
      "row_type", "var_type", "variable", "label", "N",
      "est", "ll", "ul", "ci", "pvalue_exact", "pvalue", "p_pvalue"
    )))
  results[["n"]] <- n
  results[["model_obj"]] <- x
  results[["inputs"]] <- func_inputs

  # assigning a class of fmt_regression (for special printing in Rmarkdown)
  class(results) <- "fmt_regression"

  return(results)
}

# this function adds the label to the estimates tibble
# for continuous variables this means adding a column with the label.
# But for categorical, we add a row on top with the label
add_label <- function(var_type, estimates, var_label, variable) {
  dplyr::case_when(
    var_type == "continuous" ~ list(estimates %>% dplyr::mutate_(row_type = ~"label", label = ~var_label)),
    var_type == "categorical" ~ list(
      dplyr::bind_rows(
        dplyr::data_frame(row_type = "label", label = var_label),
        estimates %>% dplyr::mutate_(
          row_type = ~"level",
          label = ~ stringr::str_replace(term, stringr::fixed(variable), "")
        )
      )
    )
  ) %>%
    purrr::pluck(1)
}

# little function that will round statistics,
# and add "Ref." for a reference categorical variable
fmt_estimates <- function(x, beta_fun, pvalue_fun) {
  x %>%
    dplyr::mutate_(
      est = ~ ifelse(is.na(estimate), "Ref.", beta_fun(estimate)),
      ll = ~ beta_fun(conf.low),
      ul = ~ beta_fun(conf.high),
      ci = ~ ifelse(is.na(estimate), NA_character_, paste0(ll, ", ", ul)),
      pvalue_exact = ~p.value,
      pvalue = ~ pvalue_fun(p.value),
      p_pvalue = ~ dplyr::case_when(
        is.na(pvalue) ~ NA_character_,
        stringr::str_sub(pvalue, end = 1L) %in% c("<", ">") ~ paste0("p", pvalue),
        TRUE ~ paste0("p=", pvalue)
      )
    )
}


## creates a default header
# adding default header
default_header_fmt_regression <- function(x, model_tbl, exponentiate, conf.level, n) {
  est_name <-
    dplyr::case_when(
      exponentiate == T ~ "exp(Coefficient)",
      exponentiate == F ~ "Coefficient"
    )

  if (class(x)[1] == "glm") {
    est_name <-
      dplyr::case_when(
        exponentiate == T & x$family$family == "binomial" & x$family$link == "logit" ~ "OR",
        exponentiate == T & x$family$family == "poisson" & x$family$link == "log" ~ "IRR",
        TRUE ~ est_name
      )
  }
  else if (class(x)[1] == "coxph") {
    est_name <-
      dplyr::case_when(
        exponentiate == T ~ "HR",
        TRUE ~ est_name
      )
  }

  # adding header row, and appending
  model_tbl <-
    dplyr::bind_rows(
      dplyr::data_frame(
        row_type = "header1",
        label = paste0("N = ", n),
        est = est_name,
        ci = paste(fmt_percent(conf.level, symbol = T), "CI"),
        pvalue = "p-value"
      ),
      model_tbl
    )

  return(model_tbl)
}
