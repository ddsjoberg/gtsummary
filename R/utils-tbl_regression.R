#' Tidies regression object based on class
#'
#' The `tidy_wrap()` function has two primary functions.  First, using either
#' `broom::tidy` or `broom.mixed::tidy` (depending on model input class)
#' the regression model object is converted into a data frame. It then adjusts the
#' output for use in the rest of \code{\link{tbl_regression}}.
#'
#' The output of `broom::tidy` or `broom.mixed::tidy` will often include additional information
#' that will not be included in a printed table from `tbl_regression()`
#' (e.g. scale parameters, random effects, etc.).  This
#' simple helper function deletes extraneous rows from the output.
#' It also adds a column of NAs if the model does not calculate p.values, since
#' the rest of `tbl_regression()` depends on having a column called p.value.
#'
#' @param x regression model object
#' @param exponentiate logical argument passed directly to `broom::tidy`
#' or `broom.mixed::tidy`.
#' @param conf.level confidence level passed directly to `broom::tidy`
#' or `broom.mixed::tidy`.
#' @keywords internal

# Points function to use mixed vs non-mixed version of broom
tidy_wrap <- function(x, exponentiate, conf.level) {
  mixed_classes <- c("lmerMod", "glmerMod", "nlme")
  if (class(x)[1] %in% mixed_classes) { # can add other classes later. Need exact subclass.
    tidy_bit <- broom.mixed::tidy(
      x,
      exponentiate = exponentiate,
      conf.level = conf.level, conf.int = T, effects = "fixed"
    )
  }

  if (!(class(x)[1] %in% mixed_classes)) {
    tidy_bit <- broom::tidy(
      x,
      exponentiate = exponentiate,
      conf.level = conf.level, conf.int = T
    )
  }

  # deleting scale parameters from survreg objects
  if (class(x)[1] == "survreg") {
    return(
      tidy_bit %>%
        filter_(~ term != "Log(scale)")
    )
  }

  # looks for if p.value column is missing and adds NAs if so
  missed <- base::setdiff("p.value", names(tidy_bit))
  tidy_bit[missed] <- NA

  # otherwise returning original output
  return(tidy_bit)
}


#' Convert a regression model object to a parsed list matching model terms to variable names
#'
#' With the `parse_terms()` function, each term in the tidied model is parsed and
#' matched with a variable name.
#'
#' @param x regression model object
#' @param tidy_model tidied model output from tidy_wrap function
#' @param show_yesno categorical and factor variables that are `c("No", "Yes")` or
#' `c("no", "yes")` default to dichotomous printing. To force both levels to be shown
#' @return list with one entry for each variable, each element are the terms from
#' the regression model.For categorical variables, the reference level has been added.
#' @keywords internal

# this function takes in the model object,
# parses the model terms (returned from broom::tidy or broom.mixed::tidy)
# to match the input variables with the term names
# a named list is returned.  The names
# are the variables that went into the model
# and list items are the results from the model associated with
# each variable (in tibble format)
parse_terms <- function(x, tidy_model, show_yesno) {
  # reversing names to search through the covariates first
  # varnames <- stats::model.frame(x) %>% names() %>% rev()

  # initializing emply list to store results in
  tidy_list <- list()

  # if intercept is in model, extracting it
  if (last(tidy_model$term) == "(Intercept)") {
    tidy_list[["(Intercept)"]] <- tidy_model[nrow(tidy_model), ]
    tidy_model <- tidy_model[-(nrow(tidy_model)), ]
  }

  varnames <- stats::model.frame(x) %>% names() %>% rev()

  # looping over every covariate to assign to term in tidy table
  for (v in varnames) {
    # if all varnames have been assigned, then work is done
    if (nrow(tidy_model) == 0) break

    # idenfiying variables to print on a single row (no yes)
    dichotomous <-
      !(v %in% show_yesno) &
      (
        (map_lgl(
          list(c("No", "Yes", NA), c("no", "yes", NA), c("NO", "YES", NA)),
          ~ is.character(v) & setdiff(stats::model.frame(x)[[v]], .x) %>% length() == 0
        ) %>% any()) |
          (is.factor(v) & attr(stats::model.frame(x)[[v]], "levels") %>% toupper() %>% identical(c("NO", "YES")))
      )

    # matching if not factor (i.e. single line)
    if (v == tidy_model$term[1] | dichotomous == TRUE) {
      # extracting the row for the list, and deleting the row from tidy_model
      tidy_list[[v]] <- tidy_model[1, ]

      tidy_model <- tidy_model[-1, ]
      next
    }
    # matching for factor variables
    else {
      # making dataframe of the terms
      fct_terms <-
        tibble(
          level = stats::model.frame(x)[[v]] %>% unique()
        ) %>%
        mutate_(
          term = ~ paste0(v, level)
        ) %>%
        arrange_("level") %>%
        select("term")

      # checking that these new terms match any terms in model
      # if not, skipping this step. when there is a random effect
      # we would expect a skip here

      if (!(fct_terms$term %in% tidy_model$term) %>% any()) {
        next
      }

      # extracting terms into the list
      tidy_list[[v]] <-
        fct_terms %>%
        left_join(tidy_model[1:(nrow(fct_terms) - 1), ], by = "term")

      tidy_model <- tidy_model[-(1:(nrow(fct_terms) - 1)), ]
    }
  }

  return(tidy_list %>% rev())
}


# this function adds the label to the estimates tibble
# for continuous variables this means adding a column with the label.
# for categorical, we add a row on top with the label
add_label <- function(var_type, estimates, var_label, variable) {
  case_when(
    var_type == "continuous" ~ list(estimates %>% mutate_(row_type = ~"label", label = ~var_label)),
    var_type == "categorical" ~ list(
      bind_rows(
        tibble(row_type = "label", label = var_label),
        estimates %>% mutate_(
          row_type = ~"level",
          label = ~ stringr::str_replace(term, stringr::fixed(variable), "")
        )
      )
    )
  ) %>%
    pluck(1)
}

