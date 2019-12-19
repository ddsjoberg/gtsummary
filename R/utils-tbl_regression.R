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
#' @param tidy_fun tidy function and arguments passed to it
#' @noRd
#' @keywords internal
#' @author Daniel D. Sjoberg

# Points function to use mixed vs non-mixed version of broom
tidy_wrap <- function(x, exponentiate, conf.level, tidy_fun) {
  mixed_classes <- c("lmerMod", "glmerMod")
  if (is.null(tidy_fun)) {
    if (class(x)[1] %in% mixed_classes) { # can add other classes later. Need exact subclass.
      tidy_bit <- broom.mixed::tidy(
        x,
        exponentiate = exponentiate,
        conf.level = conf.level, conf.int = TRUE, effects = "fixed"
      )
    }

    if (!(class(x)[1] %in% mixed_classes)) {
      tidy_bit <- broom::tidy(
        x,
        exponentiate = exponentiate,
        conf.level = conf.level, conf.int = TRUE
      )
    }

    # deleting scale parameters from survreg objects
    if (class(x)[1] == "survreg") {
      return(
        tidy_bit %>%
          filter(!!parse_expr('term != "Log(scale)"'))
      )
    }
  }

  # if user specified a tidier use it here.
  if (!is.null(tidy_fun)) {
    tryCatch(
      {
        tidy_bit <- do.call(
          tidy_fun,
          args = list(x,
            exponentiate = exponentiate,
            conf.level = conf.level, conf.int = T
          )
        )
      },
      error = function(e) {
        usethis::ui_oops(paste0(
          "There was an error calling {usethis::ui_code('tidy_fun')}.\n",
          "Most likely, this is because the argument passed in {usethis::ui_code('tidy_fun = ')} \n",
          "was\nmisspelled, does not exist, is not compatible with your object, \n",
          "or was missing necessary arguments. See error message below. \n"
        ))
        print(e)
        stop(e)
      }
    )
  }

  # looks for if p.value column is missing and adds NAs if so
  missed <- base::setdiff("p.value", names(tidy_bit))
  tidy_bit[missed] <- NA

  # otherwise returning original output
  return(tidy_bit)
}


#' Function takes a model object and a tidied version (from the broom package)
#' and returns a parsed table of all results with labels and reference rows included
#'
#' @param fit model object
#' @param tidy broom tidy result
#' @inheritParams tbl_regression
#' @noRd
#' @importFrom stringr fixed str_detect
#' @keywords internal

parse_fit <- function(fit, tidy, label, show_single_row) {
  # extracting model frame
  model_frame <- stats::model.frame(fit)

  # add a check on what the model.frame output is and print a message if it's not
  # a data.frame with all vector columns
  if (!(all("data.frame" %in% class(model_frame) && all(purrr::map_lgl(model_frame, ~ rlang::is_vector(.x)))))) {
    message(paste0(
      "Model input `x` has an unexpected format for `model.frame(x)` \n",
      " which may affect `tbl_regression()` output.\n",
      "Expected `model.frame` format is a data frame with vector elements."
    ))
  }

  # all terms in model ---------------------------------------------------------
  # this code looks over the model terms, and extracts a list of each term
  # for interaction terms, the terms are reported separately
  # for example, trtDrug:age will result in trt:Drug and age.
  # counting number of colons in term names
  n_colon <- tidy$term %>% stringr::str_count(pattern = ":")

  # putting unique terms in tibble (to later be matched to the parent variable)
  # parsing terms and separating interactions (sep by :)
  term_match <-
    tibble(
      term = tidy$term %>%
        stringr::str_split_fixed(pattern = ":", n_colon + 1) %>%
        keep(~ . != "") %>%
        discard(~ . == "(Intercept)") %>%
        unique() %>%
        rev(),
      variable = NA_character_
    )

  # match term to variable -----------------------------------------------------
  # cycling over variables and assigning to terms in model
  for (v in (names(model_frame) %>% rev())) {

    # checking character and factor levels
    if (any(class(model_frame[[v]]) %in% c("character", "factor", "logical"))) {
      term_match <-
        term_match %>%
        mutate(
          variable = ifelse(
            stringr::str_starts(fixed(.data$term), fixed(v)) &
              .data$term %in% paste0(v, unique(model_frame[[v]])) &
              is.na(.data$variable),
            v,
            .data$variable
          )
        )
    }
    # checking numeric variable names
    else {
      term_match <-
        term_match %>%
        mutate(
          variable = ifelse(
            .data$term == v & is.na(.data$variable), v, .data$variable
          )
        )
    }
  }

  # if the variable name contains a ':', weird formatting will likely happen
  if (str_detect(fixed(names(model_frame[, -1, drop = FALSE])), fixed(":")) %>% any()) {
    message(glue(
      "Some variable names contain ':', which may cause formatting issues. ",
      "Please rename columns without ':'.\n\n",
      "Variable name(s): {paste0('`', names(model_frame[, -1, drop = FALSE]), '`', collapse = ' ')}"
    ))
  }

  # if the variable value contains a ':', weird formatting will likely happen
  var_values_contain_colon <-
    map_lgl(
      names(model_frame),
      function(x) {
        if (any(class(model_frame[[x]]) %in% c("factor", "character"))) {
          unique(model_frame[[x]]) %>%
            as.character() %>%
            fixed() %>%
            str_detect(fixed(":")) %>%
            any() %>%
            return()
        }
        else {
          return(FALSE)
        }
      }
    ) %>%
    any()
  # if the variable values contains a ':', weird formatting may occur
  if (var_values_contain_colon) {
    warning(glue(
      "Some variable values contain ':', which may cause formatting issues. ",
      "Please re-level values without ':'."
    ))
  }

  # more  var labels -----------------------------------------------------------
  # model.frame() strips variable labels from cox models.  this attempts
  # to grab the labels in another way
  labels_parent_frame <- tryCatch(
    {
      stats::model.frame.default(fit) %>%
        purrr::imap(~ attr(.x, "label"))
    },
    warning = function(w) {
      NULL
    },
    error = function(e) {
      NULL
    }
  )

  # tidy_long ------------------------------------------------------------------
  # this is one line per term, AND interaction terms have one row per variable in the interaction
  tidy_long <-
    tidy %>%
    mutate(
      # making a table with info about variables and levels
      term_id = 1:n(),
      # term_split finds all the variables invovled in interaction terms
      term_split = map(
        .data$term,
        ~ stringr::str_split_fixed(
          .x,
          pattern = ":",
          stringr::str_count(.x, pattern = ":") + 1
        ) %>%
          as.character()
      )
    ) %>%
    unnest(.data$term_split) %>%
    mutate(
      # matching the variable name to each term in the model
      variable = map_chr(
        .data$term_split,
        ~ term_match %>%
          filter(.data$term == .x) %>%
          pull(.data$variable) %>%
          {
            ifelse(.x == "(Intercept)", NA, .)
          }
      ),
      # variable labels
      variable_lbl = map_chr(
        .data$variable,
        ~ label[[.x]] %||% attr(model_frame[[.x]], "label") %||%
          labels_parent_frame[[.x]] %||% .x
      ),
      variable_lbl = ifelse(is.na(.data$variable_lbl) & .data$term == "(Intercept)",
        "(Intercept)",
        .data$variable_lbl
      ),
      # indicating whether each variable is categorical or continuous
      variable_type = map_chr(
        .data$variable,
        ~ case_when(
          any(class(model_frame[[.x]]) %in% c("character", "factor")) ~ "categorical",
          TRUE ~ "continuous"
        )
      ),
      # parsing the term to extract the levels to report
      # if variable is categorical, remove the variable name from term and report level,
      # if variable is not categorical, replace the varname with the label
      level = pmap_chr(
        list(.data$term_split, .data$variable, .data$variable_lbl, .data$variable_type),
        function(term_split, variable, variable_lbl, variable_type) {
          if (variable_type == "continuous") {
            return(variable_lbl)
          }
          stringr::str_remove(term_split, pattern = fixed(variable))
        }
      )
    )

  # tidy_term ------------------------------------------------------------------
  # one line per term in the model
  tidy_term <-
    tidy_long %>%
    group_by(.data$term_id, .data$term) %>%
    mutate(
      # indicating whether obs is an interaction term or not
      interaction = n() > 1,
      # groups are terms that belong to the same variable (or interaction set)
      group = .data$variable %>% paste(collapse = ":"),
      group = ifelse(.data$term == "(Intercept)" & is.na(.data$variable), "(Intercept)", .data$group),
      # the collpase only comes into play when there are interactions present
      group_lbl = .data$variable_lbl %>% paste(collapse = " * "),
      level_lbl = .data$level %>% paste(collapse = " * "),
      # types are continuous, categorical, and interaction
      var_type = ifelse(.data$interaction, "interaction", .data$variable_type),
    ) %>%
    select(-c("term_split", "variable", "variable_lbl", "variable_type", "level")) %>%
    ungroup() %>%
    distinct()

  # tidy_group -----------------------------------------------------------------
  # groups are terms that belong to the same variable (or interaction set)
  tidy_group <-
    tidy_term %>%
    group_by(.data$group, .data$group_lbl, .data$var_type) %>%
    nest() %>%
    mutate(
      # assess how many line to display results one (one or more than one)
      single_row = pmap_lgl(
        list(.data$var_type, .data$group, .data$group_lbl, .data$data),
        function(var_type, group, group_lbl, data) {
          if (var_type == "continuous") {
            return(TRUE)
          }
          if (var_type == "categorical" & group %in% show_single_row) {
            return(TRUE)
          }
          if (var_type == "categorical") {
            return(FALSE)
          }
          # display on single line of it a numeric-numeric interaction
          if (var_type == "interaction") {
            if (nrow(data) > 1) {
              return(FALSE)
            } else if (group_lbl == data$level_lbl) {
              return(TRUE)
            } else {
              return(FALSE)
            }
          }
        }
      )
    )

  # show_single_row check ------------------------------------------------------
  # checking that all variables listed in show_single_row appear in results
  for (i in show_single_row) {
    if (!i %in% tidy_group$group) {
      stop(glue(
        "'{i}' from argument 'show_single_row' is not a variable ",
        "from the model. Select from:\n",
        "{paste(tidy_group$group %>% setdiff('(Intercept)'), collapse = ', ')}"
      ))
    }
  }

  # check that all variables in show_single_row are dichotomous
  bad_show_single_row <-
    tidy_group %>%
    mutate(
      bad_show_single_row = purrr::map2_lgl(
        .data$group, .data$data,
        ~ .x %in% show_single_row && nrow(.y) > 1
      )
    ) %>%
    filter(.data$bad_show_single_row == TRUE) %>%
    pull(.data$group)
  if (length(bad_show_single_row) > 0) {
    stop(glue(
      "'{paste(bad_show_single_row, collapse = \"', '\")}' from argument ",
      "'show_single_row' may only be applied to binary variables."
    ))
  }

  # final touches to result ----------------------------------------------------
  # adding in refernce rows, and header rows for categorical and interaction variables
  result <-
    tidy_group %>%
    mutate(
      table = pmap(
        list(.data$group, .data$group_lbl, .data$single_row, .data$var_type, .data$data),
        ~ parse_final_touches(
          group = ..1,
          group_lbl = ..2,
          single_row = ..3,
          var_type = ..4,
          data = ..5,
          model_frame = model_frame
        )
      )
    )

  # returning final formatted tibble of results
  map_dfr(result$table, ~.x)
}

#' Adds refernce rows, and header rows for categorical and interaction variables
#'
#' @noRd
#' @keywords internal

parse_final_touches <- function(group, group_lbl, single_row, var_type, data, model_frame) {
  # this is for continuous variables, and numeric on numeric interactions
  if (single_row == TRUE) {
    result <- data %>%
      mutate(
        variable = group,
        row_type = "label",
        label = group_lbl,
        row_ref = NA
      )
  }
  # for interaction, do not add reference rows (just a header)
  else if (var_type == "interaction") {
    result <- data %>%
      mutate(
        variable = group,
        row_type = "level",
        label = .data$level_lbl,
        row_ref = NA
      ) %>%
      {
        bind_rows(
          tibble(
            variable = group,
            row_type = "label",
            label = group_lbl
          ), . # levels go below the header
        )
      }
  }
  # adding reference rows AND header row for categorical variables
  else if (var_type == "categorical") {
    result <-
      tibble(
        level_lbl = model_frame[[group]] %>% unique() %>% sort() %>% as.character()
      ) %>%
      left_join(
        data,
        by = "level_lbl"
      ) %>%
      mutate(
        variable = group,
        row_type = "level",
        label = .data$level_lbl,
        row_ref = is.na(.data$estimate)
      ) %>%
      {
        bind_rows(
          tibble(
            variable = group,
            row_type = "label",
            label = group_lbl
          ),
          . # levels go below the header
        )
      }
  }

  # keeping necessary vars and renaming
  result %>%
    mutate(
      N = nrow(model_frame),
      var_type = var_type
    ) %>%
    select(c(
      "variable", "var_type", "row_ref", "row_type", "label", "N",
      "estimate", "conf.low", "conf.high", "p.value"
    ))
}

#' @title Vetted tidy models
#'
#' @description Below is a list of models vetted for use
#' in [tbl_regression] and [tbl_uvregression].  If a model is passed to these
#' functions and the model is not listed below and  a `tidy()` function is
#' not specified in the `tidy_fun=` argument, the model object will be passed
#' to [broom::tidy].
#'
#' \itemize{
#'  \item{[stats::lm]}
#'  \item{[stats::glm]}
#'  \item{[survival::coxph]}
#'  \item{[survival::survreg]}
#'  \item{[lme4::glmer]}
#'  \item{[lme4::lmer]}
#'  \item{[geepack::geeglm]}
#' }
#' @name tidy_vetted
#' @section model support:
#' If [broom::tidy] or [broom.mixed::tidy] support a class of model not listed
#' above, please submit a [GitHub Issue](https://github.com/ddsjoberg/gtsummary/issues).
#' The model can be added to the list of vetted models and unit tests will be
#' put in place to ensure continued support for the model.
#'
#' @section custom tidiers:
#' In some cases you may need to use a custom function to tidy regression model
#' results. A user may pass a custom tidier function to [tbl_regression] and
#' [tbl_uvregression] in the `tidy_fun=` argument.  The function passed, however,
#' must be a proper tidier.  For example, it must return a tibble that is one
#' line per variable in the regression model; it must accept arguments
#' `exponentiate=`, `conf.level=`, and `conf.int=` since these arguments will
#' be passed to the function internally (we recommend including `...` to
#' capture any arguments you may not need). See below for an example where the
#' confidence limits are calculated using Wald's method.
#' @examples
#' my_tidy <- function(x, exponentiate =  FALSE, conf.level = 0.95, ...) {
#'   dplyr::bind_cols(
#'     broom::tidy(x, exponentiate = exponentiate, conf.int = FALSE),
#'     broom::confint_tidy(x, func = stats::confint.default, conf.level = conf.level)
#'   )
#' }
NULL
