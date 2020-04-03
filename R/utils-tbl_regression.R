#' Tidies regression object based on class
#'
#' The `tidy_wrap()` function has two primary functions.  First, using either
#' `broom::tidy`
#' the regression model object is converted into a data frame. It then adjusts the
#' output for use in the rest of \code{\link{tbl_regression}}.
#'
#' The output of `broom::tidy` will often include additional information
#' that will not be included in a printed table from `tbl_regression()`
#' (e.g. scale parameters, random effects, etc.).  This
#' simple helper function deletes extraneous rows from the output.
#' It also adds a column of NAs if the model does not calculate p.values, since
#' the rest of `tbl_regression()` depends on having a column called p.value.
#'
#' @param x regression model object
#' @param exponentiate logical argument passed directly to `broom::tidy`
#' @param conf.level confidence level passed directly to `broom::tidy`
#' @param tidy_fun tidy function and arguments passed to it
#' @noRd
#' @keywords internal
#' @author Daniel D. Sjoberg

# Points function to use mixed vs non-mixed version of broom
tidy_wrap <- function(x, exponentiate, conf.level, tidy_fun) {
  mixed_classes <- c("lmerMod", "glmerMod")
  if (is.null(tidy_fun)) {
    if (inherits(x, mixed_classes)) { # can add other classes later. Need exact subclass.
      tryCatch({
        tidy_bit <- broom.mixed::tidy(
        x,
        exponentiate = exponentiate,
        conf.level = conf.level, conf.int = TRUE, effects = "fixed"
      )
      },
      error = function(e) {
        usethis::ui_oops(paste0(
          "There was an error calling {usethis::ui_code('broom.mixed::tidy(x, conf.int = TRUE, effects = \"fixed\")')},\n",
          "which is required for gtsummary to print the model summary.\n",
          "See error message below. \n"
        ))
        stop(as.character(e), call. = FALSE)
      }
      )
    }

    if (!inherits(x, mixed_classes)) {
      tryCatch({
        tidy_bit <- broom::tidy(
        x,
        exponentiate = exponentiate,
        conf.level = conf.level, conf.int = TRUE
      )
      },
      error = function(e) {
        usethis::ui_oops(paste0(
          "There was an error calling {usethis::ui_code('broom::tidy(x, conf.int = TRUE)')},\n",
          "which is required for gtsummary to print the model summary.\n",
          "See error message below. \n"
        ))
        stop(as.character(e), call. = FALSE)
      }
      )
    }

    # deleting scale parameters from survreg objects
    if (inherits(x, "survreg")) {
      tidy_bit <- tidy_bit %>%
        filter(.data$term != "Log(scale)")
    }
  }

  # if user specified a tidier use it here.
  if (!is.null(tidy_fun)) {
    tryCatch({
        tidy_bit <- do.call(
          tidy_fun,
          args = list(x,
            exponentiate = exponentiate,
            conf.level = conf.level, conf.int = TRUE
          )
        )
      },
      error = function(e) {
        usethis::ui_oops(paste0(
          "There was an error calling {usethis::ui_code('tidy_fun')}.\n",
          "Most likely, this is because the argument passed in {usethis::ui_code('tidy_fun=')} \n",
          "was\nmisspelled, does not exist, is not compatible with your object, \n",
          "or was missing necessary arguments. See error message below. \n"
        ))
        stop(as.character(e), call. = FALSE)
      }
    )
  }

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
#' @keywords internal

parse_fit <- function(fit, tidy, label, show_single_row) {
  # enquos ---------------------------------------------------------------------
  show_single_row <- rlang::enquo(show_single_row)

  # extracting model frame
  model_frame <- stats::model.frame(fit)

  # add a check on what the model.frame output is and print a message if it's not
  # a data.frame with all vector columns
  if (!(all(inherits(model_frame, "data.frame") && all(purrr::map_lgl(model_frame, ~ rlang::is_vector(.x)))))) {
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

  # more var labels -----------------------------------------------------------
  # model.frame() strips variable labels from cox models.  this attempts
  # to grab the labels in another way
  labels_parent_frame <- tryCatch({
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
        ~ stats::na.omit(term_match) %>%
          filter(.data$term == .x) %>%
          pull(.data$variable) %>%
          {ifelse(length(.) == 0, .x, .)} # for unmatched terms, using term as variable
      )
    )

  # adding variable labels -----------------------------------------------------
  label <- tidyselect_to_list(.data = vctr_2_tibble(unique(tidy_long$variable)),
                              x = label, arg_name = "label")
  # # all sepcifed labels must be a string of length 1
  if (!every(label, ~ rlang::is_string(.x))) {
    stop("Each `label` specified must be a string of length 1.", call. = FALSE)
  }

  tidy_long_lbl <-
    tidy_long %>%
    mutate(
      # variable labels
      variable_lbl = map_chr(
        .data$variable,
        ~ label[[.x]] %||% attr(model_frame[[.x]], "label") %||%
          labels_parent_frame[[.x]] %||% .x
      ),
      # variable_lbl = ifelse(is.na(.data$variable_lbl) & .data$term == "(Intercept)",
      #   "(Intercept)",
      #   .data$variable_lbl
      # ),
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
    tidy_long_lbl %>%
    group_by(.data$term_id, .data$term) %>%
    mutate(
      # indicating whether obs is an interaction term or not
      interaction = n() > 1,
      # groups are terms that belong to the same variable (or interaction set)
      group = .data$variable %>% paste(collapse = ":"),
      # group = ifelse(.data$term == "(Intercept)" & is.na(.data$variable), "(Intercept)", .data$group),
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
  show_single_row <-
    var_input_to_string(data = vctr_2_tibble(unique(tidy_term$group)),
                        arg_name = "show_single_row",
                        select_input = !!show_single_row)

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
      "'show_single_row' may only be applied to dichotomous variables."
    ), call. = FALSE)
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
  df_result <- map_dfr(result$table, ~.x)

  # need to attach the label and show_single_row evaluated lists to save out later
  attr(df_result, "label") <- label
  attr(df_result, "show_single_row") <- show_single_row

  df_result
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
  # vars to keep (in order)
  cols_to_keep <-
    c("variable", "var_type", "row_ref", "row_type", "label", "N",
    "estimate", "conf.low", "conf.high", "p.value") %>%
    intersect(c(names(result), "N", "var_type"))
  result %>%
    mutate(
      N = nrow(model_frame),
      var_type = var_type
    ) %>%
    select(all_of(cols_to_keep))
}

#' Takes a vector and transforms to data frame with those column names
#'
#' This will be used for tidyselect to used those functions to select from
#' the vector
#' @noRd
#' @keywords internal
vctr_2_tibble <- function(x) {
  n <- length(x)
  matrix(ncol = n) %>%
    tibble::as_tibble(.name_repair = "minimal") %>%
    rlang::set_names(x) %>%
    dplyr::slice(0)
}

