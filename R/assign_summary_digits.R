#' Assign Default Digits
#'
#' Used to assign the default formatting for variables summarized with
#' `tbl_summary()`.
#'
#' @param data (`data.frame`)\cr
#'   a data frame
#' @param statistic (`named list`)\cr
#'   a named list; notably, _not_ a [`formula-list-selector`][syntax]
#' @param type (`named list`)\cr
#'   a named list; notably, _not_ a [`formula-list-selector`][syntax]
#' @param digits (`named list`)\cr
#'   a named list; notably, _not_ a [`formula-list-selector`][syntax].
#'   Default is `NULL`
#'
#' @return a named list
#' @export
#'
#' @examples
#' assign_summary_digits(
#'   mtcars,
#'   statistic = list(mpg = "{mean}"),
#'   type = list(mpg = "continuous")
#' )
assign_summary_digits <- function(data, statistic, type, digits = NULL) {
  set_cli_abort_call()
  # stats returned for all variables
  lst_cat_summary_fns <-
    case_switch(
      inherits(data, "data.frame") ~ .categorical_summary_functions(c("n", "p")),
      inherits(data, "survey.design") ~
        .categorical_summary_functions(c("n", "N", "p",
                                         "n_unweighted", "N_unweighted", "p_unweighted",
                                         "p.std.error", "deff"))
    )

  lst_all_fmt_fns <-
    case_switch(
      inherits(data, "data.frame") ~
        .categorical_summary_functions(c("N_obs", "N_miss", "N_nonmiss", "p_miss", "p_nonmiss")),
      inherits(data, "survey.design") ~
        .categorical_summary_functions(c("N_obs", "N_miss", "N_nonmiss", "p_miss", "p_nonmiss",
                                         "N_obs_unweighted", "N_miss_unweighted", "N_nonmiss_unweighted",
                                         "p_miss_unweighted", "p_nonmiss_unweighted"))
    )


  # extract the statistics
  statistic <- lapply(statistic, function(x) .extract_glue_elements(x) |> unlist())

  lapply(
    names(statistic),
    function(variable) {
      # if user passed digits AND they've specified every statistic, use the passed value
      # otherwise, we need to calculate the defaults, and later we can update with the pieces the user passed
      if (!is.null(digits[[variable]])) {
        # if a scalar or vector passed, convert it to a list
        if (!is.list(digits[[variable]]) && is_vector(digits[[variable]])) {
          digits[[variable]] <- as.list(digits[[variable]])
        }

        # if user-passed value is not named, repeat the passed value to the length of 'statistic'
        if (!is_named(digits[[variable]])) {
          if (!is_function(digits[[variable]])) digits[[variable]] <- rep_named(statistic[[variable]], digits[[variable]])
          else digits[[variable]] <- rep_named(statistic[[variable]], digits[variable])
        }

        # convert integers to a proper function
        digits[[variable]] <- .convert_integer_to_fmt_fn(digits[[variable]])

        # check value is a function
        if (!is_list(digits[[variable]]) || some(digits[[variable]], \(.x) !is_function(.x))) {
          cli::cli_abort(
            c("Error in {.arg digits} argument for variable {.val {variable}},",
              i = "Passed values must be either a {.cls function} or {.cls integer}."),
            call = get_cli_abort_call()
          )
        }

        # if the passed value fully specifies the formatting for each 'statistic',
        # then return it. Otherwise, the remaining stat will be filled below
        if (setequal(statistic[[variable]], names(digits[[variable]]))) {
          return(lst_all_fmt_fns |> utils::modifyList(digits[[variable]]))
        }
      }

      if (type[[variable]] %in% c("categorical", "dichotomous")) {
        return(
          c(lst_cat_summary_fns, lst_all_fmt_fns) |>
            utils::modifyList(digits[[variable]] %||% list())
        )
      }

      if (type[[variable]] %in% c("continuous", "continuous2")) {
        return(
          rep_named(
            statistic[[variable]],
            list(.guess_continuous_summary_digits(data, variable))
          ) |>
            utils::modifyList(lst_all_fmt_fns) |>
            utils::modifyList(digits[[variable]] %||% list())
        )
      }
    }
  ) |>
    stats::setNames(names(statistic))
}

.convert_integer_to_fmt_fn <- function(x) {
  imap(
    x,
    function(value, stat_name) {
      # if not an integer, simply return the value
      if (!is_integerish(value)) {
        return(value)
      }
      # if an integer is passed for a percentage, process stat with label_style_number()
      if (stat_name %in% c("p", "p_miss", "p_nonmiss", "p_unweighted")) {
        return(label_style_number(digits = value, scale = 100))
      }
      # otherwise, use style_number() to style number
      return(label_style_number(digits = value))
    }
  )
}

.guess_continuous_summary_digits <- function(data, variable) {
  # if all missing, return 0
  if (all(is.na(as.data.frame(data)[[variable]]))) {
    return(label_style_number(digits = 0L))
  }

  # if class is integer, then round everything to nearest integer
  if (inherits(as.data.frame(data)[[variable]], "integer")) {
    return(label_style_number(digits = 0L))
  }

  # if it's a date or time, then convert the result to character
  if (is_date_time(as.data.frame(data)[[variable]])) {
    return(as.character)
  }

  # otherwise guess the number of digits to use based on the spread
  # calculate the spread of the variable
  # styler: off
  tryCatch({
    var_spread <-
      case_switch(
        inherits(data, "data.frame") ~
          stats::quantile(data[[variable]], probs = c(0.95), na.rm = TRUE) -
          stats::quantile(data[[variable]], probs = c(0.05), na.rm = TRUE),
        inherits(data, "survey.design") ~
          cardx::ard_continuous(data, variables = all_of(variable), statistic = ~ c("p5", "p95")) |>
          dplyr::pull("stat") |>
          reduce(\(.x, .y) .y - .x)
      )

      label_style_number(
        digits =
          dplyr::case_when(
            var_spread < 0.01 ~ 4L,
            var_spread >= 0.01 & var_spread < 0.1 ~ 3L,
            var_spread >= 0.1 & var_spread < 10 ~ 2L,
            var_spread >= 10 & var_spread < 20 ~ 1L,
            var_spread >= 20 ~ 0L
          )
      )
    },
    error = function(e) 0L
  )
  # styler: on
}

.categorical_summary_functions <- function(statistics = c("N", "N_obs", "N_miss", "N_nonmiss",
                                                          "n_unweighted", "N_unweighted",
                                                          "p_miss", "p_nonmiss", "p_unweighted")) {
  lst_defaults <-
    c(
      c("n", "N", "N_obs", "N_miss", "N_nonmiss",
        "N_obs_unweighted", "n_unweighted", "N_unweighted",
        "N_miss_unweighted", "N_nonmiss_unweighted") |>
        intersect(statistics) |>
        rep_named(list(label_style_number())),
      c("p", "p_miss", "p_nonmiss", "p_unweighted",
        "p_miss_unweighted", "p_nonmiss_unweighted") |>
        intersect(statistics) |>
        rep_named(list(get_theme_element("tbl_summary-fn:percent_fun", default = label_style_percent()))),
      c("p.std.error", "deff") |>
        intersect(statistics) |>
        rep_named(list(label_style_sigfig(digits = 3)))
    )

  lst_defaults
}
