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
  lst_cat_summary_fns <- .categorical_summary_functions(c("n", "p"))
  lst_all_fmt_fns <-
    .categorical_summary_functions(c("N_obs", "N_miss", "N_nonmiss", "p_miss", "p_nonmiss"))

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
          digits[[variable]] <- rep_named(statistic[[variable]], digits[[variable]])
        }

        # convert integers to a proper function
        digits[[variable]] <- .convert_integer_to_fmt_fn(digits[[variable]])

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
            list(.guess_continuous_summary_digits(data[[variable]]))
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
      # if an integer is passed for a percentage, process stat with style_percent()
      if (stat_name %in% c("p", "p_miss", "p_nonmiss", "p_unweighted")) {
        return(label_style_percent(digits = value))
      }
      # otherwise, use style_number() to style number
      return(label_style_number(digits = value))
    }
  )
}

.guess_continuous_summary_digits <- function(x) {
  # if all missing, return 0
  if (all(is.na(x))) {
    return(label_style_number(digits = 0L))
  }

  # if class is integer, then round everything to nearest integer
  if (inherits(x, "integer")) {
    return(label_style_number(digits = 0L))
  }

  # if it's a date or time, then convert the result to character
  if (is_date_time(x)) {
    return(as.character)
  }

  # otherwise guess the number of digits to use based on the spread
  # calculate the spread of the variable
  tryCatch(
    {
      var_spread <-
        stats::quantile(x, probs = c(0.95), na.rm = TRUE) -
        stats::quantile(x, probs = c(0.05), na.rm = TRUE)

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
}

.categorical_summary_functions <-
  function(statistics = c(
             "
                          N", "N_obs", "N_miss", "N_nonmiss", "n_unweighted", "N_unweighted",
             "p_miss", "p_nonmiss", "p_unweighted"
           )) {
    lst_defaults <-
      c(
        c("n", "N", "N_obs", "N_miss", "N_nonmiss", "n_unweighted", "N_unweighted") |>
          intersect(statistics) |>
          rep_named(list(label_style_number())),
        c("p", "p_miss", "p_nonmiss", "p_unweighted") |>
          intersect(statistics) |>
          rep_named(list(label_style_percent()))
      )

    lst_defaults
  }
