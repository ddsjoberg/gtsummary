#' Summarize continuous variable
#'
#' Summarize a continuous variable by one or more categorical variables
#'
#' @param variable (`string`)\cr
#'   A single variable name of the continuous variable being summarized.
#' @param by (`string`)\cr
#'   A single variable name of the stratifying variable.
#' @param include (`character`)\cr
#'   Character vector of the categorical variables to
#' @param statistic ([`formula-list-selector`][syntax])\cr
#'   Specifies summary statistics to display for each variable.  The default is
#'   `everything() ~ "{median} ({p25}, {p75})"`.
#' @inheritParams card_summary
#'
#' @return a gtsummary table of class `"card_summary"`
#' @export
#'
#' @examples
#' library(cards)
#'
#' bind_ard(
#'   # the primary ARD with the results
#'   ard_continuous(
#'     trial,
#'     # the order variables are passed here is important.
#'     # 'trt' is the column stratifying variable and needs to be listed first.
#'     by = c(trt, grade),
#'     variables = age
#'   ) ,
#'   # add univariate trt tabulation
#'   ard_categorical(
#'     trial,
#'     variables = trt
#'   ),
#'   # add missing and attributes ARD
#'   ard_missing(
#'     trial,
#'     by = c(trt, grade),
#'     variables = age
#'   ),
#'   ard_attributes(
#'     trial,
#'     variables = c(trt, grade, age)
#'   )
#' ) |>
#'   card_continuous(by = "trt", variable = "age", include = "grade")
#'
#' bind_ard(
#'   # the primary ARD with the results
#'   ard_continuous(trial, by = grade, variables = age),
#'   # add missing and attributes ARD
#'   ard_missing(trial, by = grade, variables = age),
#'   ard_attributes(trial, variables = c(grade, age))
#' ) |>
#'   card_continuous(variable = "age", include = "grade")
card_continuous <- function(cards, variable, include, by = NULL, statistic = everything() ~ "{median} ({p25}, {p75})") {
  set_cli_abort_call()
  check_not_missing(cards)
  check_not_missing(variable)
  check_not_missing(include)
  include <- enquo(include)

  # save processed function inputs ---------------------------------------------
  card_continuous_inputs <- as.list(environment())
  call <- match.call()

  # prepare the cards object for `card_summary()` ------------------------------
  cards <- cards |>
    dplyr::group_by(.data$context) |>
    dplyr::group_map(
      \(.x, .y) {
        if (.y$context %in% "attributes" || identical(.x$variable[1], by)) {
          return(dplyr::bind_cols(.x, .y))
        }

        .x |>
          dplyr::select(-cards::all_ard_variables()) %>%
          {case_switch(
            all(c("group2", "group2_level") %in% names(.)) ~
              dplyr::rename(., variable = "group2", variable_level = "group2_level"),
            .default = dplyr::rename(., variable = "group1", variable_level = "group1_level")
          )} |>
          dplyr::bind_cols(.y) |>
          dplyr::mutate(
            context = ifelse(.y$context %in% "missing", "missing", "categorical")
          )
      }
    ) |>
    dplyr::bind_rows() |>
    # dropping group2 and group2_level since they are all NA or NULL
    dplyr::select(-c(\(x) all(is.na(x)), \(x) is.null(unlist(x)))) |>
    structure(class = class(cards))


  # Create table via `card_summary()` ------------------------------------------
  result <-
    card_summary(
      cards = cards,
      statistic = statistic,
      type = everything() ~ "categorical",
      include = !!include,
      missing = "no"
    ) |>
    structure(class = c("tbl_continuous", "gtsummary"))

  # prepend the footnote with information about the variable -------------------
  result$table_styling$footnote$footnote <-
    paste0(
      cards |>
        dplyr::filter(.data$context == "attributes", .data$variable == .env$variable, .data$stat_name == "label") |>
        dplyr::pull("stat") |>
        unlist(),
      ": ",
      result$table_styling$footnote$footnote
    )

  # add other information to the returned object
  result$inputs <- card_continuous_inputs
  result$call_list <- list(card_continuous = call)

  result |>
    structure(class = c("tbl_continuous", "gtsummary"))
}
