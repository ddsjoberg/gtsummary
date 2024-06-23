#' Continuous Summary Table Bridges
#'
#' @description
#' Bridge function for converting `tbl_continuous()` cards to basic gtsummary objects.
#' This bridge function converts the 'cards' object to a format suitable to
#' pass to `brdg_summary()`: no `pier_*()` functions required.
#'
#' @inheritParams brdg_summary
#' @inheritParams tbl_continuous
#'
#' @return a gtsummary object
#' @export
#'
#' @examples
#' library(cards)
#'
#' bind_ard(
#'   # the primary ARD with the results
#'   ard_continuous(trial, by = grade, variables = age),
#'   # add missing and attributes ARD
#'   ard_missing(trial, by = grade, variables = age),
#'   ard_attributes(trial, variables = c(grade, age))
#' ) |>
#'   # adding the column name
#'   dplyr::mutate(
#'     gts_column =
#'       ifelse(!context %in% "attributes", "stat_0", NA_character_)
#'   ) |>
#'   brdg_continuous(
#'     variable = "age",
#'     include = "grade",
#'     statistic = list(grade = "{median} ({p25}, {p75})")
#'  ) |>
#'  as_tibble()
brdg_continuous <- function(cards, by = NULL, statistic, include, variable) {
  set_cli_abort_call()

  # prepare the cards object for `brdg_summary()` ------------------------------
  cards <- .cards_continuous_to_summary(cards, by)

  # Create table via `brdg_summary()` ------------------------------------------
  brdg_summary(
    cards = cards,
    statistic = statistic,
    by = by,
    type = rep_named(include, list("categorical")),
    variables = include,
    missing = "no"
  )
}

.cards_continuous_to_summary <- function(cards, by) {
  cards |>
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
}
