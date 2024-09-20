#' Create and Stack Hierarchical ARDs
#'
#' @export
#'
#' @examples
#' ard_stack_hierarchical(
#'   data = cards::ADAE,
#'   hierarchies = c(SEX, AESEV),
#'   by = TRTA,
#'   id = USUBJID,
#'   include = everything(),
#'   statistic = all_categorical() ~ "{n} ({p})",
#'   denominator = cards::ADSL
#' )
#'
ard_stack_hierarchical <- function(data,
                                   hierarchies,
                                   by = NULL,
                                   id = NULL,
                                   include,
                                   statistic,
                                   denominator,
                                   .overall = FALSE,
                                   .missing = FALSE,
                                   .attributes = FALSE,
                                   .total_n = FALSE,
                                   .shuffle = FALSE) {
  # if id provided, then check that denominator also provided
  if (!is_empty(id)) {
    check_data_frame(
      denominator,
      message = "A {.cls data.frame} must be passed in argument {.arg denominator} when argument {.arg id} is supplied."
    )
  }

  # evaluate tidyselect
  cards::process_selectors(data, hierarchies = {{ hierarchies }}, id = {{ id }}, by = {{ by }})
  cards::process_selectors(data[hierarchies], include = {{ include }})

  which_include <- which(hierarchies %in% include)

  # get top level
  if (1 %in% which_include) {
    data_top <- data

    # count by id
    if (!is_empty(id)) {
      data_top <- data_top |>
        slice(1L, .by = c(id, by, tail(hierarchies, 1)))
    }

    ard_lvls <- list(
      cards::ard_hierarchical(
        data = data_top,
        variables = tail(hierarchies, 1),
        by = all_of(by),
        statistic = statistic,
        denominator = denominator,
        id = all_of(id)
      )
    )
    which_include <- which_include[-1]
  } else {
    ard_lvls <- list()
  }

  # get inner levels
  for (i in which_include) {
    data_inner <- data

    # count by id
    if (!is_empty(id)) {
      data_inner <- data_inner |>
        slice(1L, .by = c(id, by, hierarchies[c(1:(i - 1), length(hierarchies))]))
    }

    ard_lvls <- c(
      ard_lvls,
      list(
        cards::ard_hierarchical(
          data = data_inner,
          variables = hierarchies[c(1:(i - 1), length(hierarchies))],
          by = all_of(by),
          statistic = statistic,
          denominator = denominator,
          id = all_of(id)
        )
      )
    )
  }

  ard_lvls |>
    dplyr::bind_rows() |>
    cards::tidy_ard_column_order() |>
    cards::tidy_ard_row_order()
}
