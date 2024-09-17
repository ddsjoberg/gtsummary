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
  # evaluate tidyselect
  cards::process_selectors(data, hierarchies = {{ hierarchies }}, id = {{ id }}, by = {{ by }})
  cards::process_selectors(data[hierarchies], include = {{ include }})

  which_include <- which(head(hierarchies, -1) %in% include)

  # get top level
  if (hierarchies[1] %in% include) {
    ard_lvls <- list(
      cards::ard_hierarchical(
        data = data,
        variables = tail(hierarchies, 1),
        by = all_of(by),
        statistic = statistic,
        denominator = denominator,
        id = all_of(id)
      )
    )
  } else {
    ard_lvls <- list()
  }

  # get inner levels
  for (i in which_include) {
    ard_lvls <- c(
      ard_lvls,
      list(
        cards::ard_hierarchical(
          data = data,
          variables = hierarchies[c(1:i, length(hierarchies))],
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
