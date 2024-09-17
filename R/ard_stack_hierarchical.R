#' @export
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
  ard_lvls <- list()

  for (i in seq_along(hierarchies)) {
    ard_lvls <- c(
      ard_lvls,
      list(
        cards::ard_hierarchical(
          data = data,
          variables = hierarchies[1:i],
          by = all_of(by),
          statistic = statistic,
          denominator = denominator,
          id = all_of(id)
        ),
        if (i != 1 && i != length(hierarchies)) {
          cards::ard_hierarchical(
            data = data,
            variables = hierarchies[-i],
            by = all_of(by),
            statistic = statistic,
            denominator = denominator,
            id = all_of(id)
          )
        } else {
          NULL
        }

      )
    )
  }

  ard_lvls |>
    dplyr::bind_rows() |>
    cards::tidy_ard_column_order() |>
    cards::tidy_ard_row_order()
}
