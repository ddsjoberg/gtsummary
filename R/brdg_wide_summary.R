#' Wide summary table bridge
#'
#' @description
#' Bridge function for converting `tbl_wide_summary()` (and similar) cards to basic gtsummary objects.
#' All bridge functions begin with prefix `brdg_*()`.
#'
#' The ARD data frame passed to this function must include a `"gts_column"`
#' column, which assigns each statistic to the column it will populate,
#' e.g. `"stat_1"` for statistics in the first element of each `statistic`
#' vector, `"stat_2"` for the second, etc.
#'
#' @inheritParams brdg_summary
#' @inheritParams tbl_summary
#'
#' @return a gtsummary object
#' @export
#'
#' @examples
#' library(cards)
#'
#' bind_ard(
#'   ard_summary(trial, variables = c(age, marker)),
#'   ard_attributes(trial, variables = c(age, marker))
#' ) |>
#'   # this column is used by the `pier_*()` functions
#'   dplyr::mutate(
#'     gts_column = dplyr::case_when(
#'       stat_name == "mean" ~ "stat_1",
#'       stat_name == "sd" ~ "stat_2"
#'     )
#'   ) |>
#'   brdg_wide_summary(
#'     variables = c("age", "marker"),
#'     statistic = list(age = c("{mean}", "{sd}"), marker = c("{mean}", "{sd}")),
#'     type = list(age = "continuous", marker = "continuous")
#'   )
brdg_wide_summary <- function(cards, variables, statistic, type) {
  set_cli_abort_call()

  # construct the column headers for each statistic ----------------------------
  lst_stat_labels <-
    cards[c("stat_name", "stat_label")] |>
    unique() |>
    deframe() |>
    as.list()
  df_stat_info <-
    dplyr::tibble(
      statistic = statistic[[1]],
      gts_column = paste0("stat_", seq_along(.data$statistic))
    ) |>
    dplyr::mutate(
      statistic_header = statistic |>
        map_chr(
          ~ glue::glue_data(.x = lst_stat_labels, .x) |>
            str_replace_all(pattern = "%%", replacement = "%", fixed = TRUE)
        ) %>%
        {paste0("**", ., "**")} # styler: off
    )

  # build the table body pieces with bridge functions and stack them -----------
  table_body <-
    map(
      seq_len(nrow(df_stat_info)),
      function(i) {
        gts_column_i <- df_stat_info$gts_column[i]
        cards_i <- cards |>
          dplyr::filter(.data$gts_column %in% .env$gts_column_i | .data$context == "attributes")
        dplyr::left_join(
          dplyr::tibble(
            variable = variables,
            var_type = type[.data$variable] |> unlist() |> unname()
          ),
          dplyr::bind_rows(
            pier_summary_continuous(
              cards = cards_i,
              variables = .get_variables_by_type(type, type = "continuous"),
              statistic = statistic |> map(getElement, i)
            ),
            pier_summary_categorical(
              cards = cards_i,
              variables = .get_variables_by_type(type, type = "categorical"),
              statistic = statistic |> map(getElement, i)
            ),
            pier_summary_dichotomous(
              cards = cards_i,
              variables = .get_variables_by_type(type, type = "dichotomous"),
              statistic = statistic |> map(getElement, i)
            )
          ),
          by = "variable"
        )
      }
    ) %>%
    {suppressMessages(reduce(., dplyr::left_join))}


  # construct default table_styling --------------------------------------------
  x <- .create_gtsummary_object(table_body)

  # add info to x$table_styling$header for dynamic headers ---------------------
  x <- .add_table_styling_stats(x, cards = cards, by = NULL)

  # adding styling -------------------------------------------------------------
  x <- x |>
    # add header to label column and add default indentation
    modify_table_styling(
      columns = "label",
      label = glue("**{translate_string('Characteristic')}**"),
      rows = .data$row_type %in% c("level", "missing"),
      indent = 4L
    ) |>
    modify_header(!!!deframe(df_stat_info[c("gts_column", "statistic_header")]))

  x |>
    structure(class = "gtsummary")
}
