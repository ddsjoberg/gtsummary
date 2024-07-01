#' Wide summary table bridge
#'
#' @description
#' Bridge function for converting `tbl_wide_summary()` (and similar) cards to basic gtsummary objects.
#' All bridge functions begin with prefix `brdg_*()`.
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
#'   ard_continuous(trial, variables = c(age, marker)),
#'   ard_attributes(trial, variables = c(age, marker))
#' ) |>
#'   brdg_wide_summary(
#'     variables = c("age", "marker"),
#'     statistic = list(age = c("{mean}", "{sd}"), marker = c("{mean}", "{sd}")),
#'     type = list(age = "continuous", marker = "continuous")
#'   )
brdg_wide_summary <- function(cards, variables, statistic, type) {
  set_cli_abort_call()

  # add gts info to the cards table --------------------------------------------
  lst_stat_labels <-
    cards[c("stat_name", "stat_label")] |>
    unique() |>
    deframe() |>
    as.list()
  df_stat_info <-
    dplyr::tibble(
      statistic = statistic[[1]],
      stat_name = map(statistic, ~.extract_glue_elements(.x))
    ) |>
    dplyr::mutate(
      statistic_header = statistic |>
        map_chr(
          ~ glue::glue_data(.x = lst_stat_labels, .x) |>
            str_replace_all(pattern = "%%", replacement = "%", fixed = TRUE)
        ) %>%
        {paste0("**", ., "**")}, # styler: off
      stat_id = dplyr::row_number(),
      gts_column = paste0("stat_", .data$stat_id)
    ) |>
    tidyr::unnest(cols = "stat_name")

  cards <- cards |>
    dplyr::left_join(
      df_stat_info[c("stat_name", "stat_id", "gts_column")],
      by = "stat_name"
    )

  # build the table body pieces with bridge functions and stack them -----------
  table_body <-
    map(
      unique(df_stat_info$stat_id),
      function(i) {
        dplyr::left_join(
          dplyr::tibble(
            variable = variables,
            var_type = type[.data$variable] |> unlist() |> unname()
          ),
          dplyr::bind_rows(
            pier_summary_continuous(
              cards = cards |> dplyr::filter(.data$stat_id == i | .data$context == "attributes"),
              variables = .get_variables_by_type(type, type = "continuous"),
              statistic = statistic |> map(getElement, i)
            ),
            pier_summary_categorical(
              cards = cards |> dplyr::filter(.data$stat_id == i | .data$context == "attributes"),
              variables = .get_variables_by_type(type, type = "categorical"),
              statistic = statistic |> map(getElement, i)
            ),
            pier_summary_dichotomous(
              cards = cards |> dplyr::filter(.data$stat_id == i | .data$context == "attributes"),
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
    modify_header(!!!deframe(unique(df_stat_info[c("gts_column", "statistic_header")])))

  x |>
    structure(class = "gtsummary")
}
