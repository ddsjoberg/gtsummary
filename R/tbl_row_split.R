

tbl_row_split <- function(data, strata, .tbl_fun, ..., .header = "{strata}") {
  set_cli_abort_call()

  # check inputs ---------------------------------------------------------------
  check_not_missing(data)
  check_not_missing(strata)
  check_not_missing(.tbl_fun)
  check_data_frame(data)
  cards::process_selectors(data, strata = {{ strata }})
  if (is_empty(strata) || any(strata %in% c("stat", "stat_name", "n", "N", "p", "strata"))) {
    cli::cli_abort(
      "At least one must be selected in the {.arg strata} argument,
       and cannot be named {.val {c('stat', 'stat_name', 'n', 'N', 'p', 'strata')}}.",
      call = get_cli_abort_call()
    )
  }
  check_string(header)
  .tbl_fun <- rlang::as_function(.tbl_fun, call = get_cli_abort_call())

  # nest data and create tables within each level ------------------------------
  df_tbls <-
    data |>
    tidyr::nest(...tbl_row_split_data... = -all_of(strata)) |>
    dplyr::arrange(dplyr::pick(all_of(strata))) |>
    dplyr::mutate(
      # create the gtsummary table
      ...tbl_row_split_tbls... = map(.data$...tbl_row_split_data..., .f = .tbl_fun, ...)
    )

  # process the headers --------------------------------------------------------
  lst_headers <-
    map(
      seq_along(strata),
      \(i) {
        cards::ard_categorical(data, variables = all_of(strata[i]), by = strata[seq_len(i - 1L)]) |>
          dplyr::select(cards::all_ard_groups(), cards::all_ard_variables(), "stat_name", "stat") |>
          dplyr::mutate(
            strata = .data$variable_level |> unlist(),
            strata = glue::glue()
          )

          cards::rename_ard_columns() |>
          dplyr::select(strata = .env$strata[i], "n", "N", "p") |>
          glue::glue_data(.header) %>%
          {dplyr::tibble("{strata[i]}" := .)}
      }
    ) |>
    set_names(strata)


  # for (i in seq_along(strata)) {
  #   ard <-
  #     cards::ard_categorical(mtcars, variables = all_of(strata[i]), by = strata[seq_len(i - 1L)]) |>
  #     dplyr::select(cards::all_ard_groups(), cards::all_ard_variables(), "stat_name", "stat") |>
  #     cards::rename_ard_columns() |>
  #     dplyr::select(strata = .env$strata[i], "n", "N", "p") %>%
  #     {dplyr::tibble("{strata[i]}" := .)}
  # }
}
