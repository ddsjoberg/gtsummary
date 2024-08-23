bootstrap_df_from_cards <- function(x) {
  # first get all the varname columns
  varname_cols <- x |>
    dplyr::select(cards::all_ard_groups("names"), cards::all_ard_variables("names")) |>
    names()

  # stack all the names and levels into a single data frame
  df_long <-
    varname_cols |>
    map(
      ~ x |>
        dplyr::select(any_of(c(.x, paste0(.x, "_level")))) |>
        dplyr::rename(name = any_of(.x), level = any_of(paste0(.x, "_level")))
    ) |>
    dplyr::bind_rows() |>
    dplyr::filter(!is.na(.data$name), !.data$name %in% "..ard_total_n..")

  if (!"level" %in% df_long) {
    df_long$level <- list(NA)
  }

  df_long |>
    dplyr::arrange(.data$name, .data$level) |>
    dplyr::slice(1L, .by = "name") |>
    tidyr::pivot_wider(names_from = "name", values_from = "level") |>
    dplyr::mutate(across(everything(), ~map(.x, \(x) x %||% NA) |> unlist())) |>
    dplyr::select(any_of(unique(df_long$name)), everything())
}

