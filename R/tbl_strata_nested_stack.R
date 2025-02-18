#' Stratified Nested Stacking
#'
#' This function stratifies your data frame, builds gtsummary tables, and
#' stacks the resulting tables in a nested style. The underlying functionality
#' is similar to `tbl_strata()`, except the resulting tables are nested or indented
#' within each group.
#'
#' @inheritParams tbl_strata
#' @param data (`data.frame`)\cr
#'   a data frame
#' @param row_header (`string`)\cr
#'   string indicating the row headers that appear in the table.
#'   The argument uses `glue::glue()` syntax to insert values into the
#'   row headers. Elements available to insert are `strata`, `n`, `N` and `p`.
#'   The `strata` element is the variable level of the strata variables.
#'   Default is `'{strata}'`.
#'
#' @returns a stacked 'gtsummary' table
#' @export
#'
#' @examples
#' # Example 1 ----------------------------------
#' tbl_strata_nested_stack(
#'   trial,
#'   strata = trt,
#'   .tbl_fun = ~ .x |>
#'     tbl_summary(include = c(age, grade), missing = "no") |>
#'     modify_header(all_stat_cols() ~ "**Summary Statistics**")
#' )
#'
#' # Example 2 ----------------------------------
#' tbl_strata_nested_stack(
#'   trial,
#'   strata = trt,
#'   .tbl_fun = ~ .x |>
#'     tbl_summary(include = c(age, grade), missing = "no") |>
#'     modify_header(all_stat_cols() ~ "**Summary Statistics**"),
#'   row_header = "{strata}, n={n}"
#' ) |>
#'   # bold the row headers; print `x$table_body` to see hidden columns
#'   modify_bold(columns = "label", rows = tbl_indent_id1 > 0)
tbl_strata_nested_stack <- function(data, strata, .tbl_fun, ..., row_header = "{strata}", quiet = FALSE) {
  set_cli_abort_call()

  # check inputs ---------------------------------------------------------------
  check_not_missing(data)
  check_not_missing(strata)
  check_not_missing(.tbl_fun)
  check_data_frame(data)
  cards::process_selectors(data, strata = {{ strata }})
  func_inputs <- as.list(environment())

  if (is_empty(strata) || any(strata %in% c("stat", "stat_name", "n", "N", "p", "strata"))) {
    cli::cli_abort(
      "At least one column must be selected in the {.arg strata} argument,
       and columns cannot be named {.val {c('stat', 'stat_name', 'n', 'N', 'p', 'strata')}}.",
      call = get_cli_abort_call()
    )
  }
  check_string(row_header)
  .tbl_fun <- rlang::as_function(.tbl_fun, call = get_cli_abort_call())

  # nest data and create tables within each level ------------------------------
  tbls <-
    cards::nest_for_ard(data, strata = strata) |>
    dplyr::mutate(
      tbl =
        map(
          .data$data,
          ~cards::eval_capture_conditions(expr(.tbl_fun(.x))) |>
            # print errors, if they occured
            cards::captured_condition_as_error(
              message = c("The following {type} occured while building a table:", x = "{condition}")
            )
        )
    ) |>
    dplyr::pull("tbl")

  # process the headers --------------------------------------------------------
  lst_headers <-
    map(
      seq_along(strata),
      \(i) {
        # for factors, remove unobserved rows
        case_switch(
          is.factor(data[[strata[i]]]) ~ dplyr::mutate(data, "{strata[i]}" := factor(.data[[strata[i]]])),
          .default = data
        ) |>
        cards::ard_categorical(variables = all_of(strata[i]), strata = any_of(strata[seq_len(i - 1L)])) |>
          dplyr::select(cards::all_ard_groups(), cards::all_ard_variables(), "stat_name", "stat") |>
          dplyr::arrange(dplyr::pick(c(cards::all_ard_groups(), cards::all_ard_variables()))) |>
          tidyr::pivot_wider(
            id_cols = c(cards::all_ard_groups(), cards::all_ard_variables()),
            values_from = "stat",
            names_from = "stat_name",
            values_fn = unlist
          ) |>
          dplyr::mutate(
            strata = .data$variable_level |> unlist(),
            "{strata[i]}_strata" := glue::glue(row_header)
          ) |>
          cards::rename_ard_columns() |>
          dplyr::select(any_of(strata), all_of(glue::glue("{strata[i]}_strata")))
      }
    ) |>
    set_names(strata)

  df_headers <- lst_headers |>
    reduce(.f = \(.x, .y) dplyr::left_join(.x, .y, by = intersect(names(.x), names(.y)))) |>
    dplyr::select(-all_of(strata)) |>
    dplyr::rename_with(.fn = ~str_remove(.x, "_strata$"))

  for (i in seq_along(strata[-1])) {
    df_headers <- df_headers |>
      dplyr::mutate(
        .by = all_of(strata[seq_len(i)]),
        "{strata[i]}" := ifelse(dplyr::row_number() == 1, .data[[strata[i]]], NA)
      )
  }

  first_non_hidden_col <- .first_unhidden_column(tbls[[1]])
  lst_df_headers <-
    map(
      seq_len(nrow(df_headers)),
      ~df_headers[.x, ] |>
        set_names(seq_along(strata)) |>
        tidyr::pivot_longer(
          cols = everything(),
          names_to = "tbl_indent_id1",
          values_to = first_non_hidden_col
        ) |>
        tidyr::drop_na() |>
        dplyr::mutate(tbl_indent_id1 = as.integer(.data$tbl_indent_id1))
    )

  # before combining the headers with tbls, doing some checks ------------------
  .checks_for_nesting_stack(tbls)

  # adding the headers, indenting, and stacking --------------------------------
  # add headers with their associated `tbl_indent_id1`
  for (i in seq_len(nrow(df_headers))) {
    # indent the innermost table
    tbls[[i]]$table_styling$indent$n_spaces <-
      tbls[[i]]$table_styling$indent$n_spaces + length(strata) * 4L

    # add nesting header rows
    tbls[[i]]$table_body <-
      dplyr::bind_rows(
        lst_df_headers[[i]],
        tbls[[i]]$table_body |> dplyr::mutate(tbl_indent_id1 = 0L)
      )
  }

  # stack the tbls
  tbl <- tbl_stack(tbls = tbls, quiet = quiet)

  # cycle over the depth and indenting nesting headers
  for (d in seq_len(nrow(df_headers) - 1L)) {
    tbl <- tbl |>
      modify_column_indent(
        columns = all_of(first_non_hidden_col),
        rows = !!expr(.data$tbl_indent_id1 == !!d),
        indent = (d - 1L) * 4L
      )
  }

  # return table ---------------------------------------------------------------
  tbl$call_list <- list(tbl_row_split = match.call())
  tbl$inputs = list(tbl_row_split = func_inputs)
  tbl$tbls <- NULL
  tbl |>
    structure(class = c("tbl_row_split", "gtsummary"))
}


.checks_for_nesting_stack <- function(tbls) {
  walk(
    seq_along(tbls),
    \(i) {
      if (.first_unhidden_column(tbls[[i]]) != .first_unhidden_column(tbls[[1]])) {
        cli::cli_abort(
          c("The first column shown in each table must be the same.",
            "i" = "The first table prints the {.val {identity(.first_unhidden_column(tbls[[1]]))}}
                   in the first position, and table {.val {i}} has {.val {identity(.first_unhidden_column(tbls[[i]]))}}"),
          call = get_cli_abort_call()
        )
      }
      if (!is.character(tbls[[i]]$table_body[[.first_unhidden_column(tbls[[i]])]])) {
        cli::cli_abort(
          "The first column printed must be {.cls character},
           which is not the case for table {.val {i}} and column {.val {identity(.first_unhidden_column(tbls[[i]]))}}",
          call = get_cli_abort_call()
        )
      }
      if ("tbl_indent_id1" %in% names(tbls[[i]]$table_body)) {
        cli::cli_abort(
          "The {.fun tbl_strata_nested_stack} function can only be run once on a table.
           One of the tables already contains a column named {.val tbl_indent_id1}
           indicating the function was previously executed.",
          call = get_cli_abort_call()
        )
      }
    }
  )
}
