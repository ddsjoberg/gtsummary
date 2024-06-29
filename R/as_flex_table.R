#' Convert gtsummary object to a flextable object
#'
#' @description Function converts a gtsummary object to a flextable object.
#' A user can use this function if they wish to add customized formatting
#' available via the flextable functions. The flextable output is particularly
#' useful when combined with R markdown with Word output, since the gt package
#' does not support Word.
#'
#' @details
#' The `as_flex_table()` function supports bold and italic markdown syntax in column headers
#' and spanning headers (`'**'` and `'_'` only).
#' Text wrapped in double stars (`'**bold**'`) will be made bold, and text between single
#' underscores (`'_italic_'`) will be made italic.
#' No other markdown syntax is supported and the double-star and underscore cannot be combined.
#' To further style your table, you may convert the table to flextable with
#' `as_flex_table()`, then utilize any of the flextable functions.
#'
#' @param ... Not used
#' @inheritParams as_gt
#' @inheritParams as_tibble.gtsummary
#' @export
#' @return A 'flextable' object
#'
#' @author Daniel D. Sjoberg
#' @examplesIf gtsummary:::is_pkg_installed("flextable", reference_pkg = "gtsummary")
#' trial |>
#'   select(trt, age, grade) |>
#'   tbl_summary(by = trt) |>
#'   add_p() |>
#'   as_flex_table()
as_flex_table <- function(x, include = everything(), return_calls = FALSE, ...) {
  set_cli_abort_call()
  check_pkg_installed("flextable", reference_pkg = "gtsummary")

  # deprecated arguments -------------------------------------------------------
  dots <- rlang::dots_list(...)
  if (!is.null(dots$strip_md_bold)) {
    lifecycle::deprecate_stop("1.6.0", "gtsummary::as_flex_table(strip_md_bold=)")
  }

  # process inputs -------------------------------------------------------------
  check_class(x, "gtsummary")

  # running pre-conversion function, if present --------------------------------
  x <- do.call(get_theme_element("pkgwide-fun:pre_conversion", default = identity), list(x))

  # converting row specifications to row numbers, and removing old cmds --------
  x <- .table_styling_expr_to_row_number(x)

  # creating list of flextable calls -------------------------------------------
  flextable_calls <- table_styling_to_flextable_calls(x = x)

  # adding user-specified calls ------------------------------------------------
  insert_expr_after <- get_theme_element("as_flex_table-lst:addl_cmds")
  flextable_calls <-
    reduce(
      .x = seq_along(insert_expr_after),
      .f = function(x, y) {
        add_expr_after(
          calls = x,
          add_after = names(insert_expr_after[y]),
          expr = insert_expr_after[[y]],
          new_name = paste0("user_added", y)
        )
      },
      .init = flextable_calls
    )

  # converting to character vector ---------------------------------------------
  cards::process_selectors(
    data = vec_to_df(names(flextable_calls)),
    include = {{ include }}
  )

  # return calls, if requested -------------------------------------------------
  if (return_calls == TRUE) {
    return(flextable_calls[include])
  }

  # taking each kable function call, concatenating them with %>% separating them
  .eval_list_of_exprs(flextable_calls[include])
}

# creating flextable calls from table_styling ----------------------------------
table_styling_to_flextable_calls <- function(x, ...) {
  # adding id number for columns not hidden
  x$table_styling$header <-
    x$table_styling$header |>
    dplyr::group_by(.data$hide) |>
    dplyr::mutate(id = ifelse(.data$hide == FALSE, dplyr::row_number(), NA)) |>
    dplyr::ungroup()

  # tibble ---------------------------------------------------------------------
  # flextable doesn't use the markdown language `__` or `**`
  # to bold and italicize text, so removing them here
  flextable_calls <- table_styling_to_tibble_calls(x, col_labels = FALSE)
  flextable_calls$tab_style_bold <-
    flextable_calls$tab_style_italic <-
    flextable_calls$fmt_missing <- NULL

  # flextable ------------------------------------------------------------------
  flextable_calls[["flextable"]] <- expr(flextable::flextable())

  # compose_header -------------------------------------------------------------
  col_labels <-
    x$table_styling$header |>
    dplyr::filter(.data$hide == FALSE)

  flextable_calls[["compose_header"]] <-
    .chr_with_md_to_ft_compose(x = col_labels$label, j = col_labels$column)

  # set_caption ----------------------------------------------------------------
  if (!is.null(x$table_styling$caption)) {
    flextable_calls[["set_caption"]] <- expr(
      flextable::set_caption(caption = !!x$table_styling$caption)
    )
  }

  # add_header_row -------------------------------------------------------------
  # this is the spanning rows
  any_spanning_header <- any(!is.na(x$table_styling$header$spanning_header))
  if (any_spanning_header == FALSE) {
    flextable_calls[["add_header_row"]] <- list()
  } else {
    df_header0 <-
      x$table_styling$header |>
      dplyr::filter(.data$hide == FALSE) |>
      dplyr::select("spanning_header") |>
      dplyr::mutate(
        spanning_header = ifelse(is.na(.data$spanning_header),
                                 " ", .data$spanning_header
        ),
        spanning_header_id = dplyr::row_number()
      )
    # assigning an ID for each spanning header group
    for (i in seq(2, nrow(df_header0))) {
      if (df_header0$spanning_header[i] == df_header0$spanning_header[i - 1]) {
        df_header0$spanning_header_id[i] <- df_header0$spanning_header_id[i - 1]
      }
    }

    df_header <-
      df_header0 |>
      dplyr::group_by(.data$spanning_header_id) |>
      dplyr::mutate(width = dplyr::n()) |>
      dplyr::distinct() |>
      dplyr::ungroup() |>
      dplyr::mutate(
        column_id = map2(.data$spanning_header_id, .data$width, ~ seq(.x, .x + .y - 1L, by = 1L))
      )

    flextable_calls[["add_header_row"]] <- list(
      expr(
        # add the header row with the spanning headers
        flextable::add_header_row(
          values = !!df_header$spanning_header,
          colwidths = !!df_header$width
        )
      )
    )

    flextable_calls[["compose_header_row"]] <-
      .chr_with_md_to_ft_compose(
        x = df_header$spanning_header,
        j = df_header$column_id
      )
  }

  # align ----------------------------------------------------------------------
  df_align <-
    x$table_styling$header |>
    dplyr::filter(.data$hide == FALSE) |>
    dplyr::select("id", "align") |>
    dplyr::group_by(.data$align) |>
    tidyr::nest() |>
    dplyr::ungroup()

  flextable_calls[["align"]] <- map2(
    df_align$align, df_align$data,
    ~ expr(flextable::align(align = !!.x, j = !!.y$id, part = "all"))
  )

  # padding --------------------------------------------------------------------
  df_padding <-
    x$table_styling$header |>
    dplyr::select("id", "column") |>
    dplyr::inner_join(
      x$table_styling$indent,
      by = "column"
    )

  flextable_calls[["padding"]] <- map(
    seq_len(nrow(df_padding)),
    ~ expr(flextable::padding(
      i = !!df_padding$row_numbers[[.x]],
      j = !!df_padding$id[[.x]],
      padding.left = !!(df_padding$n_spaces[[.x]] * 15 / 4)
    ))
  )

  # fontsize -------------------------------------------------------------------
  flextable_calls[["fontsize"]] <- list(
    expr(flextable::fontsize(part = "header", size = 11))
  )

  # autofit --------------------------------------------------------------------
  flextable_calls[["autofit"]] <- expr(flextable::autofit())

  # footnote -------------------------------------------------------------------
  header_i_index <- ifelse(any_spanning_header == TRUE, 2L, 1L)

  df_footnote <-
    .number_footnotes(x) |>
    dplyr::inner_join(
      x$table_styling$header |>
        dplyr::select("column", column_id = "id"),
      by = "column"
    ) |>
    dplyr::mutate(
      row_numbers =
        ifelse(.data$tab_location == "header",
               header_i_index,
               .data$row_numbers
        )
    ) |>
    dplyr::select(
      "footnote_id", "footnote", "tab_location",
      "row_numbers", "column_id"
    ) |>
    tidyr::nest(location_ids = c("row_numbers", "column_id")) %>%
    dplyr::mutate(
      row_numbers = map(.data$location_ids, ~ getElement(.x, "row_numbers") |> unique()),
      column_id = map(.data$location_ids, ~ getElement(.x, "column_id") |> unique())
    )

  flextable_calls[["footnote"]] <-
    map(
      seq_len(nrow(df_footnote)),
      ~ expr(
        flextable::footnote(
          i = !!df_footnote$row_numbers[[.x]],
          j = !!df_footnote$column_id[[.x]],
          value = flextable::as_paragraph(!!df_footnote$footnote[[.x]]),
          part = !!df_footnote$tab_location[[.x]],
          ref_symbols = !!df_footnote$footnote_id[[.x]]
        )
      )
    )

  # fmt_missing ----------------------------------------------------------------
  df_fmt_missing <-
    x$table_styling$fmt_missing |>
    dplyr::inner_join(
      x$table_styling$header |>
        dplyr::select("column", column_id = "id"),
      by = "column"
    ) |>
    dplyr::select("symbol", "row_numbers", "column_id") %>%
    tidyr::nest(location_ids = "column_id") %>%
    dplyr::mutate(
      column_id = map(.data$location_ids, ~ getElement(.x, "column_id") |> unique())
    )

  flextable_calls[["fmt_missing"]] <-
    map(
      seq_len(nrow(df_fmt_missing)),
      ~ expr(
        flextable::colformat_char(
          i = !!df_fmt_missing$row_numbers[[.x]],
          j = !!df_fmt_missing$column_id[[.x]],
          na_str = !!df_fmt_missing$symbol[[.x]]
        )
      )
    )

  # bold -----------------------------------------------------------------------
  df_bold <-
    x$table_styling$text_format |>
    dplyr::filter(.data$format_type == "bold") |>
    dplyr::inner_join(
      x$table_styling$header |>
        dplyr::select("column", column_id = "id"),
      by = "column"
    ) |>
    dplyr::select("format_type", "row_numbers", "column_id")

  flextable_calls[["bold"]] <-
    map(
      seq_len(nrow(df_bold)),
      ~ expr(flextable::bold(
        i = !!df_bold$row_numbers[[.x]],
        j = !!df_bold$column_id[[.x]],
        part = "body"
      ))
    )

  # italic ---------------------------------------------------------------------
  df_italic <-
    x$table_styling$text_format |>
    dplyr::filter(.data$format_type == "italic") |>
    dplyr::inner_join(
      x$table_styling$header |>
        dplyr::select("column", column_id = "id"),
      by = "column"
    ) |>
    dplyr::select("format_type", "row_numbers", "column_id")

  flextable_calls[["italic"]] <-
    map(
      seq_len(nrow(df_italic)),
      ~ expr(flextable::italic(
        i = !!df_italic$row_numbers[[.x]],
        j = !!df_italic$column_id[[.x]],
        part = "body"
      ))
    )

  # source note ----------------------------------------------------------------
  # in flextable, this is just a footnote associated without column or symbol
  if (!is.null(x$table_styling$source_note)) {
    flextable_calls[["source_note"]] <-
      expr(
        flextable::add_footer_lines(value = flextable::as_paragraph(!!x$table_styling$source_note))
      )
  }

  # border ---------------------------------------------------------------------
  flextable_calls[["border"]] <-
    list(
      # all header rows get top and bottom borders
      expr(
        flextable::border(
          border.top = officer::fp_border(width = 1),
          border.bottom = officer::fp_border(width = 1),
          part = "header"
        )
      ),
      # last row of table body gets bottom border
      expr(
        flextable::border(
          i = !!nrow(x$table_body),
          border.bottom = officer::fp_border(width = 1),
          part = "body"
        )
      )
    )

  # horizontal_line_above ------------------------------------------------------
  if (!is.null(x$table_styling$horizontal_line_above)) {
    row_number <-
      eval_tidy(x$table_styling$horizontal_line_above, data = x$table_body) |>
      which()
    flextable_calls[["horizontal_line"]] <-
      expr(
        flextable::border(
          i = !!row_number,
          border.top = officer::fp_border(width = 1),
          part = "body"
        )
      )
  }

  # padding for header ---------------------------------------------------------
  # setting all header rows to the same padding
  flextable_calls[["padding_header"]] <-
    list(
      expr(flextable::padding(
        padding.bottom = 2,
        padding.top = 2,
        part = "header"
      ))
    )

  # valign ---------------------------------------------------------------------
  # when there are line breaks within cells, ensuring variable label is top-left
  flextable_calls[["valign"]] <-
    list(
      expr(
        flextable::valign(valign = "top", part = "body")
      )
    )

  flextable_calls
}


.chr_with_md_to_ft_compose <- function(x, j, i = 1L, part = "header", break_chr = "@@@@@@@@@@@&@@@@@@@@@") {
  map2(
    .x = x, .y = j,
    .f = function(x, j) {
      x <-
        str_replace_all(
          x,
          pattern = "\\*\\*(.*?)\\*\\*",
          replacement = paste0(break_chr, "\\*\\*", "\\1", "\\*\\*", break_chr)
        )

      x <-
        str_replace_all(
          x,
          pattern = "\\_(.*?)\\_",
          replacement = paste0(break_chr, "\\_", "\\1", "\\_", break_chr)
        )

      str_split(x, pattern = break_chr) %>%
        unlist() %>%
        discard(~ . == "") %>%
        map(
          function(.x) {
            if (startsWith(.x, "**") && endsWith(.x, "**")) {
              .x <-
                str_replace_all(.x, pattern = "\\*\\*(.*?)\\*\\*", replacement = "\\1") %>%
                {expr(flextable::as_b(!!.))} # styler: off
            } else if (startsWith(.x, "_") && endsWith(.x, "_")) {
              .x <-
                str_replace_all(.x, pattern = "\\_(.*?)\\_", replacement = "\\1") %>%
                {expr(flextable::as_i(!!.))} # styler: off
            }

            return(.x)
          }
        ) %>%
        {switch(!rlang::is_empty(.), .) %||% ""} %>% # styler: off
        {expr(flextable::compose(part = !!part, i = !!i, j = !!j, value = flextable::as_paragraph(!!!.)))} # styler: off
    }
  )
}
