#' Convert gtsummary object to a flextable object
#'
#' Function converts a gtsummary object to a flextable object.
#' A user can use this function if they wish to add customized formatting
#' available via the flextable functions. The flextable output is particularly
#' useful when combined with R markdown with Word output, since the gt package
#' does not support Word.
#'
#' @section Details:
#' The `as_flex_table()` functions converts the gtsummary object to a flextable,
#' and prints it with the following styling functions.
#' 1. [flextable::flextable()]
#' 1. [flextable::set_header_labels()] to set column labels
#' 1. [flextable::add_header_row()], if applicable, to set spanning column header
#' 1. [flextable::align()] to set column alignment
#' 1. [flextable::padding()] to indent variable levels
#' 1. [flextable::fontsize()] to set font size
#' 1. [flextable::autofit()] to estimate the column widths
#' 1. [flextable::footnote()] to add table footnotes and source notes
#' 1. [flextable::bold()] to bold cells in data frame
#' 1. [flextable::italic()] to italicize cells in data frame
#' 1. [flextable::border()] to set all border widths to 1
#' 1. [flextable::padding()] to set consistent header padding
#' 1. [flextable::valign()] to ensure label column is top-left justified
#'
#' Any one of these commands may be omitted using the `include=` argument.
#'
#' Pro tip: Use the [flextable::width()] function for exacting control over
#' column width after calling [as_flex_table()].
#' @inheritParams as_gt
#' @inheritParams as_tibble.gtsummary
#' @param strip_md_bold When TRUE, all double asterisk (markdown language for
#' bold weight) in column labels and spanning headers are removed.
#' Default is TRUE
#' @export
#' @return A {flextable} object
#' @family gtsummary output types
#' @author Daniel D. Sjoberg
#' @examples
#' as_flex_table_ex1 <-
#'   trial %>%
#'   select(trt, age, grade) %>%
#'   tbl_summary(by = trt) %>%
#'   add_p() %>%
#'   as_flex_table()
#' @section Example Output:
#' \if{html}{Example 1}
#'
#' \if{html}{\figure{as_flex_table_ex1.png}{options: width=60\%}}
as_flex_table <- function(x, include = everything(), return_calls = FALSE,
                          strip_md_bold = TRUE) {
  # checking flextable installation --------------------------------------------
  assert_package("flextable", "as_flex_table")

  # stripping markdown asterisk ------------------------------------------------
  if (strip_md_bold == TRUE) {
    x$table_header <-
      x$table_header %>%
      mutate_at(
        vars(.data$label, .data$spanning_header),
        ~str_replace_all(., pattern = fixed("**"), replacement = fixed(""))
      )
  }

  # creating list of flextable calls -------------------------------------------
  flextable_calls <- table_header_to_flextable_calls(x = x)

  # adding user-specified calls ------------------------------------------------
  insert_expr_after <- get_theme_element("as_flex_table-lst:addl_cmds")
  flextable_calls <-
    purrr::reduce(
      .x = seq_along(insert_expr_after),
      .f = function(x, y) add_expr_after(calls = x,
                                         add_after = names(insert_expr_after[y]),
                                         expr = insert_expr_after[[y]],
                                         new_name = paste0("user_added", y)),
      .init = flextable_calls
    )

  # converting to charcter vector ----------------------------------------------
  include <- var_input_to_string(data = vctr_2_tibble(names(flextable_calls)),
                                 select_input = !!rlang::enquo(include))

  # return calls, if requested -------------------------------------------------
  if (return_calls == TRUE) return(flextable_calls[include])

  # taking each kable function call, concatenating them with %>% separating them
  flextable_calls[include] %>%
    # removing NULL elements
    unlist() %>%
    compact() %>%
    # concatenating expressions with %>% between each of them
    reduce(function(x, y) expr(!!x %>% !!y)) %>%
    # evaluating expressions
    eval()
}

# creating flextable calls from table_header -----------------------------------
table_header_to_flextable_calls <- function(x, ...) {

  # adding id number for columns not hidden
   table_header <-
      x$table_header %>%
      group_by(.data$hide) %>%
      mutate(id = ifelse(.data$hide == FALSE, dplyr::row_number(), NA)) %>%
      ungroup()

  # tibble ---------------------------------------------------------------------
  # flextable doesn't use the markdown language `__` or `**`
  # to bold and italicize text, so removing them here
  flextable_calls <-
    as_tibble(x, return_calls = TRUE,
              include = -c("cols_label", "tab_style_bold", "tab_style_italic"))

  # flextable ------------------------------------------------------------------
  flextable_calls[["flextable"]] <- expr(flextable::flextable())

  # set_header_labels ----------------------------------------------------------
  col_labels <-
    table_header %>%
    filter(.data$hide == FALSE) %>%
    {set_names(as.list(.[["label"]]), .[["column"]])}

  flextable_calls[["set_header_labels"]] <- expr(
    flextable::set_header_labels(!!!col_labels)
  )

  # add_header_row -------------------------------------------------------------
  # this is the spanning rows
  any_spanning_header <- sum(!is.na(table_header$spanning_header)) > 0
  if (any_spanning_header == FALSE) flextable_calls[["add_header_row"]] <- list()
  else {
    df_header0 <-
      table_header %>%
      filter(.data$hide == FALSE) %>%
      select(.data$spanning_header) %>%
      mutate(spanning_header = ifelse(is.na(.data$spanning_header),
                                      " ", .data$spanning_header),
             spanning_header_id = dplyr::row_number())
    # assigning an ID for each spanning header group
    for (i in seq(2, nrow(df_header0))) {
      if(df_header0$spanning_header[i] == df_header0$spanning_header[i-1]) {
        df_header0$spanning_header_id[i] <- df_header0$spanning_header_id[i-1]
      }
    }

    df_header <-
      df_header0 %>%
      group_by(.data$spanning_header_id) %>%
      mutate(width = n()) %>%
      distinct() %>%
      ungroup()

    flextable_calls[["add_header_row"]] <- list(
      expr(
        # add the header row with the spanning headers
        flextable::add_header_row(
          values = !!df_header$spanning_header,
          colwidths = !!df_header$width
        )
      )
    )
  }

  # align ----------------------------------------------------------------------
  df_align <-
    table_header %>%
    filter(.data$hide == FALSE) %>%
    select(.data$id, .data$align) %>%
    group_by(.data$align) %>%
    nest() %>%
    ungroup()

  flextable_calls[["align"]] <- map2(
    df_align$align, df_align$data,
    ~expr(flextable::align(align = !!.x, j = !!.y$id, part = "all"))
  )

  # padding --------------------------------------------------------------------
  df_padding <-
    table_header %>%
    filter(!is.na(.data$indent)) %>%
    select(.data$id, .data$column, .data$indent) %>%
    mutate(
      i_index = map(
        .data$indent,
        ~rlang::eval_tidy(rlang::parse_expr(.x), x$table_body) %>% which()
      )
    )

  flextable_calls[["padding"]] <- map2(
    df_padding$id, df_padding$i_index,
    ~expr(flextable::padding(i = !!.y, j = !!.x, padding.left = 15))
  )

  # fontsize -------------------------------------------------------------------
  flextable_calls[["fontsize"]] <- list(
    expr(flextable::fontsize(part = "header", size = 11))
  )

  # autofit --------------------------------------------------------------------
  flextable_calls[["autofit"]] <- expr(flextable::autofit())

  # footnote -------------------------------------------------------------------
  i_index <- ifelse(any_spanning_header == TRUE, 2L, 1L)

  footnote_abbrev <-
    table_header %>%
    select(.data$id, .data$footnote_abbrev) %>%
    filter(!is.na(.data$footnote_abbrev)) %>%
    group_by(.data$footnote_abbrev) %>%
    nest() %>%
    ungroup() %>%
    mutate(footnote = paste(.data$footnote_abbrev, collapse = ", ")) %>%
    unnest(cols = .data$data) %>%
    select(-.data$footnote_abbrev) %>%
    group_by(.data$footnote) %>%
    nest() %>%
    ungroup()

  df_footnote <-
    table_header %>%
    select(.data$id, .data$footnote) %>%
    filter(!is.na(.data$footnote)) %>%
    group_by(.data$footnote) %>%
    nest() %>%
    ungroup() %>%
    bind_rows(footnote_abbrev) %>%
    mutate(
      j_index = map(.data$data, ~.x$id),
      min_id = purrr::map_int(.data$j_index,~min(.x))
    ) %>%
    arrange(.data$min_id) %>%
    mutate(row_number = dplyr::row_number())

  flextable_calls[["footnote"]] <- pmap(
    list(df_footnote$j_index, df_footnote$footnote, df_footnote$row_number),
    ~expr(
      flextable::footnote(
        i = !!i_index, j = !!..1,
        value = flextable::as_paragraph(!!..2),
        part = "header", ref_symbols = !!..3
      )
    )
  )

  # fmt_missing_emdash ---------------------------------------------------------
  df_na_emdash <-
    table_header %>%
    filter(!is.na(.data$missing_emdash)) %>%
    select(.data$id, .data$column, .data$missing_emdash) %>%
    mutate(
      i_index = map(
        .data$missing_emdash,
        ~rlang::eval_tidy(rlang::parse_expr(.x), x$table_body) %>% which()
      )
    )

  flextable_calls[["fmt_missing_emdash"]] <-
    map2(
      df_na_emdash$i_index, df_na_emdash$id,
      ~expr(
        flextable::colformat_char(j = !!.y, i = !!.x,
                                  na_str = !!get_theme_element("tbl_regression-str:ref_row_text", default = "\U2014"))
        )
    )

  # bold -----------------------------------------------------------------------
  df_bold <-
    table_header %>%
    filter(!is.na(.data$bold)) %>%
    select(.data$id, .data$column, .data$bold) %>%
    mutate(
      i_index = map(
        .data$bold,
        ~rlang::eval_tidy(rlang::parse_expr(.x), x$table_body) %>% which()
      )
    )

  flextable_calls[["bold"]] <- map2(
    df_bold$id, df_bold$i_index,
    ~expr(flextable::bold(i = !!.y, j = !!.x, part = "body"))
  )

  # italic ---------------------------------------------------------------------
  df_italic <-
    table_header %>%
    filter(!is.na(.data$italic)) %>%
    select(.data$id, .data$column, .data$italic) %>%
    mutate(
      i_index = map(
        .data$italic,
        ~rlang::eval_tidy(rlang::parse_expr(.x), x$table_body) %>% which()
      )
    )

  flextable_calls[["italic"]] <- map2(
    df_italic$id, df_italic$i_index,
    ~expr(flextable::italic(i = !!.y, j = !!.x, part = "body"))
  )

  # source note ----------------------------------------------------------------
  # in flextable, this is just a footnote associated without column or symbol
  if (!is.null(x$list_output$source_note)) {
    flextable_calls[["source_note"]] <-
      expr(
        flextable::footnote(value = flextable::as_paragraph(!!x$list_output$source_note), ref_symbols = "")
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
        flextable::valign(j = ~label, valign = "top", part = "body")
      )
    )

  flextable_calls
}

