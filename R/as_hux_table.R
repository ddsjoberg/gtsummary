#' Convert gtsummary object to a huxtable object
#'
#' \Sexpr[results=rd, stage=render]{lifecycle::badge("experimental")}
#' Function converts a gtsummary object to a huxtable object.
#' A user can use this function if they wish to add customized formatting
#' available via the huxtable functions. The huxtable package supports output
#' to PDF via LaTeX, as well as HTML and Word.
#'
#' @section Details:
#' The `as_hux_table()` takes the data frame that will be printed, converts
#' it to a huxtable and formats the table with the following huxtable functions:
#'
#' 1. [huxtable::huxtable()]
#' 1. [huxtable::insert_row()] to insert header rows
#' 1. [huxtable::align()] to set column alignment
#' 1. [huxtable::set_left_padding()] to indent variable levels
#' 1. [huxtable::add_footnote()] to add table footnotes and source notes
#' 1. [huxtable::set_bold()] to bold cells
#' 1. [huxtable::set_italic()] to italicize cells
#' 1. [huxtable::set_na_string()] to use an em-dash for missing numbers
#'
#' Any one of these commands may be omitted using the `include=` argument.
#'
#' @inheritParams as_flex_table
#' @export
#' @return A {huxtable} object
#' @family gtsummary output types
#' @author David Hugh-Jones
#' @examples
#' trial %>%
#'   dplyr::select(trt, age, grade) %>%
#'   tbl_summary(by = trt) %>%
#'   add_p() %>%
#'   as_hux_table()
#' @export

as_hux_table <- function(x, include = everything(), return_calls = FALSE,
                         strip_md_bold = TRUE,  group_header = NULL) {
  assert_package("huxtable", "as_hux_table")

  # setting defaults -----------------------------------------------------------
  group_header <-
    group_header %||%
    get_theme_element("pkgwide-str:group_header", default = "**Group**")

  # stripping markdown asterisk ------------------------------------------------
  if (strip_md_bold == TRUE) {
    x$table_header <-
      x$table_header %>%
      mutate_at(
        vars(.data$label, .data$spanning_header),
        ~str_replace_all(., pattern = fixed("**"), replacement = fixed(""))
      )
    group_header <- str_replace_all(group_header, pattern = fixed("**"), replacement = fixed(""))
  }

  # creating list of huxtable calls -------------------------------------------
  huxtable_calls <- table_header_to_huxtable_calls(x = x, group_header = group_header)

  # adding user-specified calls ------------------------------------------------
  insert_expr_after <- get_theme_element("as_hux_table.gtsummary-lst:addl_cmds")
  huxtable_calls <-
    purrr::reduce(
      .x = seq_along(insert_expr_after),
      .f = function(x, y) add_expr_after(calls = x,
                                         add_after = names(insert_expr_after[y]),
                                         expr = insert_expr_after[[y]],
                                         new_name = paste0("user_added", y)),
      .init = huxtable_calls
    )

  # converting to character vector ----------------------------------------------
  include <- var_input_to_string(data = vctr_2_tibble(names(huxtable_calls)),
                                 select_input = !!rlang::enquo(include))

  # return calls, if requested -------------------------------------------------
  if (return_calls == TRUE) return(huxtable_calls[include])

  huxtable_calls[include] %>%
    # removing NULL elements
    unlist() %>%
    compact() %>%
    # concatenating expressions with %>% between each of them
    reduce(function(x, y) expr(!!x %>% !!y)) %>%
    # evaluating expressions
    eval()
}

# creating huxxtable calls from table_header -----------------------------------
table_header_to_huxtable_calls <- function(x, group_header, ...) {
  # if there is a grouping variable, add table_header info for it
  if (dplyr::group_vars(x$table_body) %>% length() > 0) {
    table_header <-
      tibble::tibble(column = "groupname_col",
                     label = group_header,
                     hide = FALSE,
                     align = "left") %>%
      bind_rows(x$table_header) %>%
      group_by(.data$hide) %>%
      mutate(id = ifelse(.data$hide == FALSE, dplyr::row_number(), NA)) %>%
      ungroup()
  }
  else {
    table_header <-
      x$table_header %>%
      group_by(.data$hide) %>%
      mutate(id = ifelse(.data$hide == FALSE, dplyr::row_number(), NA)) %>%
      ungroup()
  }

  # tibble ---------------------------------------------------------------------
  # huxtable doesn't use the markdown language `__` or `**`
  # to bold and italicize text, so removing them here
  huxtable_calls <-
    as_tibble(x, return_calls = TRUE,
              include = -c("cols_label", "tab_style_bold", "tab_style_italic"))

  huxtable_calls[["huxtable"]] <- expr(huxtable::as_huxtable(add_colnames = FALSE))

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

  huxtable_calls[["set_left_padding"]] <- map2(
    df_padding$id, df_padding$i_index,
    ~expr(huxtable::set_left_padding(row = !!.y, col = !!.x, value = 15))
  )

  # footnote -------------------------------------------------------------------
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

  borders <- rep(0, length(df_footnote$footnote))
  if (length(df_footnote$footnote) > 0) borders[1] <- 0.8
  huxtable_calls[["add_footnote"]] <- map2(
    df_footnote$footnote, borders,
    ~expr(
      huxtable::add_footnote(
        text = !!.x,
        border = !!.y
      )
    )
  )

  # source note ----------------------------------------------------------------
  if (!is.null(x$list_output$source_note)) {
    huxtable_calls[["add_footnote"]] <- append(huxtable_calls[["add_footnote"]],
      expr(
        huxtable::add_footnote(text = !!x$list_output$source_note)
      )
    )
  }

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

  huxtable_calls[["set_bold"]] <- map2(
    df_bold$id, df_bold$i_index,
    ~expr(huxtable::set_bold(row = !!.y, col = !!.x, value = TRUE))
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

  huxtable_calls[["set_italic"]] <- map2(
    df_italic$id, df_italic$i_index,
    ~expr(huxtable::set_italic(row = !!.y, col = !!.x, value = TRUE))
  )

  # set_na_string -------------------------------------------------------
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

  huxtable_calls[["set_na_string"]] <- map2(df_na_emdash$i_index, df_na_emdash$id,
    ~expr(
      huxtable::set_na_string(row = !!.x, col = !!.y, value = "\U2014")
    )
  )

  # insert_row ----------------------------------------------------------
  # we do this last so as to not mess up row indexes before
  col_labels <- table_header %>%
    filter(.data$hide == FALSE) %>%
    {set_names(as.list(.[["label"]]), .[["column"]])}

  huxtable_calls[["insert_row"]] <- list()

  huxtable_calls[["insert_row"]] <- append(huxtable_calls[["insert_row"]],
    expr(huxtable::insert_row(after = 0, !!!col_labels)))

  any_spanning_header <- sum(!is.na(table_header$spanning_header)) > 0
  if (any_spanning_header) {
    header_content <- table_header$spanning_header[table_header$hide == FALSE]
    huxtable_calls[["insert_row"]] <- append(huxtable_calls[["insert_row"]],
      expr(huxtable::insert_row(after = 0, !!! header_content)))

    header_colspans <- rle(header_content)$lengths
    header_colspan_cols <- cumsum(c(1,
      header_colspans[-length(header_colspans)]))
    huxtable_calls[["insert_row"]] <- append(huxtable_calls[["insert_row"]],
      expr(
        huxtable::set_colspan(row = 1, col = !! header_colspan_cols,
          value = !! header_colspans)
      )
    )
  }
  header_bottom_row <- if (any_spanning_header) 2 else 1
  huxtable_calls[["insert_row"]] <- append(huxtable_calls[["insert_row"]],
    expr(
      huxtable::set_bottom_border(row = !! header_bottom_row, col =
        huxtable::everywhere, value = 0.4)
    )
  )

  # align ----------------------------------------------------------------------
  df_align <-
    table_header %>%
    filter(.data$hide == FALSE) %>%
    select(.data$id, .data$align) %>%
    group_by(.data$align) %>%
    nest() %>%
    ungroup()

  huxtable_calls[["align"]] <- map2(
    df_align$align, df_align$data,
    ~expr(huxtable::set_align(row = huxtable::everywhere, col = !!.y$id,
                              value = !!.x))
  )

  huxtable_calls
}
