#' Convert gtsummary object to a flextable object
#'
#' \Sexpr[results=rd, stage=render]{lifecycle::badge("experimental")}
#' Function converts a gtsummary object to a flextable object.
#' A user can use this function if they wish to add customized formatting
#' available via the flextable functions. The flextable output is particularly
#' useful when combined with R markdown with Word output, since the gt package
#' does not support Word.
#'
#' @section Details:
#' The `as_flextable()` takes the data frame that will be printed and converts
#' it to a flextable and formats the table with the following flextable functions.
#' 1. [flextable::flextable()]
#' 1. [flextable::set_header_labels()] to set column labels
#' 1. [flextable::add_header_row()], if applicable, to set spanning column header
#' 1. [flextable::align()] to set column alignment
#' 1. [flextable::padding()] to indent variable levels
#' 1. [flextable::autofit()] to estimate the column widths
#' 1. [flextable::footnote()] to add table footnotes and source notes
#' 1. [flextable::bold()] to bold cells in data frame
#' 1. [flextable::italic()] to italicize cells in data frame
#'
#' Any one of these commands may be omitted using the `include=` argument.
#'
#' Pro tip: Use the [flextable::width()] function for exacting control over
#' column width after calling [as_flextable()].
#' @inheritParams as_gt
#' @param strip_md_bold When TRUE, all double asterisk (markdown language for
#' bold weight) in column labels and spanning headers are removed.
#' Default is TRUE
#' @param ... Not used
#' @name as_flextable
#' @export
#' @return A {flextable} object
#' @family gtsummary output types
#' @author Daniel D. Sjoberg
#' @examples
#' trial %>%
#'   dplyr::select(trt, age, grade) %>%
#'   tbl_summary(by = trt) %>%
#'   add_p() %>%
#'   as_flextable()
as_flextable <- function(x, ...) {
  UseMethod("as_flextable")
}

#' @rdname as_flextable
#' @export
as_flextable.gtsummary <- function(x, include = everything(), return_calls = FALSE,
                         strip_md_bold = TRUE, ...) {
  # must have flextable package installed to use this function -----------------
  if (!requireNamespace("flextable", quietly = TRUE)) {
    stop(paste0(
      "The 'flextable' package is required for 'as_flextable'.\n",
      "Install with install.packages('flextable')"
    ), call. = FALSE)
  }

  # stripping markdown asterisk ------------------------------------------------
  if (strip_md_bold == TRUE) {
    x$table_header <-
      x$table_header %>%
      mutate(
        label = str_replace_all(
          .data$label, pattern = fixed("**"), replacement = fixed("")
        ),
        spanning_header = str_replace_all(
          .data$spanning_header, pattern = fixed("**"), replacement = fixed("")
        )
      )
  }

  # creating list of flextable calls -------------------------------------------
  flextable_calls <- table_header_to_flextable_calls(x = x)
  if (return_calls == TRUE) return(flextable_calls)

  # converting to charcter vector ----------------------------------------------
  include <- var_input_to_string(data = vctr_2_tibble(names(flextable_calls)),
                                 select_input = !!rlang::enquo(include))

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
  table_header <-
    x$table_header %>%
    group_by(.data$hide) %>%
    mutate(id = ifelse(.data$hide == FALSE, dplyr::row_number(), NA)) %>%
    ungroup()

  # tibble ---------------------------------------------------------------------
  # getting flextable calls
  flextable_calls <-
    table_header_to_tibble_calls(x = x, col_labels = FALSE)
  flextable_calls[["tab_style_bold"]] <- NULL
  flextable_calls[["tab_style_italic"]] <- NULL

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
    df_header <-
      table_header %>%
      filter(.data$hide == FALSE) %>%
      select(.data$spanning_header) %>%
      mutate(spanning_header = ifelse(is.na(.data$spanning_header),
                                      " ",
                                      .data$spanning_header)) %>%
      group_by(.data$spanning_header) %>%
      dplyr::summarise(width = n()) %>%
      ungroup()

    flextable_calls[["add_header_row"]] <- expr(
      flextable::add_header_row(
        values = !!df_header$spanning_header,
        colwidths = !!df_header$width
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
  # in flextable, this is just a footnote associated with column or symbol
  if (!is.null(x$list_output$source_note)) {
    flextable_calls[["source_note"]] <-
      expr(
        flextable::footnote(value = flextable::as_paragraph(!!x$list_output$source_note), ref_symbols = "")
      )
  }

  flextable_calls
}

