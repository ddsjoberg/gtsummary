#' Convert gtsummary object to a kableExtra object
#'
#' Function converts a gtsummary object to a knitr_kable + kableExtra object.
#' A user can use this function if they wish to add customized formatting
#' available via [knitr::kable] and {kableExtra}; `as_kable_extra()` supports
#' arguments in `kableExtra::kbl()`. Bold and italic cells are supported for
#' table bodies, but not yet column names, in {kableExtra} output via gtsummary.
#'
#' @inheritParams as_kable
#' @inheritParams as_flex_table
#' @export
#' @return A {kableExtra} object
#' @family gtsummary output types
#' @author Daniel D. Sjoberg
#' @examplesIf broom.helpers::.assert_package("kableExtra", boolean = TRUE)
#' tbl <-
#'   trial %>%
#'   tbl_summary(by = trt) %>%
#'   as_kable_extra()
as_kable_extra <- function(x, include = everything(), return_calls = FALSE,
                           strip_md_bold = TRUE, fmt_missing = TRUE, ...) {
  # must have kableExtra package installed to use this function ----------------
  assert_package("kableExtra", "as_kable_extra()")

  # running pre-conversion function, if present --------------------------------
  x <- do.call(get_theme_element("pkgwide-fun:pre_conversion", default = identity), list(x))

  # converting row specifications to row numbers, and removing old cmds --------
  x <- .clean_table_styling(x)

  # stripping markdown asterisk ------------------------------------------------
  if (strip_md_bold == TRUE) {
    x$table_styling$header <-
      x$table_styling$header %>%
      mutate(
        label = str_replace_all(
          .data$label,
          pattern = fixed("**"), replacement = fixed("")
        ),
        spanning_header = str_replace_all(
          .data$spanning_header,
          pattern = fixed("**"), replacement = fixed("")
        )
      )
  }

  # creating list of kableExtra calls ------------------------------------------
  kable_extra_calls <-
    table_styling_to_kable_extra_calls(x = x, fmt_missing = fmt_missing, ...)

  # adding user-specified calls ------------------------------------------------
  insert_expr_after <- get_theme_element("as_kable_extra-lst:addl_cmds")
  kable_extra_calls <-
    purrr::reduce(
      .x = seq_along(insert_expr_after),
      .f = function(x, y) {
        add_expr_after(
          calls = x,
          add_after = names(insert_expr_after[y]),
          expr = insert_expr_after[[y]],
          new_name = paste0("user_added", y)
        )
      },
      .init = kable_extra_calls
    )

  # converting to charcter vector ----------------------------------------------
  include <-
    .select_to_varnames(
      select = {{ include }},
      var_info = names(kable_extra_calls),
      arg_name = "include"
    )

  # making list of commands to include -----------------------------------------
  # this ensures list is in the same order as names(x$kable_calls)
  include <- names(kable_extra_calls) %>% intersect(include)
  # user cannot exclude the first 'kable' command
  include <- "tibble" %>% union(include)

  # return calls, if requested -------------------------------------------------
  if (return_calls == TRUE) {
    return(kable_extra_calls)
  }

  # taking each kable function call, concatenating them with %>% separating them
  kable_extra_calls[include] %>%
    # removing NULL elements
    unlist() %>%
    compact() %>%
    # concatenating expressions with %>% between each of them
    reduce(function(x, y) expr(!!x %>% !!y)) %>%
    # evaluating expressions
    eval()
}

table_styling_to_kable_extra_calls <- function(x, fmt_missing = FALSE, ...) {
  # getting kable calls
  kable_extra_calls <-
    table_styling_to_kable_calls(x = x, fmt_missing = fmt_missing, ...)

  # adding id number for columns not hidden
  x$table_styling$header <-
    x$table_styling$header %>%
    group_by(.data$hide) %>%
    mutate(id = ifelse(.data$hide == FALSE, dplyr::row_number(), NA)) %>%
    ungroup()

  # kableExtra doesn't support markdown bold/italics, will replace in next section
  kable_extra_calls <-
    kable_extra_calls %>%
    purrr::list_modify(tab_style_bold = NULL, tab_style_italic = NULL)

  # bold and italic ------------------------------------------------------------
  kable_extra_calls <- .add_bold_italic_calls(kable_extra_calls, x, ...)

  # add_indent -----------------------------------------------------------------
  df_indent <-
    x$table_styling$text_format %>%
    filter(.data$format_type == "indent", .data$column == "label")

  if (nrow(df_indent) > 0) {
    kable_extra_calls[["add_indent"]] <-
      expr(kableExtra::add_indent(!!df_indent$row_numbers[[1]]))
  }

  # add_indent2 -----------------------------------------------------------------
  df_indent2 <-
    x$table_styling$text_format %>%
    filter(.data$format_type == "indent2", .data$column == "label")

  if (nrow(df_indent2) > 0) {
    kable_extra_calls[["add_indent2"]] <-
      expr(kableExtra::add_indent(!!df_indent2$row_numbers[[1]], level_of_indent = 2))
  }

  # add_header_above -----------------------------------------------------------
  if (any(!is.na(x$table_styling$header$spanning_header))) {
    df_header0 <-
      x$table_styling$header %>%
      filter(.data$hide == FALSE) %>%
      select(.data$spanning_header) %>%
      mutate(
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
      df_header0 %>%
      group_by(.data$spanning_header_id) %>%
      mutate(width = n()) %>%
      distinct() %>%
      ungroup()

    header <- df_header$width %>% set_names(df_header$spanning_header)

    kable_extra_calls[["add_header_above"]] <-
      expr(kableExtra::add_header_above(header = !!header))
  }

  # horizontal_line_above ------------------------------------------------------
  if (!is.null(x$table_styling$horizontal_line_above)) {
    row_number <-
      eval_tidy(x$table_styling$horizontal_line_above, data = x$table_body) %>%
      which()
    row_number <- row_number - 1
    kable_extra_calls[["horizontal_line"]] <-
      expr(
        kableExtra::row_spec(row = !!row_number, hline_after = TRUE)
      )
  }

  # footnote -------------------------------------------------------------------
  vct_footnote <-
    .number_footnotes(x) %>%
    pull(.data$footnote) %>%
    unique()

  if (length(vct_footnote > 0)) {
    kable_extra_calls[["footnote"]] <-
      expr(kableExtra::footnote(number = !!vct_footnote))
  }

  kable_extra_calls
}

.add_bold_italic_calls <- function(kable_extra_calls, x, ...) {
  dots <- rlang::dots_list(...)
  escape <- dots[["escape"]] %||% TRUE

  # use `column_spec()` if `kable(escape = TRUE)` (the default) ----------------
  if (isTRUE(escape)) {
    df_bold_italic <-
      x$table_styling$text_format %>%
      dplyr::filter(.data$format_type %in% c("bold", "italic")) %>%
      mutate(index = map(.data$row_numbers, ~seq_len(nrow(x$table_body)) %in% .x)) %>%
      dplyr::left_join(
        x$table_styling$header %>% select(.data$column, .data$id),
        by = "column"
      )

    df_bold <- df_bold_italic %>% filter(.data$format_type %in% "bold")
    df_italic <- df_bold_italic %>% filter(.data$format_type %in% "italic")

    kable_extra_calls[["bold_italic"]] <-
      c(
        # bold
        map(
          seq_len(nrow(df_bold)),
          ~rlang::expr(kableExtra::column_spec(column = !!df_bold$id[.x], bold = !!df_bold$index[[.x]]))
        ),
        # italic
        map(
          seq_len(nrow(df_italic)),
          ~rlang::expr(kableExtra::column_spec(column = !!df_italic$id[.x], italic = !!df_italic$index[[.x]]))
        )
      )

    return(kable_extra_calls)
  }


  # use `cell_spec()` if `escape = FALSE` --------------------------------------
  # inserting blank bold and italic instructions before the kable() call.
  kable_extra_call_names <- names(kable_extra_calls)
  kable_extra_calls[["bold_italic"]] <- list()
  kable_extra_call_names <-
    append(
      kable_extra_call_names,
      values = "bold_italic",
      after = which(kable_extra_call_names %in% "kable") - 1L
    )
  kable_extra_calls <- kable_extra_calls[kable_extra_call_names]

  # combine bold/italic instructions into single df
  df_bold_italic <-
    x$table_styling$text_format %>%
    dplyr::filter(.data$format_type %in% c("bold", "italic")) %>%
    tidyr::unnest(.data$row_numbers) %>%
    {dplyr::full_join(
      dplyr::filter(., .data$format_type %in% "bold") %>%
        dplyr::mutate(bold = TRUE) %>%
        dplyr::select(.data$column, .data$row_numbers, .data$bold),
      dplyr::filter(., .data$format_type %in% "italic") %>%
        dplyr::mutate(italic = TRUE) %>%
        dplyr::select(.data$column, .data$row_numbers, .data$italic),
      by = c("column", "row_numbers")
    )} %>%
    dplyr::mutate(
      dplyr::across(c(.data$bold, .data$italic), ~tidyr::replace_na(., FALSE))
    ) %>%
    tidyr::nest(row_numbers = .data$row_numbers) %>%
    dplyr::mutate(
      row_numbers = map(.data$row_numbers, ~unlist(.x) %>% unname())
    )

  # construct call to bold/italicize cells
  kable_extra_calls[["bold_italic"]] <-
    map(
      seq_len(nrow(df_bold_italic)),
      ~ expr(
        dplyr::mutate(
          dplyr::across(
            dplyr::all_of(!!df_bold_italic$column[.x]),
            function(xx) ifelse(
              dplyr::row_number() %in% !!df_bold_italic$row_numbers[[.x]],
              kableExtra::cell_spec(
                xx,
                bold = !!df_bold_italic$bold[.x],
                italic = !!df_bold_italic$italic[.x]
              ),
              xx
            )
          )
        )
      )
    )

  return(kable_extra_calls)
}
