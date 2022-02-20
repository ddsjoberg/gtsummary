#' Convert gtsummary object to a kableExtra object
#'
#' Function converts a gtsummary object to a knitr_kable + kableExtra object.
#' This allows the customized formatting available via [knitr::kable]
#' and {kableExtra}; `as_kable_extra()` supports arguments in `knitr::kable()`.
#' `as_kable_extra()` output via gtsummary supports
#' bold and italic cells for table bodies. Users
#' creating pdf output should specify `as_kable_extra(format = "latex")`.
#'
#' @section PDF via LaTeX Tips:
#'
#' This section discusses options intended for use with
#'  - `output: pdf_document` in yaml of `.Rmd`.
#'  - `as_kable_extra(format = "latex", escape = FALSE)`
#'
#' With LaTeX output with `escape = FALSE`, the markdown syntax for bold
#' and italic are converted to LaTeX code and `"\n"` is recognized as a linebreaker.
#'
#' With `escape = FALSE`, it's important that any special characters in your
#' table are escaped. For example, the default `tbl_summary()` table includes
#' a percentage symbol for categorical variables. The statistics should
#' be updated to `statistic = all_categorical() ~ "{n} ({p}\\%)"`
#'
#' ### Additional table styling
#'
#' Additional styling is available through `knitr::kable()` and
#' `kableExtra::kable_styling()` as shown in Example 3, which implements row
#' striping and repeated column headers in the presence of page breaks.
#'
#' @inheritParams as_kable
#' @inheritParams as_flex_table
#' @param format,escape,... arguments passed to `knitr::kable()`
#' @export
#' @return A {kableExtra} table
#' @family gtsummary output types
#' @author Daniel D. Sjoberg
#' @examplesIf broom.helpers::.assert_package("kableExtra", boolean = TRUE)
#' \donttest{
#' # Example 1 (general) -------------------------------------------------------
#' as_kable_extra_ex1_gen <-
#'   trial %>%
#'   select(trt, age, stage) %>%
#'   tbl_summary(by = trt) %>%
#'   bold_labels() %>%
#'   as_kable_extra()
#'
#' # Example 2 (PDF via LaTeX) -------------------------------------------------
#' as_kable_extra_ex2_pdf <-
#'   trial %>%
#'   select(trt, age, stage) %>%
#'   tbl_summary(by = trt) %>%
#'   bold_labels() %>%
#'   modify_header(all_stat_cols() ~ "**{level}**\n*N = {n}*") %>%
#'   as_kable_extra(format = "latex", escape = FALSE)
#'
#' # Example 3 (PDF via LaTeX) -------------------------------------------------
#' as_kable_extra_ex3_pdf <-
#'   trial %>%
#'   select(trt, age, stage) %>%
#'   tbl_summary(by = trt) %>%
#'   bold_labels() %>%
#'   as_kable_extra(
#'     format = "latex",
#'     escape = FALSE,
#'     booktabs = TRUE,
#'     longtable = TRUE,
#'     linesep = ""
#'   ) %>%
#'   kableExtra::kable_styling(
#'     position = "left",
#'     latex_options = c("striped", "repeat_header"),
#'     stripe_color = "gray!15"
#'   )
#' }
#' @section Example Output:
#' \if{html}{Example 1 (html)}
#'
#' \if{html}{\figure{as_kable_extra_ex1_html.png}{options: width=40\%}}
#'
#' \if{html}{Example 1 (pdf)}
#'
#' \if{html}{\figure{as_kable_extra_ex1_pdf.png}{options: width=40\%}}
#'
#' \if{html}{Example 2 (pdf)}
#'
#' \if{html}{\figure{as_kable_extra_ex2_pdf.png}{options: width=40\%}}
#'
#' \if{html}{Example 3 (pdf)}
#'
#' \if{html}{\figure{as_kable_extra_ex3_pdf.png}{options: width=40\%}}
#'

as_kable_extra <- function(x,
                           format = ifelse(knitr::is_latex_output(), "latex", "html"),
                           escape = TRUE,
                           ...,
                           include = everything(),
                           return_calls = FALSE) {
  # must have kableExtra package installed to use this function ----------------
  assert_package("kableExtra", "as_kable_extra()")

  # running pre-conversion function, if present --------------------------------
  x <- do.call(get_theme_element("pkgwide-fun:pre_conversion", default = identity), list(x))

  # converting row specifications to row numbers, and removing old cmds --------
  x <- .clean_table_styling(x)

  # creating list of kableExtra calls ------------------------------------------
  kable_extra_calls <-
    table_styling_to_kable_extra_calls(x = x,
                                       escape = escape,
                                       format = format, ...)

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

table_styling_to_kable_extra_calls <- function(x, escape, format, ...) {
  dots <- rlang::dots_list(...)

  if (!is.null(dots[["strip_md_bold"]])) {
    lifecycle::deprecate_warn(when = "1.5.3",
                              what = "gtsummary::as_kable_extra(strip_md_bold=)")
    dots <- purrr::list_modify(strip_md_bold = NULL) %>% purrr::compact()
  }
  if (!is.null(dots[["fmt_missing"]])) {
    lifecycle::deprecate_warn(when = "1.5.3",
                              what = "gtsummary::as_kable_extra(fmt_missing=)")
    dots <- purrr::list_modify(fmt_missing = NULL) %>% purrr::compact()
  }

  # if escape is FALSE and latex output, convert markdown to latex and add linebreaks
  if (!isTRUE(escape) && isTRUE(format == "latex")) {
    x <- .linebreak_gtsummary(x)
  }
  # otherwise, remove markdown syntax from headers
  else {
    x$table_styling$header <-
      x$table_styling$header %>%
      mutate(
        label = .strip_markdown(.data$label),
        spanning_header = .strip_markdown(.data$spanning_header)
      )
  }

  # getting kable calls
  kable_extra_calls <-
    table_styling_to_kable_calls(x = x,
                                 escape = escape,
                                 format = format, ...)

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

  # escaping special characters in table_body ----------------------------------
  if (!isTRUE(escape) && format %in% "latex") {
    kable_call_index <- which(names(kable_extra_calls) %in% "kable")
    cols_to_escape <-
      filter(x$table_styling$header, !.data$hide) %>% dplyr::pull(.data$column)

    expr_escape_table_body <-
      list(
        escape_table_body =
          rlang::expr(
            dplyr::mutate(
              dplyr::across(all_of(!!cols_to_escape) & where(is.character),
                            function(.x) gsub("([#$%&_{}])", "\\\\\\1", x = .x))
            )
          )
      )

    kable_extra_calls <-
      append(kable_extra_calls, values = expr_escape_table_body, after = kable_call_index - 1L)
  }

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

    header <-
      df_header$width %>%
      set_names(df_header$spanning_header) %>%
      c(list(escape = dots[["escape"]])) %>%
      purrr::compact()

    kable_extra_calls[["add_header_above"]] <-
      expr(kableExtra::add_header_above(header = !!header, escape = !!escape))
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


# This function calls `kableExtra::linebreak()` on gtsummary headers and spanning headers.
# Note that `escape = FALSE` and `format = "latex"` are required.
#  - the default `align=` argument is taken from the gtsummary object
#  - the markdown double-star and double-underscore bold syntax is converted to LaTeX, `\textbf{}`
#  - the markdown single-star and single-underscore italic syntax is converted to LaTeX, `\textit{}`
.linebreak_gtsummary <- function(x,
                                 align = NULL,
                                 linebreaker = "\n") {
  # set align argument ---------------------------------------------------------
  align <-
    align %||%
    stringr::str_sub(dplyr::filter(x$table_styling$header, !.data$hide)$align, 1, 1)

  # linebreak the headers ------------------------------------------------------
  x$table_styling$header$label <-
    kableExtra::linebreak(
      x = .markdown_to_latex(x$table_styling$header$label),
      align = align,
      linebreaker = linebreaker
    )

  x$table_styling$header$spanning_header <-
    kableExtra::linebreak(
      x = .markdown_to_latex2(x$table_styling$header$spanning_header),
      align = "c",
      linebreaker = linebreaker,
      double_escape = TRUE
    )

  # return process gtsummary table ---------------------------------------------
  x

}

# this escapes the latex code in `knitr::kable(col.names=)`
.markdown_to_latex <- function(x) {
  x %>%
    # convert bold ** to \textbf{}
    stringr::str_replace_all(
      pattern = "\\*\\*(.*?)\\*\\*",
      replacement = "\\\\textbf{\\1}"
    ) %>%
    # convert bold __ to \textbf{}
    stringr::str_replace_all(
      pattern = "\\_\\_(.*?)\\_\\_",
      replacement = "\\\\textbf{\\1}"
    ) %>%
    # convert italic * to \textit{}
    stringr::str_replace_all(
      pattern = "\\*(.*?)\\*",
      replacement = "\\\\textit{\\1}"
    ) %>%
    # convert italic _ to \textit{}
    stringr::str_replace_all(
      pattern = "\\_(.*?)\\_",
      replacement = "\\\\textit{\\1}"
    )
}

# this escapes the latex code in `kableExtra::add_header_row()`
.markdown_to_latex2 <- function(x) {
  x %>%
    # convert bold ** to \textbf{}
    stringr::str_replace_all(
      pattern = "\\*\\*(.*?)\\*\\*",
      replacement = "\\\\\\\\textbf{\\1}"
    ) %>%
    # convert bold __ to \textbf{}
    stringr::str_replace_all(
      pattern = "\\_\\_(.*?)\\_\\_",
      replacement = "\\\\\\\\textbf{\\1}"
    ) %>%
    # convert italic * to \textit{}
    stringr::str_replace_all(
      pattern = "\\*(.*?)\\*",
      replacement = "\\\\\\\\textit{\\1}"
    ) %>%
    # convert italic _ to \textit{}
    stringr::str_replace_all(
      pattern = "\\_(.*?)\\_",
      replacement = "\\\\\\\\textit{\\1}"
    )
}

.strip_markdown <- function(x) {
  x %>%
    # convert bold ** to \textbf{}
    stringr::str_replace_all(
      pattern = "\\*\\*(.*?)\\*\\*",
      replacement = "\\1"
    ) %>%
    # convert bold __ to \textbf{}
    stringr::str_replace_all(
      pattern = "\\_\\_(.*?)\\_\\_",
      replacement = "\\1"
    ) %>%
    # convert italic * to \textit{}
    stringr::str_replace_all(
      pattern = "\\*(.*?)\\*",
      replacement = "\\1"
    ) %>%
    # convert italic _ to \textit{}
    stringr::str_replace_all(
      pattern = "\\_(.*?)\\_",
      replacement = "\\1"
    )
}
