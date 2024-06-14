#' Convert gtsummary object to a kableExtra object
#'
#' Function converts a gtsummary object to a knitr_kable + kableExtra object.
#' This allows the customized formatting available via `knitr::kable()`
#' and \{kableExtra\}; `as_kable_extra()` supports arguments in `knitr::kable()`.
#' `as_kable_extra()` output via gtsummary supports
#' bold and italic cells for table bodies. Users
#' are encouraged to leverage `as_kable_extra()` for enhanced pdf printing; for html
#' output options there is better support via `as_gt()`.
#'
#' @section PDF/LaTeX:
#'
#' This section shows options intended for use with `output: pdf_document` in yaml of `.Rmd`.
#'
#' When the default values of `as_kable_extra(escape = FALSE, addtl_fmt = TRUE)`
#' are utilized, the following formatting occurs.
#'    - Markdown bold, italic, and underline syntax in the headers,
#'      spanning headers, caption, and footnote will be converted to escaped LaTeX code
#'    - Special characters in the table body, headers, spanning headers, caption,
#'      and footnote will be escaped with `.escape_latex()` or `.escape_latex2()`
#'    - The `"\n"` symbol will be recognized as a line break in the table
#'      headers, spanning headers, caption, and the table body
#'    - The `"\n"` symbol is removed from the footnotes
#'
#' To suppress _these_ additional formats, set `as_kable_extra(addtl_fmt = FALSE)`
#'
#' Additional styling is available with
#' `kableExtra::kable_styling()` as shown in Example 2, which implements row
#' striping and repeated column headers in the presence of page breaks.
#'
#' @section HTML:
#'
#' This section discusses options intended for use with `output: html_document` in yaml of `.Rmd`.
#'
#' When the default values of `as_kable_extra(escape = FALSE, addtl_fmt = TRUE)`
#' are utilized, the following formatting occurs.
#'    - The default markdown syntax in the headers and spanning headers is removed
#'    - Special characters in the table body, headers, spanning headers, caption,
#'      and footnote will be escaped with `.escape_html()`
#'    - The `"\n"` symbol is removed from the footnotes
#'
#' To suppress the additional formatting, set `as_kable_extra(addtl_fmt = FALSE)`
#'
#' @inheritParams as_kable
#' @inheritParams as_flex_table
#' @param format,escape,... arguments passed to `knitr::kable()`. Default is
#' `escape = FALSE`, and the format is auto-detected.
#' @param addtl_fmt logical indicating whether to include additional formatting.
#' Default is `TRUE`. This is primarily used to escape special characters,
#' convert markdown to LaTeX, and remove line breaks from the footnote.
#' @export
#' @return A \{kableExtra\} table
#'
#' @author Daniel D. Sjoberg
#' @examplesIf gtsummary:::is_pkg_installed("kableExtra", reference_pkg = "gtsummary")
#' # basic gtsummary tbl to build upon
#' as_kable_extra_base <-
#'   trial |>
#'   tbl_summary(by = trt, include = c(age, stage)) |>
#'   bold_labels()
#'
#' # Example 1 (PDF via LaTeX) ---------------------
#' # add linebreak in table header with '\n'
#' as_kable_extra_ex1_pdf <-
#'   as_kable_extra_base |>
#'   modify_header(all_stat_cols() ~ "**{level}**  \n*N = {n}*") |>
#'   as_kable_extra()
#'
#' # Example 2 (PDF via LaTeX) ---------------------
#' # additional styling in `knitr::kable()` and with
#' #   call to `kableExtra::kable_styling()`
#' as_kable_extra_ex2_pdf <-
#'   as_kable_extra_base |>
#'   as_kable_extra(
#'     booktabs = TRUE,
#'     longtable = TRUE,
#'     linesep = ""
#'   ) |>
#'   kableExtra::kable_styling(
#'     position = "left",
#'     latex_options = c("striped", "repeat_header"),
#'     stripe_color = "gray!15"
#'   )
as_kable_extra <- function(x,
                           escape = FALSE,
                           format = NULL,
                           ...,
                           include = everything(),
                           addtl_fmt = TRUE,
                           return_calls = FALSE) {
  set_cli_abort_call()
  check_pkg_installed(c("kableExtra", "knitr"), reference_pkg = "gtsummary")

  # process inputs -------------------------------------------------------------
  check_class(x, "gtsummary")
  check_scalar_logical(escape)
  check_scalar_logical(addtl_fmt)
  check_scalar_logical(return_calls)

  # running pre-conversion function, if present --------------------------------
  x <- do.call(get_theme_element("pkgwide-fun:pre_conversion", default = identity), list(x))

  # converting row specifications to row numbers, and removing old cmds --------
  x <- .table_styling_expr_to_row_number(x)

  # creating list of kableExtra calls ------------------------------------------
  kable_extra_calls <-
    table_styling_to_kable_extra_calls(
      x = x,
      escape = escape,
      format = format %||% ifelse(knitr::is_latex_output(), "latex", "html"),
      addtl_fmt = addtl_fmt,
      ...
    )

  # adding user-specified calls ------------------------------------------------
  insert_expr_after <- get_theme_element("as_kable_extra-lst:addl_cmds")
  kable_extra_calls <-
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
      .init = kable_extra_calls
    )

  # converting to charcter vector ----------------------------------------------
  cards::process_selectors(
    data = vec_to_df(names(kable_extra_calls)),
    include = {{ include }}
  )

  # making list of commands to include -----------------------------------------
  # this ensures list is in the same order as names(x$kable_calls)
  include <- names(kable_extra_calls) |> intersect(include)
  # user cannot exclude the first 'kable' command
  include <- "tibble" %>% union(include)

  # return calls, if requested -------------------------------------------------
  if (isTRUE(return_calls)) {
    return(kable_extra_calls)
  }

  # taking each kable function call, concatenating them with %>% separating them
  .eval_list_of_exprs(kable_extra_calls[include])
}




table_styling_to_kable_extra_calls <- function(x, escape, format, addtl_fmt, ...) {
  dots <- rlang::dots_list(...)

  if (!is.null(dots[["strip_md_bold"]])) {
    lifecycle::deprecate_warn(
      when = "1.6.0",
      what = "gtsummary::as_kable_extra(strip_md_bold=)"
    )
  }
  if (!is.null(dots[["fmt_missing"]])) {
    lifecycle::deprecate_stop(
      when = "1.6.0",
      what = "gtsummary::as_kable_extra(fmt_missing=)"
    )
  }

  # if escape is FALSE and latex output, convert markdown to latex and add linebreaks
  if (!isTRUE(escape) && isTRUE(addtl_fmt) && isTRUE(format == "latex")) {
    x <- .latex_conversion_and_escaping(x)
  }
  # if escape is FALSE and latex output, convert markdown to latex and add linebreaks
  else if (!isTRUE(escape) && isTRUE(addtl_fmt) && isTRUE(format == "html")) {
    x <- .html_conversion_and_escaping(x)
  }
  # otherwise, remove markdown syntax from headers
  # only removing from header and spanning header, as this is where default markdown
  # formatting is placed in a gtsummary object
  else {
    x$table_styling$header <-
      x$table_styling$header %>%
      dplyr::mutate(
        label = .strip_markdown(.data$label),
        spanning_header = .strip_markdown(.data$spanning_header)
      )
  }

  # getting kable calls
  kable_extra_calls <-
    table_styling_to_kable_calls(
      x = x,
      escape = escape,
      format = format, ...
    )

  # adding id number for columns not hidden
  x$table_styling$header <-
    x$table_styling$header |>
    dplyr::group_by(.data$hide) |>
    dplyr::mutate(id = ifelse(.data$hide == FALSE, dplyr::row_number(), NA)) |>
    dplyr::ungroup()

  # kableExtra doesn't support markdown bold/italics, will replace in next section
  kable_extra_calls <-
    kable_extra_calls |>
    utils::modifyList(val = list(tab_style_bold = NULL, tab_style_italic = NULL))

  # escaping special characters in table_body ----------------------------------
  kable_call_index <- which(names(kable_extra_calls) %in% "kable")
  cols_to_escape <-
    dplyr::filter(x$table_styling$header, !.data$hide) |> dplyr::pull("column")
  kable_extra_calls <-
    append(kable_extra_calls,
           values = list(escape_table_body = NULL),
           after = kable_call_index - 1L
    )

  if (!isTRUE(escape) && isTRUE(addtl_fmt) && format %in% "latex") {
    # getting all unique column/rows where cell will be bold or italic
    df_text_format_collapsed <-
      x$table_styling$text_format |>
      dplyr::filter(.data$format_type %in% c("bold", "italic")) |>
      dplyr::select("column", "row_numbers") |>
      tidyr::unnest("row_numbers") |>
      dplyr::distinct() |>
      tidyr::nest(row_numbers = "row_numbers") |>
      dplyr::mutate(row_numbers = map(.data$row_numbers, ~ unlist(.) |> unname()))

    # expression identify the bold/italic cells. will be used in the `across()` below
    if (nrow(df_text_format_collapsed) > 0) {
      expr_no_escape <-
        map2(
          df_text_format_collapsed$column,
          df_text_format_collapsed$row_numbers,
          function(.x, .y) {
            expr((dplyr::cur_column() %in% !!.x & dplyr::row_number() %in% !!.y))
          }
        ) |>
        reduce(function(.x, .y) expr(!!.x | !!.y))
    } else {
      expr_no_escape <- expr(!!rep_len(FALSE, nrow(x$table_body)))
    } # no cells will be skipped if no bold/italic formatting


    # collapse header into fewer rows by align status
    df_header_by_align <-
      x$table_styling$header |>
      dplyr::filter(!.data$hide) |>
      dplyr::select("column", "align") |>
      tidyr::nest(column = "column") |>
      dplyr::mutate(column = map(.data$column, ~ unlist(.) |> unname()))

    # create one call per alignment type found in table
    kable_extra_calls[["escape_table_body"]] <-
      map(
        seq_len(nrow(df_header_by_align)),
        function(i) {
          rlang::expr(
            dplyr::mutate(
              dplyr::across
              (
                all_of(!!!df_header_by_align$column[i]) & where(is.character),
                ~ ifelse(
                  !!expr_no_escape,
                  .x,
                  gtsummary::.escape_latex(.x, align = !!str_sub(df_header_by_align$align[i], 1, 1))
                )
              )
            )
          )
        }
      ) |>
      compact()
  } else if (!isTRUE(escape) && isTRUE(addtl_fmt) && format %in% "html") {
    kable_extra_calls[["escape_table_body"]] <-
      rlang::expr(
        dplyr::mutate(
          dplyr::across(all_of(!!cols_to_escape) & where(is.character), gtsummary::.escape_html)
        )
      )
  }

  # bold and italic ------------------------------------------------------------
  kable_extra_calls <- .add_bold_italic_calls(kable_extra_calls, x, escape, ...)

  # add_indent -----------------------------------------------------------------
  df_indent <-
    x$table_styling$indent |>
    dplyr::filter(.data$column == "label")

  if (nrow(df_indent) > 0) {
    kable_extra_calls[["add_indent"]] <-
      map(
        seq_along(nrow(df_indent)),
        ~expr(
          kableExtra::add_indent(!!df_indent$row_numbers[[.x]],
                                 level_of_indent = !!(df_indent$n_spaces[[.x]] / 4L))
        )
      )
  }

  # add_header_above -----------------------------------------------------------
  if (any(!is.na(x$table_styling$header$spanning_header))) {
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
      dplyr::group_by(.data$spanning_header_id) %>%
      dplyr::mutate(width = dplyr::n()) %>%
      dplyr::distinct() %>%
      dplyr::ungroup()

    header <- df_header$width |> set_names(df_header$spanning_header)

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
    .number_footnotes(x) |>
    dplyr::pull("footnote") |>
    unique()

  if (length(vct_footnote > 0)) {
    kable_extra_calls[["footnote"]] <-
      expr(kableExtra::footnote(number = !!vct_footnote, escape = !!escape))
  }

  kable_extra_calls
}

.add_bold_italic_calls <- function(kable_extra_calls, x, escape, ...) {
  dots <- rlang::dots_list(...)

  # use `column_spec()` if `kable(escape = TRUE)` (the default) ----------------
  if (isTRUE(escape)) {
    df_bold_italic <-
      x$table_styling$text_format |>
      dplyr::filter(.data$format_type %in% c("bold", "italic")) |>
      dplyr::mutate(index = map(.data$row_numbers, ~ seq_len(nrow(x$table_body)) %in% .x)) |>
      dplyr::left_join(
        x$table_styling$header %>% select("column", "id"),
        by = "column"
      )

    df_bold <- df_bold_italic |> dplyr::filter(.data$format_type %in% "bold")
    df_italic <- df_bold_italic |> dplyr::filter(.data$format_type %in% "italic")

    kable_extra_calls[["bold_italic"]] <-
      c(
        # bold
        map(
          seq_len(nrow(df_bold)),
          ~ rlang::expr(kableExtra::column_spec(column = !!df_bold$id[.x], bold = !!df_bold$index[[.x]]))
        ),
        # italic
        map(
          seq_len(nrow(df_italic)),
          ~ rlang::expr(kableExtra::column_spec(column = !!df_italic$id[.x], italic = !!df_italic$index[[.x]]))
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
    x$table_styling$text_format |>
    dplyr::filter(.data$format_type %in% c("bold", "italic")) |>
    tidyr::unnest("row_numbers") %>%
    {
      dplyr::full_join(
        dplyr::filter(., .data$format_type %in% "bold") %>%
          dplyr::mutate(bold = TRUE) %>%
          dplyr::select("column", "row_numbers", "bold"),
        dplyr::filter(., .data$format_type %in% "italic") %>%
          dplyr::mutate(italic = TRUE) %>%
          dplyr::select("column", "row_numbers", "italic"),
        by = c("column", "row_numbers")
      )
    } |>
    dplyr::mutate(
      dplyr::across(all_of(c("bold", "italic")), ~ tidyr::replace_na(., FALSE))
    ) |>
    tidyr::nest(row_numbers = "row_numbers") |>
    dplyr::mutate(
      row_numbers = map(.data$row_numbers, ~ unlist(.x) |> unname())
    )

  # construct call to bold/italicize cells
  kable_extra_calls[["bold_italic"]] <-
    map(
      seq_len(nrow(df_bold_italic)),
      ~ expr(
        dplyr::mutate(
          dplyr::across(
            dplyr::all_of(!!df_bold_italic$column[.x]),
            function(xx) {
              ifelse(
                dplyr::row_number() %in% !!df_bold_italic$row_numbers[[.x]],
                kableExtra::cell_spec(
                  xx,
                  bold = !!df_bold_italic$bold[.x],
                  italic = !!df_bold_italic$italic[.x]
                ),
                xx
              )
            }
          )
        )
      )
    )

  return(kable_extra_calls)
}

# This function calls `kableExtra::linebreak()` on gtsummary headers and spanning headers.
# Note that `escape = FALSE` and `format = "latex"` are required.
#  - the default `align=` argument for columns is taken from the gtsummary object
#  - the headers, spanning headers, footnotes, and captions are escaped
#  - the markdown double-star and double-underscore bold syntax is converted to LaTeX, `\textbf{}`
#  - the markdown single-star and single-underscore italic syntax is converted to LaTeX, `\textit{}`
.latex_conversion_and_escaping <- function(x,
                                           align = NULL,
                                           linebreaker = "\n") {
  # set align argument ---------------------------------------------------------
  align <-
    align %||%
    str_sub(x$table_styling$header$align, 1, 1)

  # linebreak the headers ------------------------------------------------------
  x$table_styling$header$label <-
    .escape_latex(x$table_styling$header$label, newlines = FALSE) %>%
    .markdown_to_latex() %>%
    kableExtra::linebreak(
      align = align,
      linebreaker = linebreaker
    )

  x$table_styling$header$spanning_header <-
    .escape_latex2(x$table_styling$header$spanning_header, newlines = FALSE) %>%
    .markdown_to_latex2() %>%
    kableExtra::linebreak(
      align = "c",
      linebreaker = linebreaker,
      double_escape = TRUE
    )

  # removing line breaks from footnotes
  x$table_styling$footnote$footnote <-
    gsub("\\n", " ", x$table_styling$footnote$footnote) %>%
    .escape_latex2(newlines = FALSE) %>%
    .markdown_to_latex2()
  x$table_styling$footnote_abbrev$footnote <-
    gsub("\\n", " ", x$table_styling$footnote_abbrev$footnote) %>%
    .escape_latex2(newlines = FALSE) %>%
    .markdown_to_latex2()

  if (!is.null(x$table_styling$caption)) {
    x$table_styling$caption <-
      .escape_latex(x$table_styling$caption, newlines = FALSE) %>%
      .markdown_to_latex()
  }

  # return processed gtsummary table -------------------------------------------
  x
}

# this function does the following
# - strips markdown syntax from headers and spanning headers
# - escapes special characters from header, spanning header, caption, and footnotes
.html_conversion_and_escaping <- function(x) {
  x$table_styling$header$label <-
    .strip_markdown(x$table_styling$header$label) %>%
    .escape_html()
  x$table_styling$header$spanning_header <-
    .strip_markdown(x$table_styling$header$spanning_header) %>%
    .escape_html()

  # removing line breaks from footnotes
  x$table_styling$footnote$footnote <-
    gsub("\\n", " ", x$table_styling$footnote$footnote) %>%
    .escape_html()
  x$table_styling$footnote_abbrev$footnote <-
    gsub("\\n", " ", x$table_styling$footnote_abbrev$footnote) %>%
    .escape_html()

  if (!is.null(x$table_styling$caption)) {
    x$table_styling$caption <- .escape_html(x$table_styling$caption)
  }

  # return processed gtsummary table -------------------------------------------
  x
}

# this escapes the latex code in `knitr::kable(col.names=)`
.markdown_to_latex <- function(x) {
  x %>%
    # convert bold ** to \textbf{}
    str_replace_all(
      pattern = "\\*\\*(.*?)\\*\\*",
      replacement = "\\\\textbf{\\1}"
    ) %>%
    # convert bold __ to \textbf{}
    str_replace_all(
      pattern = "\\_\\_(.*?)\\_\\_",
      replacement = "\\\\textbf{\\1}"
    ) %>%
    # convert italic * to \textit{}
    str_replace_all(
      pattern = "\\*(.*?)\\*",
      replacement = "\\\\textit{\\1}"
    ) %>%
    # convert italic _ to \textit{}
    str_replace_all(
      pattern = "\\_(.*?)\\_",
      replacement = "\\\\textit{\\1}"
    ) %>%
    # convert underline ~~ to \underline{}
    str_replace_all(
      pattern = "\\~\\~(.*?)\\~\\~",
      replacement = "\\\\underline{\\1}"
    )
}

# this escapes the latex code in `kableExtra::add_header_row()`
.markdown_to_latex2 <- function(x) {
  x %>%
    # convert bold ** to \textbf{}
    str_replace_all(
      pattern = "\\*\\*(.*?)\\*\\*",
      replacement = "\\\\\\\\textbf{\\1}"
    ) %>%
    # convert bold __ to \textbf{}
    str_replace_all(
      pattern = "\\_\\_(.*?)\\_\\_",
      replacement = "\\\\\\\\textbf{\\1}"
    ) %>%
    # convert italic * to \textit{}
    str_replace_all(
      pattern = "\\*(.*?)\\*",
      replacement = "\\\\\\\\textit{\\1}"
    ) %>%
    # convert italic _ to \textit{}
    str_replace_all(
      pattern = "\\_(.*?)\\_",
      replacement = "\\\\\\\\textit{\\1}"
    ) %>%
    # convert underline ~~ to \underline{}
    str_replace_all(
      pattern = "\\~\\~(.*?)\\~\\~",
      replacement = "\\\\\\\\underline{\\1}"
    )
}

.strip_markdown <- function(x) {
  # strip bold **
  str_replace_all(
    string = x,
    pattern = "\\*\\*(.*?)\\*\\*",
    replacement = "\\1"
  )
}

#' Special Character Escape
#'
#' These utility functions were copied from the internals of kableExtra,
#' and assist in escaping special characters in LaTeX and HTML tables.
#' These function assist in the creations of tables via `as_kable_extra()`.
#'
#' @param x character vector
#' @return character vector with escaped special characters
#' @seealso `as_kable_extra()`
#' @name kableExtra_utils
#' @keywords internal
#'
#' @examples
#' .escape_latex(c("%", "{test}"))
#' .escape_html(c(">0.9", "line\nbreak"))
NULL

#' @rdname kableExtra_utils
#' @export
.escape_html <- function(x) {
  x <- gsub("&", "&amp;", x)
  x <- gsub("<", "&lt;", x)
  x <- gsub(">", "&gt;", x)
  x <- gsub("\"", "&quot;", x)
  x <- gsub("\n", "<br />", x)
  x
}

#' @rdname kableExtra_utils
#' @export
.escape_latex <- function(x, newlines = TRUE, align = "c") {
  x <- gsub("\\\\", "\\\\textbackslash", x)
  x <- gsub("([#$%&_{}])", "\\\\\\1", x)
  x <- gsub("\\\\textbackslash", "\\\\textbackslash{}", x)
  x <- gsub("~", "\\\\textasciitilde{}", x)
  x <- gsub("\\^", "\\\\textasciicircum{}", x)
  if (newlines) x <- kableExtra::linebreak(x, align = align)
  x <- gsub("  ", "\\\\ \\\\ ", x) # spaces

  x
}

#' @rdname kableExtra_utils
#' @export
.escape_latex2 <- function(x, newlines = TRUE, align = "c") {
  x <- gsub("\\\\", "\\\\\\\\textbackslash", x)
  x <- gsub("([#$%&_{}])", "\\\\\\\\\\1", x)
  x <- gsub("\\\\textbackslash", "\\\\\\\\textbackslash{}", x)
  x <- gsub("~", "\\\\\\\\textasciitilde{}", x)
  x <- gsub("\\^", "\\\\\\\\textasciicircum{}", x)
  if (newlines) x <- kableExtra::linebreak(x, align = align)
  x <- gsub("  ", "\\\\\\\\ \\\\\\\\ ", x) # spaces

  x
}
