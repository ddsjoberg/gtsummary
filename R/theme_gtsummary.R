#' Available gtsummary themes
#'
#' \lifecycle{maturing}
#' The following themes are available to use within the gtsummary package.
#' Print theme elements with `theme_gtsummary_journal(set_theme = FALSE) %>% print()`.
#' Review the [themes vignette](https://www.danieldsjoberg.com/gtsummary/articles/themes.html)
#' for details.
#'
#' @param set_theme Logical indicating whether to set the theme. Default is `TRUE`.
#' When `FALSE` the named list of theme elements is returned invisibly
#' @param font_size Numeric font size for compact theme.
#' Default is 13 for gt tables, and 8 for all other output types
#' @section Themes:
#' - `theme_gtsummary_journal(journal=)`
#'   - `"jama"` _The Journal of the American Medical Association_
#'       - Round large p-values to 2 decimal places; separate confidence intervals with `"ll to ul"`.
#'       - `tbl_summary()` Doesn't show percent symbol; use em-dash to separate IQR; run `add_stat_label()`
#'       - `tbl_regression()`/`tbl_uvregression()` show coefficient and CI in same column
#'   - `"lancet"` _The Lancet_
#'       - Use mid-point as decimal separator; round large p-values to 2 decimal places; separate confidence intervals with `"ll to ul"`.
#'       - `tbl_summary()` Doesn't show percent symbol; use em-dash to separate IQR
#'   - `"nejm"` _The New England Journal of Medicine_
#'       - Round large p-values to 2 decimal places; separate confidence intervals with `"ll to ul"`.
#'       - `tbl_summary()` Doesn't show percent symbol; use em-dash to separate IQR
#'   - `"qjecon"` _The Quarterly Journal of Economics_
#'       - `tbl_summary()` all percentages rounded to one decimal place
#'       - `tbl_regression()`/`tbl_uvregression()` add significance stars with `add_significance_stars()`; hides CI and p-value from output
#' - `theme_gtsummary_compact()`
#'   - tables printed with gt, flextable, kableExtra, or huxtable will be compact with smaller font size and reduced cell padding
#' - `theme_gtsummary_printer(print_engine=)`
#'   - Use this theme to permanently change the default printer.
#' - `theme_gtsummary_continuous2()`
#'   - Set all continuous variables to summary type `"continuous2"` by default
#' - `theme_gtsummary_mean_sd()`
#'   - Set default summary statistics to mean and standard deviation in `tbl_summary()`
#'   - Set default continuous tests in `add_p()` to t-test and ANOVA
#' - `theme_gtsummary_eda()`
#'   - Set all continuous variables to summary type `"continuous2"` by default
#'   - In `tbl_summary()` show the median, mean, IQR, SD, and Range by default
#'
#' Use `reset_gtsummary_theme()` to restore the default settings
#'
#' Review the [themes vignette](https://www.danieldsjoberg.com/gtsummary/articles/themes.html)
#' to create your own themes.
#' @examples
#' \donttest{
#' # Setting JAMA theme for gtsummary
#' theme_gtsummary_journal("jama")
#' # Themes can be combined by including more than one
#' theme_gtsummary_compact()
#'
#' set_gtsummary_theme_ex1 <-
#'   trial %>%
#'   select(age, grade, trt) %>%
#'   tbl_summary(by = trt) %>%
#'   as_gt()
#'
#' # reset gtsummary themes
#' reset_gtsummary_theme()
#' }
#' @section Example Output:
#' \if{html}{Example}
#'
#' \if{html}{\out{
#' `r man_create_image_tag(file = "set_gtsummary_theme_ex1.png", width = "60")`
#' }}
#' @name theme_gtsummary
#' @seealso [Themes vignette](https://www.danieldsjoberg.com/gtsummary/articles/themes.html)
#' @seealso [set_gtsummary_theme()], [reset_gtsummary_theme()]
NULL

# ------------------------------------------------------------------------------
#' @rdname theme_gtsummary
#' @export
#' @param journal String indicating the journal theme to follow. One of
#' `c("jama", "lancet", "nejm", "qjecon")`. Details below.
theme_gtsummary_journal <- function(journal = c("jama", "lancet", "nejm", "qjecon"),
                                    set_theme = TRUE) {
  journal <- match.arg(journal)
  if (journal == "jama") {
    lst_theme <-
      list(
        "pkgwide-str:theme_name" = "JAMA",
        "pkgwide-fn:pvalue_fun" = function(x) style_pvalue(x, digits = 2),
        "pkgwide-fn:prependpvalue_fun" = function(x) style_pvalue(x, digits = 2, prepend_p = TRUE),
        "pkgwide-str:ci.sep" = " to ",
        "style_number-arg:decimal.mark" = ".",
        "style_number-arg:big.mark" = ",",
        "add_stat_label-arg:location" = "row",
        "tbl_summary-str:continuous_stat" = "{median} ({p25} \U2013 {p75})",
        "tbl_summary-str:categorical_stat" = "{n} ({p})",
        "tbl_summary-fn:addnl-fn-to-run" = function(x) {
          add_stat_label(x)
        },
        "add_difference-fn:addnl-fn-to-run" = function(x) {
          # merging coef and CI columns, if error, returning x unaltered
          tryCatch(
            {
              new_header_text <-
                paste0(
                  x$table_styling$header %>% filter(.data$column == "estimate") %>% pull("label"),
                  " **(**",
                  x$table_styling$header %>% filter(.data$column == "ci") %>% pull("label"),
                  "**)**"
                )

              # adding CI footnote to any existing abbreviation footnote, e.g. for OR, HR, etc.
              estimate_footnote <-
                x$table_styling$footnote_abbrev %>%
                filter(.data$column %in% "estimate") %>%
                filter(dplyr::row_number() == dplyr::n(), !is.na(.data$footnote)) %>%
                dplyr::pull("footnote") %>%
                c("CI = Confidence Interval") %>%
                paste(collapse = ", ")
              x %>%
                # merge estimate and CI into one cell
                modify_table_styling(
                  columns = "estimate",
                  rows = !!expr(.data$variable %in% !!x$table_body$variable &
                    !is.na(.data$estimate)),
                  cols_merge_pattern = "{estimate} ({conf.low} to {conf.high})"
                ) %>%
                # hide ci column
                modify_column_hide(any_of("ci")) %>%
                # update column header
                modify_header(list(estimate = new_header_text)) %>%
                # add CI abbreviation footnote
                modify_footnote(estimate ~ estimate_footnote, abbreviation = TRUE)
            },
            error = function(e) x
          )
        },
        "tbl_regression-fn:addnl-fn-to-run" = function(x) {
          # merging coef and CI columns, if error, returning x unaltered
          tryCatch(
            {
              new_header_text <-
                paste0(
                  x$table_styling$header %>% filter(.data$column == "estimate") %>% pull("label"),
                  " **(", style_number(x$inputs$conf.level, scale = 100), "% CI)**"
                )

              # adding CI footnote to any existing abbreviation footnote, e.g. for OR, HR, etc.
              estimate_footnote <-
                x$table_styling$footnote_abbrev %>%
                filter(.data$column %in% "estimate") %>%
                filter(dplyr::row_number() == dplyr::n(), !is.na(.data$footnote)) %>%
                dplyr::pull("footnote") %>%
                c("CI = Confidence Interval") %>%
                paste(collapse = ", ")
              x %>%
                # merge estimate and CI into one cell
                modify_table_styling(
                  columns = "estimate",
                  rows = !!expr(.data$variable %in% !!x$table_body$variable &
                    !is.na(.data$estimate) &
                    !.data$reference_row %in% TRUE),
                  cols_merge_pattern = "{estimate} ({conf.low} to {conf.high})"
                ) %>%
                # hide ci column
                modify_column_hide(any_of("ci")) %>%
                # update column header
                modify_header(list(estimate = new_header_text)) %>%
                # add CI abbreviation footnote
                modify_footnote(estimate ~ estimate_footnote, abbreviation = TRUE)
            },
            error = function(e) x
          )
        }
      )
  } else if (journal == "nejm") {
    lst_theme <-
      list(
        "pkgwide-str:theme_name" = "New England Journal of Medicine",
        "pkgwide-fn:pvalue_fun" = function(x) style_pvalue(x, digits = 2),
        "pkgwide-fn:prependpvalue_fun" = function(x) style_pvalue(x, digits = 2, prepend_p = TRUE),
        "style_number-arg:decimal.mark" = ".",
        "style_number-arg:big.mark" = ",",
        "tbl_summary-str:continuous_stat" = "{median} ({p25} \U2013 {p75})",
        "tbl_summary-str:categorical_stat" = "{n} ({p})",
        "pkgwide-str:ci.sep" = " to "
      )
  } else if (journal == "lancet") {
    lst_theme <-
      list(
        "pkgwide-str:theme_name" = "The Lancet",
        "pkgwide-fn:pvalue_fun" =
          function(x, prepend_p = FALSE) {
            # this function assures that 5s are rounded up (and not to even, the default in `round()`)
            round2 <- function(x, digits = 0) {
              round(x + .Machine$double.eps * sign(x), digits = digits)
            }

            p_fmt <-
              dplyr::case_when(
                # allowing some leeway for numeric storage errors
                x > 1 + 1e-15 ~ NA_character_,
                x < 0 - 1e-15 ~ NA_character_,
                x > 0.99 ~ paste0(">", gtsummary::style_number(x = 0.99, digits = 2)),
                round2(x, digits = 2) >= 0.10 ~ gtsummary::style_number(x, digits = 2),
                round2(x, digits = 3) >= 0.01 ~ gtsummary::style_number(x, digits = 3),
                x >= 0.0001 ~ gtsummary::style_number(x, digits = 4),
                x < 0.0001 ~ paste0("<", gtsummary::style_number(x = 0.0001, digits = 4))
              )

            # prepending a p = in front of value
            if (prepend_p == TRUE) {
              p_fmt <- dplyr::case_when(
                is.na(p_fmt) ~ NA_character_,
                stringr::str_sub(p_fmt, end = 1L) %in% c("<", ">") ~ paste0("p", p_fmt),
                TRUE ~ paste0("p=", p_fmt)
              )
            }

            attributes(p_fmt) <- attributes(x)
            return(p_fmt)
          },
        "pkgwide-fn:prependpvalue_fun" =
          function(x, prepend_p = TRUE) {
            # this function assures that 5s are rounded up (and not to even, the default in `round()`)
            round2 <- function(x, digits = 0) {
              round(x + .Machine$double.eps * sign(x), digits = digits)
            }

            p_fmt <-
              dplyr::case_when(
                # allowing some leeway for numeric storage errors
                x > 1 + 1e-15 ~ NA_character_,
                x < 0 - 1e-15 ~ NA_character_,
                x > 0.99 ~ paste0(">", gtsummary::style_number(x = 0.99, digits = 2)),
                round2(x, digits = 2) >= 0.10 ~ gtsummary::style_number(x, digits = 2),
                round2(x, digits = 3) >= 0.01 ~ gtsummary::style_number(x, digits = 3),
                x >= 0.0001 ~ gtsummary::style_number(x, digits = 4),
                x < 0.0001 ~ paste0("<", gtsummary::style_number(x = 0.0001, digits = 4))
              )

            # prepending a p = in front of value
            if (prepend_p == TRUE) {
              p_fmt <- dplyr::case_when(
                is.na(p_fmt) ~ NA_character_,
                stringr::str_sub(p_fmt, end = 1L) %in% c("<", ">") ~ paste0("p", p_fmt),
                TRUE ~ paste0("p=", p_fmt)
              )
            }

            attributes(p_fmt) <- attributes(x)
            return(p_fmt)
          },
        "tbl_summary-str:continuous_stat" = "{median} ({p25} \U2013 {p75})",
        "style_number-arg:decimal.mark" =
          ifelse(.Platform$OS.type == "windows", special_char$interpunct, "\U00B7"),
        "style_number-arg:big.mark" = "\U2009",
        "pkgwide-str:ci.sep" = " to "
      )
  } else if (journal == "qjecon") {
    lst_theme <-
      list(
        "pkgwide-str:theme_name" = "The Quarterly Journal of Economics",
        "tbl_regression-arg:conf.int" = FALSE,
        "tbl_summary-fn:percent_fun" = function(x) style_number(x, digits = 1, scale = 100),
        "tbl_regression-fn:addnl-fn-to-run" = function(x) {
          new_header_text <-
            paste(
              x$table_styling$header %>% filter(.data$column == "estimate") %>% pull("label"),
              "**(SE)**",
              sep = "  \n"
            )

          estimate_footnote <-
            x$table_styling$footnote_abbrev %>%
            filter(.data$column %in% "estimate") %>%
            filter(dplyr::row_number() == dplyr::n(), !is.na(.data$footnote)) %>%
            dplyr::pull("footnote") %>%
            c("SE = Standard Error") %>%
            paste(collapse = ", ")

          x %>%
            add_significance_stars(
              pattern = "{estimate}{stars}  \n({std.error})",
              hide_se = TRUE
            ) %>%
            # update column header
            modify_header(list(estimate = new_header_text)) %>%
            # add SE abbreviation footnote
            modify_footnote(estimate ~ estimate_footnote, abbreviation = TRUE)
        },
        "as_gt-lst:addl_cmds" = list(
          tab_spanner = list(
            rlang::expr(gt::fmt_markdown(columns = everything())),
            rlang::expr(gt::tab_style(style = "vertical-align:top", locations = gt::cells_body(columns = dplyr::any_of("label"))))
          )
        )
      )
  }

  if (set_theme == TRUE) set_gtsummary_theme(lst_theme)
  return(invisible(lst_theme))
}

# ------------------------------------------------------------------------------
#' @rdname theme_gtsummary
#' @export
theme_gtsummary_compact <- function(set_theme = TRUE, font_size = NULL) {
  lst_theme <-
    list(
      "pkgwide-str:theme_name" = "Compact",
      # compact gt tables
      "as_gt-lst:addl_cmds" = list(
        tab_spanner = rlang::expr(
          gt::tab_options(
            table.font.size = !!(font_size %||% 13),
            data_row.padding = gt::px(1),
            summary_row.padding = gt::px(1),
            grand_summary_row.padding = gt::px(1),
            footnotes.padding = gt::px(1),
            source_notes.padding = gt::px(1),
            row_group.padding = gt::px(1)
          )
        )
      ),
      # compact flextables
      "as_flex_table-lst:addl_cmds" = list(
        valign = list(
          rlang::expr(flextable::fontsize(size = !!(font_size %||% 8), part = "all")),
          rlang::expr(flextable::padding(padding.top = 0, part = "all")),
          rlang::expr(flextable::padding(padding.bottom = 0, part = "all")),
          rlang::expr(flextable::set_table_properties(layout = "autofit"))
        )
      ),
      # compact huxtable
      "as_hux_table.gtsummary-lst:addl_cmds" = list(
        insert_row = list(
          rlang::expr(huxtable::set_font_size(value = !!(font_size %||% 8))),
          rlang::expr(huxtable::set_bottom_padding(value = 0)),
          rlang::expr(huxtable::set_top_padding(value = 0))
        )
      ),
      # compact kableExtra
      "as_kable_extra-lst:addl_cmds" = list(
        kable = list(
          rlang::expr(kableExtra::kable_styling(font_size = !!(font_size %||% 8)))
        )
      )
    )

  if (set_theme == TRUE) set_gtsummary_theme(lst_theme)
  return(invisible(lst_theme))
}

# ------------------------------------------------------------------------------
#' @rdname theme_gtsummary
#' @param print_engine String indicating the print method. Must be one of
#' `"gt"`, `"kable"`, `"kable_extra"`, `"flextable"`, `"tibble"`
#' @export
theme_gtsummary_printer <- function(print_engine = c("gt", "kable", "kable_extra", "flextable", "huxtable", "tibble"),
                                    set_theme = TRUE) {
  lst_theme <- list("pkgwide-str:print_engine" = match.arg(print_engine))

  if (set_theme == TRUE) set_gtsummary_theme(lst_theme)
  return(invisible(lst_theme))
}

# ------------------------------------------------------------------------------
#' @rdname theme_gtsummary
#' @param language String indicating language. Must be one of `"de"` (German),
#' `"en"` (English), `"es"` (Spanish), `"fr"` (French), `"gu"` (Gujarati),
#' `"hi"` (Hindi), `"is"` (Icelandic),`"ja"` (Japanese), `"kr"` (Korean),
#' `"nl"` (Dutch), `"mr"` (Marathi), `"no"` (Norwegian), `"pt"` (Portuguese),
#' `"se"` (Swedish), `"zh-c,n"` (Chinese Simplified), `"zh-tw"` (Chinese Traditional)
#'
#' If a language is missing a translation for a word or phrase, please feel free
#' to reach out on [GitHub](https://github.com/ddsjoberg/gtsummary/issues)
#' with the translated text!
#' @param iqr.sep string indicating separator for the default IQR in `tbl_summary()`.
#' If `decimal.mark=` is NULL, `iqr.sep=` is `", "`. The comma
#' separator, however, can look odd when `decimal.mark = ","`. In this case the argument
#' will default to an en dash
#' @param ci.sep string indicating separator for confidence intervals.
#' If `decimal.mark=` is NULL, `ci.sep=` is `", "`. The comma
#' separator, however, can look odd when `decimal.mark = ","`. In this case the argument
#' will default to an en dash
#' @inheritParams style_number
#' @export
theme_gtsummary_language <- function(language = c(
                                       "de", "en", "es", "fr", "gu", "hi", "is", "ja",
                                       "kr", "mr", "nl", "no", "pt", "se", "zh-cn", "zh-tw"
                                     ),
                                     decimal.mark = NULL, big.mark = NULL,
                                     iqr.sep = NULL,
                                     ci.sep = NULL,
                                     set_theme = TRUE) {
  language <- match.arg(language)
  ret <- list(
    "pkgwide-str:theme_name" = paste("language:", language),
    "pkgwide-str:language" = language
  )

  # setting formatting of numbers
  if (!is.null(decimal.mark)) ret <- c(ret, list("style_number-arg:decimal.mark" = decimal.mark))
  if (!is.null(big.mark)) ret <- c(ret, list("style_number-arg:big.mark" = big.mark))

  # setting themes for separators
  if (is.null(iqr.sep) && identical(decimal.mark, ",")) {
    iqr.sep <- " \U2013 "
  }
  if (!is.null(iqr.sep)) {
    ret <- c(ret, list(
      "tbl_summary-str:continuous_stat" =
        paste0("{median} ({p25}", iqr.sep, "{p75})")
    ))
  }

  if (is.null(ci.sep) && identical(decimal.mark, ",")) {
    ci.sep <- " \U2013 "
  }
  if (!is.null(ci.sep)) ret <- c(ret, list("pkgwide-str:ci.sep" = ci.sep))

  # either returning list OR setting theme and returning list
  if (set_theme == TRUE) set_gtsummary_theme(ret)
  return(invisible(ret))
}

# ------------------------------------------------------------------------------
#' @rdname theme_gtsummary
#' @param statistic Default statistic continuous variables
#' @export
theme_gtsummary_continuous2 <- function(statistic = "{median} ({p25, {p75})", set_theme = TRUE) {
  lst_theme <- list(
    "tbl_summary-str:default_con_type" = "continuous2",
    "tbl_summary-str:continuous_stat" = statistic
  )

  if (set_theme == TRUE) set_gtsummary_theme(lst_theme)
  return(invisible(lst_theme))
}

# ------------------------------------------------------------------------------
#' @rdname theme_gtsummary
#' @param statistic Default statistic continuous variables
#' @export
theme_gtsummary_mean_sd <- function(set_theme = TRUE) {
  lst_theme <- list(
    "tbl_summary-str:continuous_stat" = "{mean} ({sd})",
    "add_p.tbl_summary-attr:test.continuous_by2" = "t.test",
    "add_p.tbl_summary-attr:test.continuous" = "aov",
    "add_p.tbl_svysummary-attr:test.continuous" = "svy.t.test"
  )

  if (set_theme == TRUE) set_gtsummary_theme(lst_theme)
  return(invisible(lst_theme))
}

# ------------------------------------------------------------------------------
#' @rdname theme_gtsummary
#' @export
theme_gtsummary_eda <- function(set_theme = TRUE) {
  lst_theme <- list(
    "pkgwide-str:theme_name" = "Exploratory Data Analysis",
    "tbl_summary-str:default_con_type" = "continuous2",
    "tbl_summary-str:continuous_stat" =
      c("{median} ({p25}, {p75})", "{mean} ({sd})", "{min}, {max}"),
    "tbl_summary-fn:percent_fun" = function(x) style_percent(x, digits = 1)
  )

  if (set_theme == TRUE) set_gtsummary_theme(lst_theme)
  return(invisible(lst_theme))
}
