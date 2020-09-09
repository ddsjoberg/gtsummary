#' Available gtsummary themes
#'
#' \Sexpr[results=rd, stage=render]{lifecycle::badge("experimental")}
#' The following themes are available to use within the gtsummary package.
#' Use the [set_gtsummary_theme()] function to set a theme.
#'
#' @param set_theme Logical indicating whether to set the theme. Default is `TRUE`.
#' When `FALSE` the named list of theme elements is returned invisibly.
#' @param journal String indicating the journal theme to follow.
#'  - `"jama"` Journal of the American Medical Association
#' @param print_engine String indicating the print method. Must be one of
#' `"gt"`, `"kable"`, `"kable_extra"`, `"flextable"`, `"tibble"`
#' @seealso [set_gtsummary_theme()]
#' @section Themes:
#' - `theme_gtsummary_journal(journal=)`
#'   - `"jama"`
#'     - sets theme to align with the JAMA reporting guidelines
#'     - large p-values are rounded to two decimal places
#'     - in `tbl_summary()` the IQR is separated with a dash, rather than comma
#'     - in `tbl_summary()` the percent symbol is not printed next to percentages
#'   - `"lancet"`
#'     - sets theme to align with the The Lancet reporting guidelines
#'     - large p-values are rounded to two decimal places
#'     - in `tbl_summary()` the IQR is separated with a dash, rather than comma
#'     - confidence intervals are separated with `4.5 to 7.8`, rather than a comma
#' - `theme_gtsummary_compact()`
#'   - tables printed with gt or flextable will be compact with smaller font size and reduced cell padding
#' - `theme_gtsummary_printer(print_engine=)`
#'   - `"gt"` sets the gt package as the default print engine
#'   - `"kable"` sets the `knitr::kable()` function as the default print engine
#'   - `"flextable"` sets the flextable package as the default print engine
#'   - `"kable_extra"` sets the kableExtra package as the default print engine
#' - `theme_gtsummary_continuous2()`
#'   - Set all continuous variables to summary type `"continuous2"` by default
#'   - Use the `statistic=` argument to set the default continuous variable summary statistics
#'
#' Use `reset_gtsummary_theme()` to restore the default settings
#'
#' Review the [themes vignette](http://www.danieldsjoberg.com/gtsummary/articles/themes.html)
#' to create your own themes.
#' @examples
#' # Setting JAMA theme for gtsummary
#' theme_gtsummary_journal("jama")
#' # Themes can be combined by including more than one
#' theme_gtsummary_compact()
#'
#' set_gtsummary_theme_ex1 <-
#'   trial %>%
#'   dplyr::select(age, grade, trt) %>%
#'   tbl_summary(by = trt) %>%
#'   add_stat_label() %>%
#'   as_gt()
#'
#' # reset gtsummary themes
#' reset_gtsummary_theme()
#' @section Example Output:
#' \if{html}{Example}
#'
#' \if{html}{\figure{set_gtsummary_theme_ex1.png}{options: width=70\%}}
#' @name theme_gtsummary
#' @seealso [Themes vignette](http://www.danieldsjoberg.com/gtsummary/articles/themes.html)
#' @seealso `set_gtsummary_theme()`, `reset_gtsummary_theme()`
NULL

# ------------------------------------------------------------------------------
#' @rdname theme_gtsummary
#' @export
theme_gtsummary_journal <- function(journal = c("jama", "lancet"), set_theme = TRUE) {
  journal <- match.arg(journal)
  if (journal == "jama") {
    lst_theme <-
      list(
        "pkgwide-str:theme_name" = "JAMA",
        "pkgwide-fn:pvalue_fun" = function(x) style_pvalue(x, digits = 2),
        "pkgwide-fn:prependpvalue_fun" = function(x) style_pvalue(x, digits = 2, prepend_p = TRUE),
        "style_number-arg:decimal.mark" = ".",
        "style_number-arg:big.mark" = ",",
        "add_stat_label-arg:location" = "row",
        "tbl_summary-str:continuous_stat" = "{median} ({p25} \U2013 {p75})",
        "tbl_summary-str:categorical_stat" = "{n} ({p})"
      )
  }
  else if (journal == "lancet") {
    lst_theme <-
      list(
        "pkgwide-str:theme_name" = "The Lancet",
        "pkgwide-fn:pvalue_fun" = function(x) style_pvalue(x, digits = 2),
        "pkgwide-fn:prependpvalue_fun" = function(x) style_pvalue(x, digits = 2, prepend_p = TRUE),
        "tbl_summary-str:continuous_stat" = "{median} ({p25} \U2013 {p75})",
        # "style_number-arg:decimal.mark" = "\U00B7", # i am not sure why this does not work
        "style_number-arg:decimal.mark" = ".",
        "style_number-arg:big.mark" = "\U2009",
        "pkgwide-str:ci.sep" = " to "
      )
  }



  if (set_theme == TRUE) set_gtsummary_theme(lst_theme)
  return(invisible(lst_theme))
}

# ------------------------------------------------------------------------------
#' @rdname theme_gtsummary
#' @export

theme_gtsummary_compact <- function(set_theme = TRUE){
  lst_theme <-
    list(
      "pkgwide-str:theme_name" = "Compact",
      # compact gt tables
      "as_gt-lst:addl_cmds" = list(
        tab_spanner = rlang::expr(
          gt::tab_options(table.font.size = 'small',
                          data_row.padding = gt::px(1),
                          summary_row.padding = gt::px(1),
                          grand_summary_row.padding = gt::px(1),
                          footnotes.padding = gt::px(1),
                          source_notes.padding = gt::px(1),
                          row_group.padding = gt::px(1))
        )
      ),
      # compact flextables
      "as_flex_table-lst:addl_cmds" = list(
        valign = list(
          rlang::expr(flextable::fontsize(size = 8, part = "all")),
          rlang::expr(flextable::padding(padding.top = 0, part = "all")),
          rlang::expr(flextable::padding(padding.bottom = 0, part = "all"))
        )
      ),
      # compact huxtable
      "as_hux_table.gtsummary-lst:addl_cmds" = list(
        insert_row = list(
          rlang::expr(huxtable::set_font_size(value = 8)),
          rlang::expr(huxtable::set_bottom_padding(value = 0)),
          rlang::expr(huxtable::set_top_padding(value = 0))
        )
      ),
      # compact kableExtra
      "as_kable_extra-lst:addl_cmds" = list(
        kable = list(
          rlang::expr(kableExtra::kable_styling(font_size = 8))
        )
      )
    )

  if (set_theme == TRUE) set_gtsummary_theme(lst_theme)
  return(invisible(lst_theme))
}

# ------------------------------------------------------------------------------
#' @rdname theme_gtsummary
#' @param print_engine String indicating the print engine. Default is `"gt"`
#' @export
theme_gtsummary_printer <- function(
  print_engine = c("gt", "kable", "kable_extra", "flextable", "huxtable", "tibble"),
  set_theme = TRUE) {

  lst_theme <- list("pkgwide-str:print_engine" = match.arg(print_engine))

  if (set_theme == TRUE) set_gtsummary_theme(lst_theme)
  return(invisible(lst_theme))
}

# ------------------------------------------------------------------------------
#' @rdname theme_gtsummary
#' @param language String indicating language. Must be one of
#' - `"de"` (German)
#' - `"en"` (English)
#' - `"es"` (Spanish)
#' - `"fr"` (French)
#' - `"gu"` (Gujarati)
#' - `"hi"` (Hindi)
#' - `"ja"` (Japanese)
#' - `"mr"` (Marathi)
#' - `"pt"` (Portuguese)
#' - `"se"` (Swedish)
#' - `"zh-cn"` Chinese (Simplified)
#' - `"zh-tw"` Chinese (Traditional)
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
theme_gtsummary_language <- function(language = c("de", "en", "es", "fr", "gu", "hi", "ja",
                                                  "mr", "pt", "se", "zh-cn", "zh-tw"),
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
  if (is.null(iqr.sep) && identical(decimal.mark, ","))
    iqr.sep <- " \U2013 "
  if (!is.null(iqr.sep))
    ret <- c(ret, list("tbl_summary-str:continuous_stat" =
                         paste0("{median} ({p25}", iqr.sep, "{p75})")))

  if (is.null(ci.sep) && identical(decimal.mark, ","))
    ci.sep <- " \U2013 "
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
