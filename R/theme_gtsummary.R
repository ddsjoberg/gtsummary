#' Available gtsummary themes
#'
#' \Sexpr[results=rd, stage=render]{lifecycle::badge("experimental")}
#' The following themes are available to use within the gtsummary package.
#' Use the [set_gtsummary_theme()] function to set a theme.
#'
#' @param journal String indicating the journal theme to follow.
#'  - `"jama"` Journal of the American Medical Association
#' @param print_engine String indicating the print method. Must be one of
#' `"gt"`, `"kable"`, `"kable_extra"`, `"flextable"`, `"huxtable"`, `"tibble"`
#' @seealso [set_gtsummary_theme()]
#' @section Themes:
#' - `theme_gtsummary_journal(journal=)`
#'   - `journal = "jama"`
#'     - sets theme to align with the JAMA reporting guidelines
#'     - large p-values are rounded to two decimal places
#'     - in `tbl_summary()` the IQR is separated with a dash, rather than comma
#'     - in `tbl_summary()` the percent symbol is not printed next to percentages
#' - `theme_gtsummary_compact()`
#'   - tables printed with gt, flextable, and huxtable will be compact with smaller font size and reduced cell padding
#' - `theme_gtsummary_printer(print_engine=)`
#'   - `"gt"` sets the gt package as the default print engine
#'   - `"kable"` sets the `knitr::kable()` function as the default print engine
#'   - `"flextable"` sets the flextable package as the default print engine
#'   - `"kable_extra"` sets the kableExtra package as the default print engine
#'   - `"huxtable"` sets the huxtable package as the default print engine
#'
#' Use `reset_gtsummary_theme()` to restore the default settings
#'
#' Review the [themes vignette](http://www.danieldsjoberg.com/gtsummary/dev/articles/themes.html)
#' to create your own themes.
#' @examples
#' # Setting JAMA theme for gtsummary
#' set_gtsummary_theme(theme_gtsummary_journal("jama"))
#' # Themes can be combined by including more than one
#' set_gtsummary_theme(theme_gtsummary_compact())
#'
#' set_gtsummary_theme_ex1 <-
#'   trial %>%
#'   dplyr::select(age, grade, trt) %>%
#'   tbl_summary(by = trt) %>%
#'   add_stat_label() %>%
#'   as_gt()
#'
#' # reset gtsummary theme
#' reset_gtsummary_theme()
#' @section Example Output:
#' \if{html}{Example}
#'
#' \if{html}{\figure{set_gtsummary_theme_ex1.png}{options: width=70\%}}
#' @name theme_gtsummary
NULL

# ------------------------------------------------------------------------------
#' @rdname theme_gtsummary
#' @export
theme_gtsummary_journal <- function(journal = "jama") {
  journal <- match.arg(journal)
  if (journal == "jama") {
    lst_theme <-
      list(
        "pkgwide-str:theme_name" = "JAMA",
        "pkgwide-fn:pvalue_fun" = function(x) style_pvalue(x, digits = 2),
        "pkgwide-fn:prependpvalue_fun" = function(x) style_pvalue(x, digits = 2, prepend_p = TRUE),
        "add_stat_label-arg:location" = "row",
        "tbl_summary-str:continuous_stat" = "{median} ({p25} - {p75})",
        "tbl_summary-str:categorical_stat" = "{n} ({p})"
      )
  }

  return(lst_theme)
}

# ------------------------------------------------------------------------------
#' @rdname theme_gtsummary
#' @export
theme_gtsummary_compact <- function(){
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
    "as_flextable.gtsummary-lst:addl_cmds" = list(
      footnote = list(
        rlang::expr(flextable::fontsize(size = 8, part = "all")),
        rlang::expr(flextable::padding(padding.top = 0, part = "all")),
        rlang::expr(flextable::padding(padding.bottom = 0, part = "all"))
      )
    ),
    # compact huxtable
    "as_huxtable.gtsummary-lst:addl_cmds" = list(
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
}

# ------------------------------------------------------------------------------
#' @rdname theme_gtsummary
#' @param print_engine String indicating the print engine. Default is `"gt"`
#' @export
theme_gtsummary_printer <- function(
  print_engine = c("gt", "kable", "kable_extra", "flextable", "huxtable", "tibble")) {

  list("pkgwide-str:print_engine" = match.arg(print_engine))
}
