#' Creates header rows for `fmt_table1` of `fmt_regression` objects
#'
#' This function takes in a data frame and information to include in the header
#' rows.  The user will rarely call this function directly, rather they will use a
#' wrapper for this function `modify_header()`.
#'
#' @param data data frame. `fmt_table1` only.
#' @param by string of column name in data. `fmt_table1` only.
#' @param mod regression model object. `fmt_regression` only.
#' @param label string vector including text to appear above the label column.
#' `fmt_table1` and `fmt_regression`.
#' @param stat_by string vector of text to include above the summary statistics
#' stratified by a variable.  The following fields are available for use in the
#' headers: `n`, `N`, `p`, `name`, `label`, and `level`.  `n` is the number of observations in
#' each by group. `N` is the total number of observations. `p` is the percentage of rows
#' in a by group. `name` is the name of the by variable. `level` is the by variable level.
#' Must specify 'by' along with 'stat_by'. `fmt_table1` only.
#' @param stat_overall string vector including text to appear above the overall summary
#' statistics. `N`, the total number of observations, is available for use in the
#' description. `fmt_table1` only
#' @param pvalue string vector including text to appear above the p-value column.
#' `fmt_table1` and `fmt_regression`.
#' @param N string vector including text to appear above the N column
#' @param est string vector including text to appear above the coefficient column.
#' `fmt_regression` only.
#' @param ci string vector including text to appear above the confidence interval
#' column. `fmt_regression` only.
#' @keywords internal
#' @examples
#' create_header(
#'   data = mtcars,
#'   stat_overall = c("Overall", "N = {N}"),
#'   pvalue = c("p-value", "")
#' )
#' create_header(
#'   data = mtcars, by = "am",
#'   stat_by = c("{level}", "N = {n}"),
#'   label = c("Variables", "")
#' )
#' @export

create_header <- function(data = NULL, by = NULL, mod = NULL, label = NULL,
                          stat_by = NULL, stat_overall = NULL, pvalue = NULL,
                          est = NULL, ci = NULL, N = NULL) {
  if (!is.null(data) & !is.null(mod)) {
    stop("Specify 'data' or 'mod', but not both")
  }
  if (is.null(data) & !is.null(by)) {
    stop("Specify 'data' when 'by' is specified")
  }
  if (!is.null(data) & sum(c(!is.null(est), !is.null(ci)))) {
    stop("'est' and 'ci' cannot be combined with 'data'")
  }
  if (!is.null(mod) & sum(c(!is.null(stat_by), !is.null(stat_overall)))) {
    stop("'stat_by' and 'stat_overall' cannot be combined with 'mod'")
  }


  results <- list()

  # row_type -------------------------------------------------------------------
  max_length <- max(
    length(label),
    length(stat_by),
    length(stat_overall),
    length(pvalue),
    length(est),
    length(ci)
  )

  results[["row_type"]] <-
    dplyr::data_frame(
      row_type = paste0("header", max_length:1)
    )

  # label ----------------------------------------------------------------------
  if (!is.null(label)) {
    results[["label"]] <-
      dplyr::data_frame(label = fill_blanks(label, max_length))
  }


  # stat_by --------------------------------------------------------------------
  if (!is.null(by) & !is.null(stat_by)) {
    results[["stat_by"]] <-
      data %>%
      dplyr::select(dplyr::one_of(by)) %>%
      dplyr::left_join(
        get_by_info(data, by) %>% dplyr::select(dplyr::one_of(c(by, "by_col"))),
        by = by
      ) %>%
      purrr::set_names("level", "by_col") %>%
      dplyr::count_(c("by_col", "level")) %>%
      dplyr::mutate_(
        name = ~by,
        label = ~ attr(data[[by]], "label") %||% NA_character_,
        N = ~ sum(n),
        p = ~ fmt_percent(n / N),
        row_type = ~ list(
          dplyr::data_frame(
            row_type = paste0("header", length(fill_blanks(stat_by, max_length)):1),
            stat_def = fill_blanks(stat_by, max_length)
          )
        )
      ) %>%
      tidyr::unnest_("row_type") %>%
      tidyr::nest("level", "name", "label", "n", "N", "p") %>%
      dplyr::mutate_(
        header = ~ purrr::map2(stat_def, data, ~ glue::glue_data(.y, .x) %>% as.character())
      ) %>%
      tidyr::unnest_(c("data", "header")) %>%
      dplyr::select(dplyr::one_of("row_type", "by_col", "header")) %>%
      tidyr::spread_("by_col", "header") %>%
      dplyr::arrange(dplyr::desc(!!rlang::sym("row_type"))) %>%
      dplyr::select(-dplyr::one_of("row_type"))
  }

  # stat_overall -------------------------------------------------------------
  if (!is.null(stat_overall)) {
    results[["stat_overall"]] <-
      dplyr::data_frame(stat_def = fill_blanks(stat_overall, max_length)) %>%
      dplyr::mutate_(
        row_type = ~ paste0("header", dplyr::n():1),
        N = ~ nrow(data)
      ) %>%
      tidyr::nest("N") %>%
      dplyr::mutate_(
        stat_overall = ~ purrr::map2(stat_def, data, ~ glue::glue_data(.y, .x) %>% as.character())
      ) %>%
      tidyr::unnest(data, stat_overall) %>%
      dplyr::select(dplyr::one_of(c("stat_overall")))
  }


  # pvalue--------------------------------------------------------------------
  if (!is.null(pvalue)) {
    results[["pvalue"]] <-
      dplyr::data_frame(pvalue = fill_blanks(pvalue, max_length))
  }

  # est ----------------------------------------------------------------------
  if (!is.null(est)) {
    results[["est"]] <-
      dplyr::data_frame(est = fill_blanks(est, max_length))
  }

  # ci ----------------------------------------------------------------------
  if (!is.null(ci)) {
    results[["ci"]] <-
      dplyr::data_frame(ci = fill_blanks(ci, max_length))
  }

  # N ----------------------------------------------------------------------
  if (!is.null(N)) {
    results[["N"]] <-
      dplyr::data_frame(N = fill_blanks(N, max_length))
  }

  # returning results --------------------------------------------------------
  return(results)
}
