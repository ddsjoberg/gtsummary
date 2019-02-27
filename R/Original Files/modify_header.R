#' Modifies header rows for existing `fmt_table1`, `fmt_regression`, and
#' `fmt_uni_regression` objects
#'
#' An S3 generic function. If the top row of a header column is blank,
#' you may experience printing issues when using `knitr`::\code{\link[knitr]{kable}}
#'
#' @param x `fmt_table1`, `fmt_regression`, or `fmt_uni_regression` object
#' @param ... further arguments passed to or from other methods.
#' @seealso \code{\link{modify_header.fmt_table1}},
#' \code{\link{modify_header.fmt_regression}},
#' \code{\link{modify_header.fmt_uni_regression}}
#' @export
modify_header <- function(x, ...) UseMethod("modify_header")

#' Modifies header rows for existing `fmt_table1` objects.
#'
#' If the top row of a header column is blank,
#' you may experience printing issues when using `knitr`::\code{\link[knitr]{kable}}
#'
#' @param x `fmt_table1` object
#' @param label string vector including text to appear above the label column
#' @param stat_by string vector of text to include above the summary statistics
#' stratified by a variable.  The following fields are available for use in the
#' headers: `n`, `N`, `p`, `name`, and `level`.  `n` is the number of observations in
#' each by group. `N` is the total number of observations. `p` is the percentage of rows
#' in a by group. `name` is the name of the by variable. `level` is the by variable level.
#' Syntax follows the `glue::glue()` function, e.g. `stat_by = c("{level}", "N = {n} ({p}\%)")`.
#' Must specify `by` along with `stat_by`.
#' @param stat_overall string vector including text to appear above the overall summary
#' statistics. `N`, the total number of observations, is available for use in the
#' description.
#' @param pvalue string vector including text to appear above the p-value column
#' @param ...	further arguments passed to or from other methods
#' @export
#' @examples
#' trial %>% fmt_table1(by = "trt") %>% modify_header(stat_by = c("{level}", "N = {n} ({p}%)"))
modify_header.fmt_table1 <- function(x, label = NULL, stat_by = NULL,
                                     stat_overall = NULL, pvalue = NULL, ...) {

  # extracting the previous header
  old_header <-
    x$table1 %>%
    dplyr::filter_("startsWith(row_type, 'header')")

  # number of rows in previous header
  old_header_n <- nrow(old_header)

  # max length of header
  max_length <- max(
    old_header_n,
    length(label),
    length(stat_by),
    length(stat_overall),
    length(pvalue)
  )

  # lengthening inputs if needed to match
  if (!is.null(label)) label <- fill_blanks(label, max_length)
  if (!is.null(stat_by)) stat_by <- fill_blanks(stat_by, max_length)
  if (!is.null(stat_overall)) stat_overall <- fill_blanks(stat_overall, max_length)
  if (!is.null(pvalue)) pvalue <- fill_blanks(pvalue, max_length)

  # creating headers
  header_list <-
    create_header(
      data = x[["inputs"]][["data"]], by = x[["by"]],
      label = label, stat_by = stat_by,
      stat_overall = stat_overall, pvalue = pvalue
    )

  # creating new header
  header <-
    dplyr::data_frame(row_type = paste0("header", max_length:1)) %>%
    dplyr::left_join(old_header, by = "row_type") %>%
    dplyr::left_join(header_list[["row_type"]], by = "row_type")

  if (!is.null(header_list[["label"]])) {
    header <-
      header %>%
      dplyr::select(-dplyr::one_of("label")) %>%
      dplyr::bind_cols(
        header_list[["label"]]
      )
  }

  if (!is.null(header_list[["stat_by"]])) {
    header <-
      header %>%
      dplyr::select(-dplyr::starts_with("stat_by")) %>%
      dplyr::bind_cols(
        header_list[["stat_by"]]
      )
  }

  if (!is.null(header_list[["stat_overall"]])) {
    header <-
      header %>%
      dplyr::select(-dplyr::one_of("stat_overall")) %>%
      dplyr::bind_cols(
        header_list[["stat_overall"]]
      )
  }

  if (!is.null(header_list[["pvalue"]])) {
    header <-
      header %>%
      dplyr::select(-dplyr::one_of("pvalue")) %>%
      dplyr::bind_cols(
        header_list[["pvalue"]]
      )
  }

  # removing blank rows from top
  while (first_row_missing(header) == TRUE) {
    header <- header[-1, ]
  }

  # replacing old header with new
  x$table1 <-
    dplyr::bind_rows(
      header %>% dplyr::select(dplyr::one_of(names(x$table1))),
      x$table1 %>% dplyr::filter_("!startsWith(row_type, 'header')")
    )

  return(x)
}

#' Modifies header rows for existing `fmt_regression` objects.
#'
#' If the top row of a header column is blank,
#' you may experience printing issues when using `knitr`::\code{\link[knitr]{kable}}
#'
#' @param x `fmt_regression` object
#' @param label string vector including text to appear above the label column
#' @param est string vector including text to appear above the estimate/coefficient column
#' @param ci string vector including text to appear above the confidence interval column
#' @param pvalue string vector including text to appear above the p-value column
#' @param ...	further arguments passed to or from other methods
#' @export

modify_header.fmt_regression <- function(x, label = NULL, est = NULL,
                                         ci = NULL, pvalue = NULL, ...) {

  # extracting the previous header
  old_header <-
    x$model_tbl %>%
    dplyr::filter_("startsWith(row_type, 'header')")

  # number of rows in previous header
  old_header_n <- nrow(old_header)

  # max length of header
  max_length <- max(
    old_header_n,
    length(label),
    length(est),
    length(ci),
    length(pvalue)
  )

  # lengthening inputs if needed to match
  if (!is.null(label)) label <- fill_blanks(label, max_length)
  if (!is.null(est)) est <- fill_blanks(est, max_length)
  if (!is.null(ci)) ci <- fill_blanks(ci, max_length)
  if (!is.null(pvalue)) pvalue <- fill_blanks(pvalue, max_length)

  # creating headers
  header_list <-
    create_header(
      label = label, est = est,
      ci = ci, pvalue = pvalue
    )

  # creating new header
  header <-
    dplyr::data_frame(row_type = paste0("header", max_length:1)) %>%
    dplyr::left_join(old_header, by = "row_type") %>%
    dplyr::left_join(header_list[["row_type"]], by = "row_type")

  if (!is.null(header_list[["label"]])) {
    header <-
      header %>%
      dplyr::select(-dplyr::one_of("label")) %>%
      dplyr::bind_cols(
        header_list[["label"]]
      )
  }

  if (!is.null(header_list[["est"]])) {
    header <-
      header %>%
      dplyr::select(-dplyr::one_of("est")) %>%
      dplyr::bind_cols(
        header_list[["est"]]
      )
  }

  if (!is.null(header_list[["ci"]])) {
    header <-
      header %>%
      dplyr::select(-dplyr::one_of("ci")) %>%
      dplyr::bind_cols(
        header_list[["ci"]]
      )
  }

  if (!is.null(header_list[["pvalue"]])) {
    header <-
      header %>%
      dplyr::select(-dplyr::one_of("pvalue")) %>%
      dplyr::bind_cols(
        header_list[["pvalue"]]
      )
  }

  # removing blank rows from top
  while (first_row_missing(header) == TRUE) {
    header <- header[-1, ]
  }

  # replacing old header with new
  x$model_tbl <-
    dplyr::bind_rows(
      header %>% dplyr::select(dplyr::one_of(names(x$model_tbl))),
      x$model_tbl %>% dplyr::filter_("!startsWith(row_type, 'header')")
    )

  return(x)
}

# helper function that returns T or F, if first row is all NA or ""
first_row_missing <- function(x) {
  names(x) %>%
    setdiff("row_type") %>%
    purrr::map_lgl(~ x %>% dplyr::slice(1) %>% dplyr::pull(.x) %>% trimws() %in% c(NA, "")) %>%
    all()
}



#' Modifies header rows for existing `fmt_uni_regression` objects.
#'
#' If the top row of a header column is blank,
#' you may experience printing issues when using `knitr`::\code{\link[knitr]{kable}}
#'
#' @param x `fmt_uni_regression` object
#' @param label string vector including text to appear above the label column
#' @param N string vector including text to appear above the N column
#' @param est string vector including text to appear above the estimate/coefficient column
#' @param ci string vector including text to appear above the confidence interval column
#' @param pvalue string vector including text to appear above the p-value column
#' @param ...	further arguments passed to or from other methods
#' @export

modify_header.fmt_uni_regression <- function(x, label = NULL, N = NULL, est = NULL,
                                             ci = NULL, pvalue = NULL, ...) {

  # extracting the previous header
  old_header <-
    x$model_tbl %>%
    dplyr::filter_("startsWith(row_type, 'header')")

  # number of rows in previous header
  old_header_n <- nrow(old_header)

  # max length of header
  max_length <- max(
    old_header_n,
    length(label),
    length(est),
    length(ci),
    length(pvalue)
  )

  # lengthening inputs if needed to match
  if (!is.null(label)) label <- fill_blanks(label, max_length)
  if (!is.null(N)) N <- fill_blanks(N, max_length)
  if (!is.null(est)) est <- fill_blanks(est, max_length)
  if (!is.null(ci)) ci <- fill_blanks(ci, max_length)
  if (!is.null(pvalue)) pvalue <- fill_blanks(pvalue, max_length)

  # creating headers
  header_list <-
    create_header(
      label = label, est = est,
      N = N, ci = ci, pvalue = pvalue
    )

  # creating new header
  header <-
    dplyr::data_frame(row_type = paste0("header", max_length:1)) %>%
    dplyr::left_join(old_header, by = "row_type") %>%
    dplyr::left_join(header_list[["row_type"]], by = "row_type")

  if (!is.null(header_list[["label"]])) {
    header <-
      header %>%
      dplyr::select(-dplyr::one_of("label")) %>%
      dplyr::bind_cols(
        header_list[["label"]]
      )
  }

  if (!is.null(header_list[["N"]])) {
    header <-
      header %>%
      dplyr::select(-dplyr::one_of("N")) %>%
      dplyr::bind_cols(
        header_list[["N"]]
      )
  }

  if (!is.null(header_list[["est"]])) {
    header <-
      header %>%
      dplyr::select(-dplyr::one_of("est")) %>%
      dplyr::bind_cols(
        header_list[["est"]]
      )
  }

  if (!is.null(header_list[["ci"]])) {
    header <-
      header %>%
      dplyr::select(-dplyr::one_of("ci")) %>%
      dplyr::bind_cols(
        header_list[["ci"]]
      )
  }

  if (!is.null(header_list[["pvalue"]])) {
    header <-
      header %>%
      dplyr::select(-dplyr::one_of("pvalue")) %>%
      dplyr::bind_cols(
        header_list[["pvalue"]]
      )
  }

  # removing blank rows from top
  while (first_row_missing(header) == TRUE) {
    header <- header[-1, ]
  }

  # replacing old header with new
  x$model_tbl <-
    dplyr::bind_rows(
      header %>% dplyr::select(dplyr::one_of(names(x$model_tbl))),
      x$model_tbl %>% dplyr::filter_("!startsWith(row_type, 'header')")
    )

  return(x)
}

# helper function that returns T or F, if first row is all NA or ""
first_row_missing <- function(x) {
  names(x) %>%
    setdiff("row_type") %>%
    purrr::map_lgl(~ x %>% dplyr::slice(1) %>% dplyr::pull(.x) %>% trimws() %in% c(NA, "")) %>%
    all()
}
