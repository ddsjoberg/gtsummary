#' Create footnotes for individual p-values
#'
#' \lifecycle{experimental}
#' The usual presentation of footnotes for p-values on a gtsummary table is
#' to have a single footnote that lists all statistical tests that were used to
#' compute p-values on a given table. The `separate_p_footnotes()` function
#' separates aggregated p-value footnotes to individual footnotes that denote
#' the specific test used for each of the p-values.
#'
#' @param x object with class `"tbl_summary"` or `"tbl_svysummary"`
#'
#' @export
#' @family tbl_summary tools
#' @family tbl_svysummary tools
#' @seealso Review [list, formula, and selector syntax][syntax] used throughout gtsummary
#' @examples
#' # Example 1 ----------------------------------
#' separate_p_footnotes_ex1 <-
#'   trial %>%
#'   select(trt, age, grade) %>%
#'   tbl_summary(by = trt) %>%
#'   add_p() %>%
#'   separate_p_footnotes()
#' @section Example Output:
#'
#' \if{html}{Example 1}
#'
#' \if{html}{\out{
#' `r man_create_image_tag(file = "separate_p_footnotes_ex1.png", width = "80")`
#' }}

separate_p_footnotes <- function(x) {
  # check inputs ---------------------------------------------------------------
  .assert_class(x, "gtsummary")
  if (!"p.value" %in% names(x$table_body)) {
    stop("`x=` must be a gtsummary table with a p-value column.", call. = FALSE)
  }
  if (!"stat_test_lbl" %in% names(x$meta_data)) {
    stop("The `x$meta_data` data frame must have a column called 'stat_test_lbl'.")
  }


  # remove p-value column footnote ---------------------------------------------
  x <- gtsummary::modify_footnote(x, p.value ~ NA_character_)

  # add footnotes to the body of the table -------------------------------------
  footnote_calls <-
    x$meta_data %>%
    dplyr::select("variable", "stat_test_lbl") %>%
    tibble::deframe() %>%
    map(translate_text) %>%
    purrr::imap(
      ~ rlang::expr(
        gtsummary::modify_table_styling(
          columns = "p.value",
          rows = .data$variable %in% !!.y & !is.na(.data$p.value),
          footnote = !!.x
        )
      )
    )

  # concatenating expressions with %>% between each of them
  footnote_calls %>%
    purrr::reduce(function(x, y) rlang::expr(!!x %>% !!y), .init = rlang::expr(!!x)) %>%
    # evaluating expressions
    eval()
}
