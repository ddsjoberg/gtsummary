#' Create footnotes for individual p-values
#'
#' The usual presentation of footnotes for p-values on a gtsummary table is
#' to have a single footnote that lists all statistical tests that were used to
#' compute p-values on a given table. The `separate_p_footnotes` function
#' separates aggregated p-value footnotes to individual footnotes that denote
#' the specific test used for each of the p-values.
#'
#' @param x object with class `"tbl_summary"` created from the gtsummary package
#'
#' @export
#'
#' @examples
#' library(gtsummary)
#' separate_p_footnotes_ex1 <-
#'   trial %>%
#'   select(trt, age, grade) %>%
#'   tbl_summary(by = trt) %>%
#'   add_p() %>%
#'   separate_p_footnotes()
#'
#' @section Example Output:
#' \if{html}{\figure{separate_p_footnotes_ex1.png}{options: width=80\%}}

separate_p_footnotes <- function(x) {
  # check inputs ---------------------------------------------------------------
  if (!inherits(x, "gtsummary") || !"p.value" %in% names(x$table_body))
    stop("x must be a gtsummary table with a p-value column.", call. = FALSE)
  if (!"stat_test_lbl" %in% names(x$meta_data))
    stop("The `x$meta_data` data frame must have a column called 'stat_test_lbl'.")


  # remove p-value column footnote ---------------------------------------------
  x <- gtsummary::modify_footnote(x, p.value ~ NA_character_)

  # add footnotes to the body of the table -------------------------------------
  footnote_calls <-
    x$meta_data %>%
    dplyr::select(.data$variable, .data$stat_test_lbl) %>%
    tibble::deframe() %>%
    purrr::imap(
      ~rlang::expr(
        gtsummary::modify_table_styling(columns = "p.value",
                                        rows = .data$variable %in% !!.y & !is.na(.data$p.value),
                                        footnote = !!.x)
      )
    )

  # concatenating expressions with %>% between each of them
  footnote_calls %>%
    purrr::reduce(function(x, y) rlang::expr(!!x %>% !!y), .init = rlang::expr(!!x)) %>%
    # evaluating expressions
    eval()
}
