#' Add table caption
#'
#' @param x a gtsummary object
#' @param caption the table caption
#' @inheritParams modify
#'
#' @section output types:
#' Captions are assigned based on output type.
#' - `gt::gt(caption=)`, available in gt version >0.2.2
#' - `flextable::set_caption(caption=)`
#' - `huxtable::set_caption(value=)`
#' - `knitr::kable(caption=)`
#'
#' @export
#' @examples
#' # Example 1 ----------------------------------
#' add_caption_ex1 <-
#'   trial %>%
#'   select(age, grade, trt) %>%
#'   tbl_summary(by = trt) %>%
#'   add_caption("Patient Characteristics")
#'
#' @section Example Output:
#'
#' \if{html}{Example 1}
#'
#' \if{html}{\figure{add_caption_ex1.png}{options: width=50\%}}

add_caption <- function(x, caption, text_interpret = c("md", "html")) {
  # checking inputs ------------------------------------------------------------
  if (!inherits(x, "gtsummary")) abort("`x=` must be class 'gtsummary'.")
  if (!rlang::is_string(caption)) abort("`caption=` must be a string.")
  text_interpret <- match.arg(text_interpret)

  # adding caption to gtsummary object ----------------------------------------
  x$list_output$caption <- caption
  attr(x$list_output$caption, "text_interpret") <- text_interpret

  # returning updated object ---------------------------------------------------
  x
}
