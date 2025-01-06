#' Modify table caption
#'
#' @description
#' Captions are assigned based on output type.
#' - `gt::gt(caption=)`
#' - `flextable::set_caption(caption=)`
#' - `huxtable::set_caption(value=)`
#' - `knitr::kable(caption=)`
#'
#' @param x (`gtsummary`)\cr
#'   A gtsummary object
#' @param caption (`string`/`character`)\cr
#'   A string for the table caption/title. NOTE: The `gt` print engine supports a
#'   vector of captions. But not every print engine supports this feature, and
#'   for those outputs, only a string is accepted.
#' @inheritParams modify
#'
#' @export
#' @return Updated gtsummary object
#'
#' @examples
#' trial |>
#'   tbl_summary(by = trt, include = c(marker, stage)) |>
#'   modify_caption(caption = "**Baseline Characteristics** N = {N}")
modify_caption <- function(x, caption, text_interpret = c("md", "html")) {
  set_cli_abort_call()
  updated_call_list <- c(x$call_list, list(modify_footnote = match.call()))

  # checking inputs ------------------------------------------------------------
  check_class(x, "gtsummary")
  check_class(caption, "character")
  text_interpret <- arg_match(text_interpret)

  # evaluating update with glue ------------------------------------------------
  if ("label" %in% x$table_styling$header$column) {
    caption <- map(caption, ~.evaluate_string_with_glue(x, list(label = .x))) |>
      unlist() |>
      unname()
  }

  # adding caption to gtsummary object ----------------------------------------
  x$table_styling$caption <- caption
  attr(x$table_styling$caption, "text_interpret") <- text_interpret

  # returning updated object ---------------------------------------------------
  x$call_list <- updated_call_list
  x
}
