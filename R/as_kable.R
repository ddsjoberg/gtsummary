#' Convert gtsummary object to a kable object
#'
#' @description Output from [knitr::kable] is less full featured compared to
#' summary tables produced with [gt](https://gt.rstudio.com/index.html).
#' For example, kable summary tables do not include indentation, footnotes,
#' or spanning header rows.
#'
#' Line breaks (`\n`) are removed from column headers and table cells.
#'
#' @details Tip: To better distinguish variable labels and level labels when
#' indenting is not supported, try [bold_labels()] or [italicize_levels()].
#'
#' @inheritParams as_gt
#' @param x Object created by a function from the gtsummary package
#' (e.g. [tbl_summary] or [tbl_regression])
#' @inheritParams as_gt
#' @inheritParams as_tibble.gtsummary
#' @param ... Additional arguments passed to [knitr::kable]
#' @export
#' @return A `knitr_kable` object
#'
#' @author Daniel D. Sjoberg
#' @examples
#' trial |>
#'   tbl_summary(by = trt) |>
#'   bold_labels() |>
#'   as_kable()
as_kable <- function(x, ..., include = everything(), return_calls = FALSE) {
}
