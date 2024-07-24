#' Butcher table
#'
#' Some gtsummary objects can become large and the size becomes cumbersome
#' when working with the object.
#' The function removes all elements from a gtsummary object, except those
#' required to print the table. This may result in gtsummary functions
#' that add information or modify the table, such as `add_global_p()`,
#' will no longer execute
#' after the excess elements have been removed (aka butchered). Of note,
#' the majority of `inline_text()` calls will continue to execute
#' properly.
#'
#' @param x (`gtsummary`)\cr
#'   a gtsummary object
#' @param include (`character`)\cr
#'   names of additional elements to retain in the gtsummary
#'   object. `c("table_body", "table_styling")` will always be retained.
#'
#' @return a gtsummary object
#' @export
#'
#' @examplesIf identical(Sys.getenv("NOT_CRAN"), "true") || identical(Sys.getenv("IN_PKGDOWN"), "true")
#' tbl_large <-
#'   trial |>
#'   tbl_uvregression(
#'     y = age,
#'     method = lm
#'   )
#'
#' tbl_butchered <-
#'   tbl_large |>
#'   tbl_butcher()
#'
#' # size comparison
#' object.size(tbl_large) |> format(units = "Mb")
#' object.size(tbl_butchered)|> format(units = "Mb")
tbl_butcher <- function(x, include = c("table_body", "table_styling")) {
  set_cli_abort_call()
  check_not_missing(x)
  check_class(x, "gtsummary")
  check_class(include, "character")

  # select additional elements to keep in output -------------------------------
  include <- union(c("table_body", "table_styling"), include) |> unique()
  include <- arg_match(include, values = names(x), multiple = TRUE)

  # remove elements and returned reduced object --------------------------------
  x |>
    utils::modifyList(
      val = rep_named(setdiff(names(x), include), list(NULL)),
      keep.null = FALSE
    )
}
