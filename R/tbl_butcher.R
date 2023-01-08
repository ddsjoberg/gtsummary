#' Reduce size of gtsummary objects
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
#' @param x a gtsummary object
#' @param include names of additional elements to retain in the gtsummary
#' object. `c("table_body", "table_styling")` will always be retained.
#' Default is `NULL`
#'
#' @return a gtsummary object
#' @export
#'
#' @examples
#' \donttest{
#' tbl_large <-
#'   trial %>%
#'   tbl_uvregression(
#'     y = age,
#'     method = lm
#'   )
#'
#' tbl_butchered <-
#'   tbl_large %>%
#'   tbl_butcher()
#'
#' # size comparison
#' object.size(tbl_large) %>% format(units = "Mb")
#' object.size(tbl_butchered) %>% format(units = "Mb")
#' }
tbl_butcher <- function(x, include = NULL) {
  if (!inherits(x, "gtsummary") || is.null(x$table_styling)) {
    stop("`x=` must be a gtsummary object created with v1.4.0 or later.")
  }

  # select additional elements to keep in output -------------------------------
  include <-
    .select_to_varnames(
      select = {{ include }},
      var_info = names(x),
      arg_name = "include"
    )

  # elements to be removed from `x=` -------------------------------------------
  element_names <-
    names(x) %>%
    setdiff(union(c("table_body", "table_styling"), include))
  lst_nulls <-
    rep_len(list(NULL), length(element_names)) %>%
    rlang::set_names(element_names)

  # remove elements and returned reduced object --------------------------------
  utils::modifyList(x, val = lst_nulls)
}
