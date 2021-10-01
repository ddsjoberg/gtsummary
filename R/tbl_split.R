#' Split gtsummary table
#'
#' The `tbl_split` function splits a single gtsummary table into multiple tables
#'
#' @param x gtsummary table
#' @param variables variables at which to split the gtsummary table rows (tables
#'  will be separated after each of these variables)
#' @param ... not used
#'
#' @return `tbl_split` object
#' @name tbl_split
#'
#' @family gtsummary-related functions
#' @examples
#' library(gtsummary)
#'
#' tbl <-
#'   tbl_summary(trial) %>%
#'   tbl_split(variables = c(marker, grade))
#'
#' @export
#' @rdname tbl_split
tbl_split <- function(x, variables) {
  # check/parse inputs ---------------------------------------------------------
  if (!inherits(x, "gtsummary"))
    stop("`x=` must be class 'gtsummary'", call. = FALSE)

  variables <-
    broom.helpers::.select_to_varnames(
      {{ variables }},
      var_info = x$table_body,
      arg_name = "variable"
    ) %>%
    # adding last variable
    union(x$table_body$variable %>% rev() %>% purrr::pluck(1))

  # merging split points -------------------------------------------------------
  # convert list of table_body into list of gtsummary obejcts
  x$table_body %>%
    dplyr::left_join(
      tibble(variable = variables, ..group.. = variables),
      by = "variable"
    ) %>%
    tidyr::fill(.data$..group.., .direction = "up") %>%
    tidyr::nest(data = -.data$..group..) %>%
    dplyr::pull(.data$data) %>%
    purrr::map(
      ~list(.) %>%
        purrr::set_names("table_body") %>%
        c(purrr::list_modify(x, "table_body" = NULL)) %>% # add the other parts of the gtsummary table
        `class<-`(class(x)) # add original class from `x`
    ) %>%
    `class<-`("tbl_split") # assign class (can't assign gtsummary becuase of print definitions)
}

print.tbl_split <- function(x, ...) {
  purrr::walk(x, print)
}
