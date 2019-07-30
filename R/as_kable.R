#' Convert to knitr_kable object
#'
#' Function converts object to a knitr_kable object.  This function is used in the
#' background when the results are printed or knit.  A user can use this function
#' if they wish to add customized formatting available via [knitr::kable]
#'
#' @param x object created by a function from the gtsummary package
#' (e.g. \code{\link{tbl_summary}} or \code{\link{tbl_regression}})
#' @param omit vector of named kable commands to omit. Default is `NULL`
#' @export
#' @seealso \link{tbl_summary} \link{tbl_regression} \link{tbl_uvregression}
#' @author Daniel D. Sjoberg
#' @examples
#' trial %>%
#' tbl_summary(by = "trt") %>%
#' as_kable()

as_kable <- function(x, omit = NULL) {
  # user cannot omit the first 'data' command
  omit <- omit %>% setdiff("kable")

  no_print_vars <- case_when(
    class(x) == "tbl_summary" ~
      list(names(x$table_body) %>%
             intersect(c("variable", "row_type"))),
    class(x) == "tbl_regression"  | (class(x) == "tbl_stack" & class(x$tbl_regression_list[[1]]) == "tbl_regression") ~
      list(names(x$table_body) %>%
             intersect(c("variable", "var_type", "row_ref",
                         "row_type", "N", "conf.high", "nevent"))),
    class(x) == "tbl_uvregression" | (class(x) == "tbl_stack" & class(x$tbl_regression_list[[1]]) == "tbl_uvregression") ~
      list(names(x$table_body) %>%
             intersect(c("variable", "var_type", "row_ref",
                         "row_type", "conf.high"))),
    class(x) == "tbl_survival" ~ list(names(x$table_body) %>%
                                        intersect(c("prob", "time", "strata", "n.risk", "n.event", "n", "n.event.tot",
                                                    "n.event.strata", "variable", "level", "conf.low", "conf.high"))),
    TRUE ~ list(character())
  ) %>%
    unlist()

  # tbl_merge not refactored for nive output yet.
  if (class(x) == "tbl_merge" || (class(x) == "tbl_stack" & class(x$tbl_regression_list[[1]]) == "tbl_merge")) {
    message(glue(
      "tbl_merge objects are not yet supported for 'knitr::kable()' output.\n",
      "For improved formatting install the gt package.\n",
      "'remotes::install_github(\"rstudio/gt\")'",
    ))
  }

  # saving vector of column labels
  col_labels <-
    tibble(
      column = names(x$table_body) %>% setdiff(no_print_vars)
    ) %>%
    left_join(x$table_header, by = "column") %>%
    mutate(label = coalesce(.data$label, .data$column)) %>%
    pull(.data$label)

  # taking each gt function call, concatenating them with %>% separating them
  x$kable_calls[names(x$kable_calls) %>% setdiff(omit)] %>%
    # removing NULL elements
    compact() %>%
    glue_collapse(sep = " %>% ") %>%
    # converting strings into expressions to run
    parse(text = .) %>%
    eval() %>%
    # performing final modifcations prior to returning kable object
    select(-no_print_vars) %>%
    mutate_all(~ifelse(is.na(.), "", .)) %>%
    knitr::kable(col.names = col_labels)
}
