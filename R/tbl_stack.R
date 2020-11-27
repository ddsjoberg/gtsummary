#' Stacks two or more gtsummary objects
#'
#' Assists in patching together more complex tables. `tbl_stack()` appends two
#' or more `tbl_regression`, `tbl_summary`, `tbl_svysummary`, or `tbl_merge` objects.
#' Column attributes, including number formatting and column footnotes, are
#' retained from the first passed gtsummary object.
#'
#' @param tbls List of gtsummary objects
#' @param group_header Character vector with table headers where length matches
#' the length of `tbls=`
#' @inheritParams add_global_p.tbl_regression
#' @family tbl_summary tools
#' @family tbl_svysummary tools
#' @family tbl_regression tools
#' @family tbl_uvregression tools
#' @family tbl_survfit tools
#' @seealso [tbl_merge]
#' @author Daniel D. Sjoberg
#' @export
#' @return A `tbl_stack` object
#' @examples
#' # Example 1 ----------------------------------
#' # stacking two tbl_regression objects
#' t1 <-
#'   glm(response ~ trt, trial, family = binomial) %>%
#'   tbl_regression(
#'     exponentiate = TRUE,
#'     label = list(trt ~ "Treatment (unadjusted)")
#'   )
#'
#' t2 <-
#'   glm(response ~ trt + grade + stage + marker, trial, family = binomial) %>%
#'   tbl_regression(
#'     include = "trt",
#'     exponentiate = TRUE,
#'     label = list(trt ~ "Treatment (adjusted)")
#'   )
#'
#' tbl_stack_ex1 <- tbl_stack(list(t1, t2))
#'
#' # Example 2 ----------------------------------
#' # stacking two tbl_merge objects
#' library(survival)
#' t3 <-
#'   coxph(Surv(ttdeath, death) ~ trt, trial) %>%
#'   tbl_regression(
#'     exponentiate = TRUE,
#'     label = list(trt ~ "Treatment (unadjusted)")
#'   )
#'
#' t4 <-
#'   coxph(Surv(ttdeath, death) ~ trt + grade + stage + marker, trial) %>%
#'   tbl_regression(
#'     include = "trt",
#'     exponentiate = TRUE,
#'     label = list(trt ~ "Treatment (adjusted)")
#'   )
#'
#'
#' # first merging, then stacking
#' row1 <- tbl_merge(list(t1, t3), tab_spanner = c("Tumor Response", "Death"))
#' row2 <- tbl_merge(list(t2, t4))
#' tbl_stack_ex2 <-
#'   tbl_stack(list(row1, row2), group_header = c("Unadjusted Analysis", "Adjusted Analysis"))
#' @section Example Output:
#' \if{html}{Example 1}
#'
#' \if{html}{\figure{tbl_stack_ex1.png}{options: width=50\%}}
#'
#' \if{html}{Example 2}
#'
#' \if{html}{\figure{tbl_stack_ex2.png}{options: width=80\%}}

tbl_stack <- function(tbls, group_header = NULL, quiet = NULL) {
  # setting defaults -----------------------------------------------------------
  quiet <- quiet %||% get_theme_element("pkgwide-lgl:quiet") %||% FALSE

  # input checks ---------------------------------------------------------------
  # class of tbls
  if (!inherits(tbls, "list")) {
    stop("Expecting 'tbls' to be a list, e.g. 'tbls = list(tbl1, tbl2)'")
  }

  # checking all inputs are class gtsummary
  if (!purrr::every(tbls, ~inherits(.x, "gtsummary"))) {
    stop("All objects in 'tbls' must be class 'gtsummary'", call. = FALSE)
  }

  # if group_header specified, then it must be a vector of same length tbls ----
  if (!is.null(group_header) && length(tbls) != length(group_header)) {
    stop("The length of `tbls=` and `group_header=` must match.", call. = FALSE)
  }

  # will return call, and all arguments passed to tbl_stack
  func_inputs <- as.list(environment())

  # stacking tables ------------------------------------------------------------
  # the table_body and call_list will be updated with the tbl_stack values
  results <- list()
  if (is.null(group_header)) {
    results$table_body <-
      map_dfr(tbls, ~pluck(.x, "table_body"))
  }
  else if (!is.null(group_header)) {
    # adding grouping column
    results$table_body <-
      purrr::map2_dfr(
        tbls, seq_along(tbls),
        ~pluck(.x, "table_body") %>% mutate(groupname_col = group_header[.y])
      ) %>%
      select(.data$groupname_col, everything()) %>%
      group_by(.data$groupname_col)
  }

  # creating table header ------------------------------------------------------
  # print message if column headers, footnotes, etc. are different among tbls
  if (identical(quiet, FALSE)) print_stack_differences(tbls)

  results$table_header <-
    map_dfr(tbls, ~pluck(.x, "table_header")) %>%
    group_by(.data$column) %>%
    filter(dplyr::row_number() == 1) %>%
    ungroup() %>%
    table_header_fill_missing(table_body = results$table_body)

  # adding label for grouping variable, if present -----------------------------
  if ("groupname_col" %in% names(results$table_body)) {
    results <- modify_header_internal(
      results,
      groupname_col = get_theme_element("tbl_stack-str:group_header", default = "**Group**")
    )
    # making groups column left aligned (if printed with non-gt printer)
    results$table_header <-
      results$table_header %>%
      mutate(align = ifelse(.data$column == "groupname_col", "left", .data$align))

  }

  # returning results ----------------------------------------------------------
  results$call_list <- list(tbl_stack = match.call())
  results$tbls <- tbls

  class(results) <- c("tbl_stack", "gtsummary")
  results
}

# function prints changes to column labels, footnotes, and spanning headers
print_stack_differences <- function(tbls) {
  tbl_differences <-
    purrr::map2_dfr(
      tbls, seq_len(length(tbls)),
      ~pluck(.x, "table_header") %>%
        mutate(..tbl_id.. = .y)
    ) %>%
    select(.data$..tbl_id.., .data$column, .data$label, .data$footnote,
           .data$footnote_abbrev, .data$spanning_header) %>%
    tidyr::pivot_longer(cols = c(.data$label, .data$footnote, .data$footnote_abbrev,
                                 .data$spanning_header)) %>%
    group_by(.data$column, .data$name) %>%
    mutate(
      new_value = .data$value[1],
      name_fmt = case_when(name == "label" ~ "Column header",
                           name == "footnote" ~ "Column footnote",
                           name == "footnote_abbrev" ~ "Column abbreviation footnote",
                           name == "spanning_header" ~ "Spanning column header")
    ) %>%
    filter(.data$new_value != .data$value) %>%
    ungroup() %>%
    arrange(.data$name != "label", .data$name_fmt, .data$..tbl_id..)

  if (nrow(tbl_differences) > 0) {
    paste("When tables are stacked,",
          "attributes from the first table are used.",
          "The following attributes were changed.",
          "Use {ui_code('quiet = TRUE')} to supress this message.") %>%
      stringr::str_wrap() %>%
      usethis::ui_info()

    # purrr::pwalk(
    #   list(tbl_differences$name_fmt, tbl_differences$..tbl_id..,
    #        tbl_differences$column, tbl_differences$value, tbl_differences$new_value),
    #   function(name_fmt, ..tbl_id.., column, value, new_value)
    #     ui_done("{name_fmt}, table {..tbl_id..} ({column}): {ui_field(value)} ---> {ui_field(new_value)}")
    # )
  }

  return(invisible())
}
