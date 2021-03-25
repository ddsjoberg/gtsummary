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
#' @inheritParams add_global_p
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
  results$table_body <-
    purrr::map2_dfr(
      tbls, seq_along(tbls),
      function(tbl, id) {
        # adding a table ID and group header
        table_body <- pluck(tbl, "table_body") %>% mutate(tbl_id = id)
        if (!is.null(group_header))
          table_body <-
            table_body %>%
            mutate(groupname_col = group_header[id])

        table_body %>% select(any_of(c("groupname_col", "tbl_id")), everything())
      }
    )

  # creating table styling -----------------------------------------------------
  # print message if column headers, footnotes, etc. are different among tbls
  if (identical(quiet, FALSE)) print_stack_differences(tbls)

  results$table_styling$header <-
    map_dfr(tbls, ~pluck(.x, "table_styling", "header")) %>%
    group_by(.data$column) %>%
    filter(dplyr::row_number() == 1) %>%
    ungroup()

  # cycle over each of the styling tibbles and stack them in reverse order -----
  for (style_type in c("footnote", "footnote_abbrev", "fmt_fun", "text_format", "fmt_missing", "cols_merge")) {
    results$table_styling[[style_type]] <-
      map_dfr(
        rev(seq_along(tbls)),
        function(i) {
          df <- tbls[[i]]$table_styling[[style_type]]
          if ("rows" %in% names(df) && nrow(df) > 0) {
            # adding tbl_id to the rows specifications,
            # e.g. data$tbl_id == 1L & .data$row_type != "label"
            df$rows <-
              map(df$rows, ~add_tbl_id_to_quo(.x, tbls[[i]]$table_body, i))
          }
          df %>%
            mutate_at(vars(any_of(c("column", "text_interpret",
                                    "footnote", "format_type", "symbol"))), as.character)
        }
      )
  }

  # adding label for grouping variable, if present -----------------------------
  class(results) <- c("tbl_stack", "gtsummary")
  results <-
    modify_table_styling(
      results,
      any_of("groupname_col"),
      label = get_theme_element("tbl_stack-str:group_header", default = "**Group**"),
      align = "left",
      hide = FALSE
    )

  # returning results ----------------------------------------------------------
  results$call_list <- list(tbl_stack = match.call())
  results$tbls <- tbls

  results
}

# function prints changes to column labels and spanning headers
print_stack_differences <- function(tbls) {
  tbl_differences <-
    purrr::map2_dfr(
      tbls, seq_len(length(tbls)),
      ~pluck(.x, "table_styling", "header") %>%
        mutate(..tbl_id.. = .y)
    ) %>%
    select(.data$..tbl_id.., .data$column, .data$label, .data$spanning_header) %>%
    tidyr::pivot_longer(cols = c(.data$label, .data$spanning_header)) %>%
    group_by(.data$column, .data$name) %>%
    mutate(
      new_value = .data$value[1],
      name_fmt = case_when(name == "label" ~ "Column header",
                           name == "spanning_header" ~ "Spanning column header")
    ) %>%
    filter(.data$new_value != .data$value) %>%
    ungroup() %>%
    arrange(.data$name != "label", .data$name_fmt, .data$..tbl_id..)

  if (nrow(tbl_differences) > 0) {
    paste("Column headers among stacked tables differ. Headers from the first table are used.",
          "Use {.code quiet = TRUE} to supress this message.") %>%
      stringr::str_wrap() %>%
      cli_alert_info()

    # purrr::pwalk(
    #   list(tbl_differences$name_fmt, tbl_differences$..tbl_id..,
    #        tbl_differences$column, tbl_differences$value, tbl_differences$new_value),
    #   function(name_fmt, ..tbl_id.., column, value, new_value)
    #     cli_alert_success("{name_fmt}, table {..tbl_id..} ({column}): {.field {value}} ---> {.field {new_value}}")
    # )
  }

  return(invisible())
}

add_tbl_id_to_quo <- function(x, table_body, tbl_id) {
  if (eval_tidy(x, data = table_body) %>% is.null()) return(x)

  # if quosure, add tbl_id
  if (inherits(x, "quosure"))
    return(
      rlang::quo(.data$tbl_id == !!tbl_id & (!!rlang::f_rhs(x))) %>%
        structure(.Environment = attr(x, ".Environment"))
    )

  # if expression, add tbl_id
  expr(.data$tbl_id == !!tbl_id & (!!x))
}
