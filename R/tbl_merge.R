#' Merge two or more gtsummary objects
#'
#' Merges two or more `tbl_regression`, `tbl_uvregression`, `tbl_stack`,
#' or `tbl_summary` objects and adds appropriate spanning headers.
#'
#' @param tbls List of gtsummary objects to merge
#' @param tab_spanner Character vector specifying the spanning headers.
#' Must be the same length as `tbls`. The
#' strings are interpreted with `gt::md`.
#' Must be same length as `tbls` argument
#' @family tbl_regression tools
#' @family tbl_uvregression tools
#' @family tbl_summary tools
#' @seealso [tbl_stack]
#' @author Daniel D. Sjoberg
#' @export
#' @return A `tbl_merge` object
#' @examples
#' # Side-by-side Regression Models
#' library(survival)
#' t1 <-
#'   glm(response ~ trt + grade + age, trial, family = binomial) %>%
#'   tbl_regression(exponentiate = TRUE)
#' t2 <-
#'   coxph(Surv(ttdeath, death) ~ trt + grade + age, trial) %>%
#'   tbl_regression(exponentiate = TRUE)
#' tbl_merge_ex1 <-
#'   tbl_merge(
#'     tbls = list(t1, t2),
#'     tab_spanner = c("**Tumor Response**", "**Time to Death**")
#'   )
#'
#' # Descriptive statistics alongside univariate regression, with no spanning header
#' t3 <-
#'   trial[c("age", "grade", "response")] %>%
#'   tbl_summary(missing = "no") %>%
#'   add_n()
#' t4 <-
#'   tbl_uvregression(
#'     trial[c("ttdeath", "death", "age", "grade", "response")],
#'     method = coxph,
#'     y = Surv(ttdeath, death),
#'     exponentiate = TRUE,
#'     hide_n = TRUE
#'   )
#'
#' tbl_merge_ex2 <-
#'   tbl_merge(tbls = list(t3, t4)) %>%
#'   as_gt(include = -tab_spanner) %>%
#'   gt::cols_label(stat_0_1 = gt::md("**Summary Statistics**"))
#'
#' @section Example Output:
#' \if{html}{Example 1}
#'
#' \if{html}{\figure{tbl_merge_ex1.png}{options: width=70\%}}
#'
#' \if{html}{Example 2}
#'
#' \if{html}{\figure{tbl_merge_ex2.png}{options: width=65\%}}
#'
tbl_merge <- function(tbls, tab_spanner = NULL) {
  # input checks ---------------------------------------------------------------
  # if tab spanner is null, default is Table 1, Table 2, etc....
  if (is.null(tab_spanner)) {
    tab_spanner <- paste0(c("**Table "), seq_len(length(tbls)), "**")
  }

  # class of tbls
  if (!inherits(tbls, "list")) {
    stop("Expecting 'tbls' to be a list, e.g. 'tbls = list(tbl1, tbl2)'")
  }

  # checking all inputs are class gtsummary
  if (!purrr::every(tbls, ~inherits(.x, "gtsummary"))) {
    stop("All objects in 'tbls' must be class 'gtsummary'", call. = FALSE)
  }

  # at least two objects must be passed
  tbls_length <- length(tbls)
  if (tbls_length < 2) stop("Supply 2 or more gtsummary regression objects to 'tbls ='")

  # length of spanning header matches number of models passed
  if (tbls_length != length(tab_spanner)) {
    stop("'tbls' and 'tab_spanner' must be the same length")
  }

  # merging tables -------------------------------------------------------------
  # nesting data by variable (one line per variable), and renaming columns with number suffix
  nested_table <- tbls %>%
    imap(function(x, y) {
      # creating a column that is the variable label
      group_by(x$table_body, .data$variable) %>%
        mutate(
          var_label = ifelse(.data$row_type == "label", .data$label, NA)
        ) %>%
        tidyr::fill(.data$var_label, .direction = "downup") %>%
        ungroup() %>%
        rename_at(
          vars(-c("variable", "row_type", "var_label", "label")),
          ~ glue("{.}_{y}")
        )
    })

  # nesting results within variable
  nested_table <- map(
    nested_table,
    ~ nest(.x, data = -one_of(c("variable", "var_label")))
  )

  # merging formatted objects together
  merged_table <-
    nested_table[[1]] %>%
    rename(table = .data$data)

  # cycling through all tbls, merging results into a column tibble
  for (i in 2:tbls_length) {
    merged_table <-
      merged_table %>%
      full_join(
        nested_table[[i]],
        by = c("variable", "var_label")
      ) %>%
      mutate(
        table = map2(
          .data$table, .data$data,
          function(table, data) {
            if (is.null(table)) {
              return(data)
            }
            if (is.null(data)) {
              return(table)
            }
            full_join(table, data, by = c("row_type", "label"))
          }
        )
      ) %>%
      select(-c("data", "table"), "table")
  }

  # unnesting results from within variable column tibbles
  table_body <-
    merged_table %>%
    select(-.data$var_label) %>%
    unnest("table") %>%
    select(.data$label, everything())

  # stacking all table_header dfs together and renaming ------------------------
  table_header <-
    imap_dfr(
      tbls,
      ~ pluck(.x, "table_header") %>%
        # tidying the code in these columns (giving it space to breathe),
        # that is can be properly pasred in the next step
        mutate_at(
          vars(.data$missing_emdash, .data$indent, .data$bold, .data$italic),
          ~map_chr(., function(t) ifelse(is.na(t), t, rlang::parse_expr(t) %>% rlang::expr_deparse()))
        ) %>%
        # updating code with new variable names
        mutate_at(
          vars(.data$missing_emdash, .data$indent, .data$bold, .data$italic),
          function(x) tbl_merge_update_chr_code(code = x, names = names(.x$table_body), n = .y)
        ) %>%
        # updading column names to include the index
        mutate(
          column = ifelse(
            .data$column %in% c("label", "variable", "row_type"),
            .data$column,
            paste(.data$column, .y, sep = "_")
          ),
          spanning_header = ifelse(
            !.data$column %in% c("label", "variable", "row_type"),
            tab_spanner[.y],
            .data$spanning_header
          )
        )
    ) %>%
    group_by(.data$column) %>%
    filter(dplyr::row_number() == 1) %>%
    ungroup()

  table_header <-
    tibble(column = names(table_body)) %>%
    left_join(table_header, by = "column") %>%
    table_header_fill_missing()

  # returning results
  results <- list(
    table_body = table_body,
    table_header = table_header,
    tbls = tbls,
    call_list = list(tbl_merge = match.call())
  )

  class(results) <- c("tbl_merge", "gtsummary")
  results
}

# this function names a chr code string, variable names, and the merge number,
# and returns teh updated code string with updated variable names
# > tbl_merge_update_chr_code("longvarname == 'label' & varname == TRUE",
#                             +                           c("varname", "longvarname"), 2)
# [1] "longvarname_2 == 'label' & varname_2 == TRUE"
tbl_merge_update_chr_code <- function(code, names, n) {
  new_names <- ifelse(
    names %in% c("label", "variable", "row_type"),
    names, paste(names, n, sep = "_")
  )

  code %>%
    map_chr(
      function(code) {
        if (is.na(code)) return(code)
        stringr::str_split_fixed(code, pattern = " ", n = Inf) %>%
        as.vector() %>%
        purrr::map_chr(function(x) {
          lgl_match <- names %in% x
          if (!any(lgl_match)) return(x)
          new_names[lgl_match]
        }) %>%
        paste(collapse = " ")
      }
    )
}
