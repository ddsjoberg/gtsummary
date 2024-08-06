#' Merge tables
#'
#' Merge gtsummary tables, e.g. `tbl_regression`, `tbl_uvregression`, `tbl_stack`,
#' `tbl_summary`, `tbl_svysummary`, etc.
#'
#' @param tbls (`list`)\cr
#'   List of gtsummary objects to merge
#' @param tab_spanner (`character`)\cr
#'   Character vector specifying the spanning headers.
#'   Must be the same length as `tbls`. The
#'   strings are interpreted with `gt::md`.
#'   Must be same length as `tbls` argument. Default is `NULL`, and places
#'   a default spanning header. If `FALSE`, no header will be placed.
#'
#' @author Daniel D. Sjoberg
#' @export
#' @return A `'tbl_merge'` object
#'
#' @examplesIf (identical(Sys.getenv("NOT_CRAN"), "true") || identical(Sys.getenv("IN_PKGDOWN"), "true")) && gtsummary:::is_pkg_installed('survival', reference_pkg = 'gtsummary')
#' # Example 1 ----------------------------------
#' # Side-by-side Regression Models
#' library(survival)
#'
#' t1 <-
#'   glm(response ~ trt + grade + age, trial, family = binomial) %>%
#'   tbl_regression(exponentiate = TRUE)
#' t2 <-
#'   coxph(Surv(ttdeath, death) ~ trt + grade + age, trial) %>%
#'   tbl_regression(exponentiate = TRUE)
#'
#' tbl_merge(
#'   tbls = list(t1, t2),
#'   tab_spanner = c("**Tumor Response**", "**Time to Death**")
#' )
#'
#' # Example 2 ----------------------------------
#' # Descriptive statistics alongside univariate regression, with no spanning header
#' t3 <-
#'   trial[c("age", "grade", "response")] %>%
#'   tbl_summary(missing = "no") %>%
#'   add_n() %>%
#'   modify_header(stat_0 ~ "**Summary Statistics**")
#' t4 <-
#'   tbl_uvregression(
#'     trial[c("ttdeath", "death", "age", "grade", "response")],
#'     method = coxph,
#'     y = Surv(ttdeath, death),
#'     exponentiate = TRUE,
#'     hide_n = TRUE
#'   )
#'
#' tbl_merge(tbls = list(t3, t4)) %>%
#'   modify_spanning_header(everything() ~ NA_character_)
tbl_merge <- function(tbls, tab_spanner = NULL) {
  set_cli_abort_call()

  # input checks ---------------------------------------------------------------
  # class of tbls
  if (!inherits(tbls, "list")) {
    cli::cli_abort(
      "Expecting argument {.arg tbls} to be class {.cls list}, e.g. {.code tbl_merge(tbls = list(tbl1, tbl2))}.",
      call = get_cli_abort_call()
    )
  }
  cards::check_list_elements(
    x = tbls,
    predicate = \(x) inherits(x, "gtsummary"),
    error_msg = "All objects in {.arg tbls} list must be class {.cls gtsummary}."
  )

  # check all tbls have the merging columns
  if (some(tbls, ~ any(!c("variable", "row_type", "var_label", "label") %in% names(.x$table_body)))) {
    cli::cli_abort(
      "All objects in the {.arg tbls} list must have columns
       {.val {c('variable', 'row_type', 'var_label', 'label')}}
       in {.code .$table_body} for merging",
      call = get_cli_abort_call()
    )
  }

  if (!is_empty(tab_spanner) && !isFALSE(tab_spanner) && !is.character(tab_spanner)) {
    cli::cli_abort(
      "The {.arg tab_spanner} argument must be {.val {NULL}}, {.val {FALSE}}, or class {.cls character}.",
      call = get_cli_abort_call()
    )
  }

  tbls_length <- length(tbls)

  # adding tab spanners if requested
  if (!isFALSE(tab_spanner)) {
    # if tab spanner is null, default is Table 1, Table 2, etc....
    if (is.null(tab_spanner)) {
      tab_spanner <- paste0(c("**Table "), seq_len(tbls_length), "**")
    }

    # length of spanning header matches number of models passed
    if (tbls_length != length(tab_spanner)) {
      cli::cli_abort(
        "The lengths of arguments {.arg tbls} and {.arg tab_spanner} must be the same.",
        call = get_cli_abort_call()
      )
    }

    # adding tab_spanners
    tbls <-
      map2(
        tbls, seq_along(tbls),
        ~ modify_spanning_header(
          .x, c(
            everything(),
            # TODO: Use of the "ci" column was deprecated in v2.0 and it can be removed from here in the future
            -any_of(c("variable", "row_type", "var_label", "label", "ci"))
          ) ~ tab_spanner[.y]
        )
      )
  }


  # merging tables -------------------------------------------------------------
  # nesting data by variable (one line per variable), and renaming columns with number suffix
  nested_table <- map2(
    tbls, seq_along(tbls),
    function(x, y) {
      # creating a column that is the variable label
      dplyr::group_by(x$table_body, .data$variable) %>%
        dplyr::mutate(
          var_label = ifelse(.data$row_type == "label", .data$label, NA)
        ) %>%
        tidyr::fill("var_label", .direction = "downup") %>%
        dplyr::ungroup() %>%
        dplyr::rename_at(
          vars(-c("variable", "row_type", "var_label", "label")),
          ~ glue("{.}_{y}")
        )
    }
  )

  # checking that merging rows are unique --------------------------------------
  nested_table %>%
    some(
      ~ nrow(.x) !=
        dplyr::select(.x, all_of(c("variable", "row_type", "var_label", "label"))) %>%
        dplyr::distinct() %>%
        nrow()
    ) %>%
    switch(
      cli::cli_inform(
        "The merging columns (variable name, variable label, row type, and label column)
         are not unique and the merge may fail or result in a malformed table.
         If you previously called {.fun tbl_stack} on your tables,
         then merging with {.fun tbl_merge} before calling {.arg tbl_stack} may resolve the issue."
      )
    )

  # nesting results within variable
  nested_table <- map(
    nested_table,
    ~ tidyr::nest(.x, data = -any_of(c("variable", "var_label")))
  )

  # merging formatted objects together
  merged_table <-
    nested_table[[1]] %>%
    dplyr::rename(table = "data")

  if (tbls_length > 1) {
    # cycling through all tbls, merging results into a column tibble
    for (i in 2:tbls_length) {
      merged_table <-
        merged_table %>%
        dplyr::full_join(
          nested_table[[i]],
          by = c("variable", "var_label")
        ) %>%
        dplyr::mutate(
          table = map2(
            .data$table, .data$data,
            function(table, data) {
              if (is.null(table)) {
                return(data)
              }
              if (is.null(data)) {
                return(table)
              }
              dplyr::full_join(table, data, by = c("row_type", "label"))
            }
          )
        ) %>%
        select(-c("data", "table"), "table")
    }
  }

  # unnesting results from within variable column tibbles
  ends_with_selectors <-
    map(seq_len(tbls_length), ~ rlang::expr(ends_with(!!paste0("_", .x))))
  table_body <-
    merged_table %>%
    tidyr::unnest("table") %>%
    dplyr::select(
      "variable", "var_label", "row_type", "label",
      !!!ends_with_selectors, everything()
    )

  # renaming columns in stylings and updating ----------------------------------
  x <- .create_gtsummary_object(table_body = table_body,
                                tbls = tbls,
                                call_list = list(tbl_merge = match.call()))

  x <- .tbl_merge_update_table_styling(x, tbls)

  # returning results
  class(x) <- c("tbl_merge", "gtsummary")
  x
}

.tbl_merge_update_table_styling <- function(x, tbls) {
  # update table_styling$header
  x$table_styling$header <-
    map2(
      tbls, seq_along(tbls),
      ~ .x$table_styling$header %>%
        dplyr::filter(!(.data$column %in% c("label", "variable", "var_label", "row_type") & .y != 1)) %>%
        dplyr::mutate(
          column = ifelse(
            .data$column %in% c("label", "variable", "var_label", "row_type") & .y == 1,
            .data$column,
            paste0(.data$column, "_", .y)
          )
        )
    ) %>%
    reduce(.rows_update_table_styling_header, .init = x$table_styling$header)

  for (style_type in c("footnote", "footnote_abbrev", "fmt_fun", "indent", "text_format", "fmt_missing", "cols_merge")) {
    x$table_styling[[style_type]] <-
      map(
        rev(seq_along(tbls)),
        function(i) {
          style_updated <- tbls[[i]]$table_styling[[style_type]]

          # return if there are no rows
          if (!is.data.frame(style_updated) || nrow(style_updated) == 0) {
            return(style_updated)
          }

          # renaming column variable
          style_updated$column <-
            ifelse(
              style_updated$column %in% c("label", "variable", "var_label", "row_type"),
              style_updated$column,
              paste0(style_updated$column, "_", i)
            ) %>%
            as.character()

          # updating column names in rows expr/quo
          if ("rows" %in% names(style_updated)) {
            style_updated$rows <-
              map(
                style_updated$rows,
                ~ .rename_variables_in_expression(.x, i, tbls[[i]])
              )
          }

          # updating column names in pattern string
          if ("pattern" %in% names(style_updated)) {
            style_updated$pattern <-
              map_chr(
                style_updated$pattern,
                ~ .rename_variables_in_pattern(.x, i, tbls[[i]])
              )
          }

          style_updated
        }
      ) |>
      dplyr::bind_rows()
  }

  # take the first non-NULL element from tbls[[.]]
  for (style_type in c("caption", "source_note")) {
    x$table_styling[[style_type]] <-
      map(seq_along(tbls), ~ getElement(tbls, .x) |> getElement("table_styling") |> getElement(style_type)) %>%
      reduce(.f = \(.x, .y) .x %||% .y)
  }

  # # rename variables in expressions, and take first non-NULL element
  for (style_type in "horizontal_line_above") {
    x$table_styling[[style_type]] <-
      map(
        seq_along(tbls),
        ~ .rename_variables_in_expression(
          rows = getElement(tbls, .x) |> getElement("table_styling") |> getElement(style_type),
          id = .x,
          tbl = tbls[[.x]]
        )
      ) %>%
      reduce(.f = \(.x, .y) .x %||% .y)
  }

  x
}

.rename_variables_in_expression <- function(rows, id, tbl) {
  # if NULL, return rows expression unmodified
  rows_evaluated <- eval_tidy(rows, data = tbl$table_body)
  if (is.null(rows_evaluated)) {
    return(rows)
  }

  # convert rows to proper expression
  expr <- switch(inherits(rows, "quosure"), f_rhs(rows)) %||% rows

  # get all variable names in expression to be renamed
  columns <- tbl$table_styling$header$column
  var_list <-
    expr(~ !!expr) %>%
    eval() %>%
    all.vars() %>%
    setdiff(c("label", "variable", "var_label", "row_type")) %>%
    intersect(columns)

  # if no variables to rename, return rows unaltered
  if (identical(var_list, character())) {
    return(rows)
  }

  # creating arguments list for `substitute()`
  substitute_args <- paste0(var_list, "_", id) %>%
    map(~ expr(as.name(!!.x))) %>%
    set_names(var_list)

  # renaming columns in expression
  expr_renamed <- expr(do.call("substitute", list(expr, list(!!!substitute_args)))) %>% eval()

  # if original rows was a quosure, convert it back to one
  if (inherits(rows, "quosure")) {
    expr_renamed <-
      quo(!!expr_renamed) %>% structure(.Environment = attr(rows, ".Environment"))
  }

  expr_renamed
}

.rename_variables_in_pattern <- function(pattern, id, tbl) {
  # get all variable names in expression to be renamed
  columns <- tbl$table_styling$header$column
  var_list <-
    str_extract_all(pattern, "\\{.*?\\}") %>%
    map(~ str_remove_all(.x, pattern = "}", fixed = TRUE)) %>%
    map(~ str_remove_all(.x, pattern = "{", fixed = TRUE)) %>%
    unlist() %>%
    setdiff(c("label", "variable", "var_label", "row_type")) %>%
    intersect(columns)

  # if no variables to rename, return rows unaltered
  if (identical(var_list, character())) {
    return(pattern)
  }

  # replace variables with new names in pattern string.
  for (v in var_list) {
    pattern <-
      str_replace_all(
        string = pattern,
        pattern = paste0("{", v, "}"),
        replacement = paste0("{", v, "_", id, "}"),
        fixed = TRUE
      )
  }

  pattern
}

