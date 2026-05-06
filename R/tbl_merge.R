#' Merge tables
#'
#' @description
#' Merge gtsummary tables, e.g. `tbl_regression`, `tbl_uvregression`, `tbl_stack`,
#' `tbl_summary`, `tbl_svysummary`, etc.
#'
#' This function merges **like tables**.
#' Generally, this means each of the tables being merged
#' should have the same structure.
#' When merging tables with different structures, rows may appear
#' out of order.
#' The ordering of rows can be updated with `modify_table_body(~dplyr::arrange(.x, ...))`.
#'
#' @param tbls (`list`)\cr
#'   List of gtsummary objects to merge
#' @param tab_spanner (`character`)\cr
#'   Character vector specifying the spanning headers.
#'   Must be the same length as `tbls`. The
#'   strings are interpreted with `gt::md`.
#'   Must be same length as `tbls` argument. Default is `NULL`, and places
#'   a default spanning header. If `FALSE`, no header will be placed.
#' @param merge_vars (`character`)\cr
#'   Column names that are used as the merge IDs.
#'   The default is `NULL`, which merges on
#'   `c(any_of(c("variable", "row_type", "var_label", "label"), cards::all_ard_groups())`.
#'   Any column name included here that does not appear in all tables, will
#'   be removed.
#' @param tbl_ids (`character`)\cr
#'   Optional character vector of IDs that will be assigned to the input tables.
#'   The ID is assigned by assigning a name to the `tbls` list, which is
#'   returned in `x$tbls`.
#' @param quiet (scalar `logical`)\cr
#'   When `FALSE`, a message is printed when unlike tables are merged warning
#'   users of potential row ordering issues.
#'
#' @author Daniel D. Sjoberg
#' @export
#' @return A `'tbl_merge'` object
#'
#' @examplesIf (identical(Sys.getenv("NOT_CRAN"), "true") || identical(Sys.getenv("IN_PKGDOWN"), "true")) && gtsummary:::is_pkg_installed('survival')
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
tbl_merge <- function(tbls, tab_spanner = NULL, merge_vars = NULL, tbl_ids = NULL,
                      quiet = FALSE) {
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
  check_class(merge_vars, cls = "character", allow_empty = TRUE)
  check_class(tbl_ids, cls = "character", allow_empty = TRUE)
  if (!is_empty(tbl_ids)) {
    check_identical_length(tbls, tbl_ids)
  }

  if (!is_empty(tab_spanner) && !isFALSE(tab_spanner) && !is.character(tab_spanner)) {
    cli::cli_abort(
      "The {.arg tab_spanner} argument must be {.code NULL}, {.val {FALSE}}, or class {.cls character}.",
      call = get_cli_abort_call()
    )
  }

  # setting the merging columns ------------------------------------------------
  if (is_empty(merge_vars)) {
    merge_vars <-
      dplyr::select(tbls[[1]]$table_body,
                    any_of(c("variable", "row_type", "var_label", "label")),
                    cards::all_ard_groups()) |>
      names()
  }
  # merge columns will be those that appear in all tbls
  merge_vars <-
    map(tbls, ~names(.x$table_body)) |>
    reduce(.f = intersect, .init = merge_vars)

  if (is_empty(merge_vars)) {
    cli::cli_abort(
      "The tables in the {.arg tbls} argument do not share any columns specified in {.arg merge_vars} argument and merge cannot be performed.",
      call = get_cli_abort_call()
    )
  }

  tbls_length <- length(tbls)

  # check whether tables are mis-matched in any way ----------------------------
  if (isFALSE(quiet)) .check_merge_likeness(tbls, merge_vars)

  # adding tab spanners if requested -------------------------------------------
  if (!isFALSE(tab_spanner)) {
    # if tab spanner is null, default is Table 1, Table 2, etc....
    if (is_empty(tab_spanner)) {
      tab_spanner <- paste0(c("**Table "), seq_len(tbls_length), "**")
    }

    # length of spanning header matches number of models passed
    if (tbls_length != length(tab_spanner)) {
      cli::cli_abort(
        "The lengths of arguments {.arg tbls} and {.arg tab_spanner} must be the same.",
        call = get_cli_abort_call()
      )
    }

    # build spanning_header rows directly instead of calling
    # modify_spanning_header() per table (avoids tidyselect eval overhead)
    for (i in seq_along(tbls)) {
      cols <- setdiff(names(tbls[[i]]$table_body), merge_vars)
      nc <- length(cols)
      tbls[[i]]$table_styling$spanning_header <- vctrs::vec_rbind(
        tbls[[i]]$table_styling$spanning_header,
        vctrs::new_data_frame(list(
          level = rep(1L, nc),
          column = cols,
          spanning_header = rep(unname(tab_spanner[i]), nc),
          text_interpret = rep("gt::md", nc),
          remove = rep(FALSE, nc)
        ))
      )
    }
  }


  # merging tables -------------------------------------------------------------
  # first renaming columns with index suffix
  lst_table_body <-
    map(
      seq_along(tbls),
      \(i) {
        tbls[[i]]$table_body |>
          dplyr::rename_with(
            .fn = ~paste(., i, sep = "_"),
            .cols = -all_of(merge_vars)
          )
      }
    )

  # now merge all the table bodies together
  table_body <-
    lst_table_body |>
    reduce(.f = dplyr::full_join, by = merge_vars) |>
    suppressWarnings() |> # suppress many to many merge warning
    dplyr::relocate(all_of(merge_vars), .before = 1L)

  # renaming columns in stylings and updating ----------------------------------
  x <- .create_gtsummary_object(table_body = table_body,
                                tbls = tbls,
                                call_list = list(tbl_merge = match.call()))

  x <- .tbl_merge_update_table_styling(x = x, tbls = tbls, merge_vars = merge_vars)

  # add tbl_ids, if specified --------------------------------------------------
  if (!is_empty(tbl_ids)) {
    names(x$tbls) <- tbl_ids
  }

  # returning results ----------------------------------------------------------
  class(x) <- c("tbl_merge", "gtsummary")
  x
}

.tbl_merge_update_table_styling <- function(x, tbls, merge_vars) {
  # update table_styling$header using base R match/update instead of
  # reduce(.rows_update_table_styling_header) which runs
  # as.data.frame → match → update → as_tibble + left_join per iteration
  result <- as.data.frame(x$table_styling$header)
  for (i in seq_along(tbls)) {
    h <- as.data.frame(tbls[[i]]$table_styling$header)
    # for table i>1, drop merge_var rows; for table 1, keep them
    if (i > 1L) h <- h[!h$column %in% merge_vars, , drop = FALSE]
    # rename non-merge columns: append _i
    non_merge <- !h$column %in% merge_vars
    h$column[non_merge] <- paste0(h$column[non_merge], "_", i)

    common_cols <- intersect(names(result), names(h))
    idx <- match(h$column, result$column)
    existing <- !is.na(idx)

    # update existing rows
    if (any(existing)) {
      for (col in setdiff(common_cols, "column")) {
        result[idx[existing], col] <- h[existing, col]
      }
    }

    # add new rows
    if (any(!existing)) {
      new_rows <- h[!existing, , drop = FALSE]
      for (col in setdiff(names(result), names(new_rows))) new_rows[[col]] <- NA
      for (col in setdiff(names(new_rows), names(result))) result[[col]] <- NA
      result <- rbind(result, new_rows[, names(result), drop = FALSE])
    }

    # add new columns from h
    new_cols <- setdiff(names(h), names(result))
    if (length(new_cols) > 0L) {
      for (col in new_cols) {
        result[[col]] <- NA
        idx2 <- match(h$column, result$column)
        result[idx2[!is.na(idx2)], col] <- h[!is.na(idx2), col]
      }
    }
  }
  x$table_styling$header <- tibble::as_tibble(result)

  for (style_type in c("spanning_header", "footnote_header", "footnote_body",
                       "footnote_spanning_header", "abbreviation", "source_note",
                       "fmt_fun", "post_fmt_fun", "indent", "text_format",
                       "fmt_missing", "cols_merge")) {
    parts <- vector("list", length(tbls))
    for (i in rev(seq_along(tbls))) {
      style_updated <- tbls[[i]]$table_styling[[style_type]]

      # skip if there are no rows
      if (!is.data.frame(style_updated) || nrow(style_updated) == 0L) next

      # renaming column variable
      if ("column" %in% names(style_updated)) {
        needs_rename <- !style_updated$column %in% merge_vars & !is.na(style_updated$column)
        if (any(needs_rename)) {
          style_updated$column[needs_rename] <- paste0(style_updated$column[needs_rename], "_", i)
        }
        style_updated$column <- as.character(style_updated$column)
      }

      # updating column names in rows expr/quo
      if ("rows" %in% names(style_updated)) {
        style_updated$rows <-
          lapply(style_updated$rows, function(.x)
            .rename_variables_in_expression(.x, i, tbls[[i]], merge_vars = merge_vars))
      }

      # updating column names in pattern string
      if ("pattern" %in% names(style_updated)) {
        style_updated$pattern <-
          vapply(style_updated$pattern, function(.x)
            .rename_variables_in_pattern(.x, i, tbls[[i]], merge_vars = merge_vars),
            character(1))
      }

      parts[[i]] <- style_updated
    }
    parts <- Filter(Negate(is.null), rev(parts))
    if (length(parts) > 0L) {
      x$table_styling[[style_type]] <- tibble::as_tibble(vctrs::vec_rbind(!!!parts))
    }
  }

  # take the first non-NULL element from tbls[[.]]
  for (style_type in c("caption")) {
    x$table_styling[[style_type]] <-
      map(seq_along(tbls), ~ getElement(tbls, .x) |> getElement("table_styling") |> getElement(style_type)) %>%
      reduce(.f = \(.x, .y) .x %||% .y)
  }

  # rename variables in expressions, and take first non-NULL element
  for (style_type in "horizontal_line_above") {
    x$table_styling[[style_type]] <-
      map(
        seq_along(tbls),
        ~ .rename_variables_in_expression(
          rows = getElement(tbls, .x) |> getElement("table_styling") |> getElement(style_type),
          id = .x,
          tbl = tbls[[.x]],
          merge_vars = merge_vars
        )
      ) %>%
      reduce(.f = \(.x, .y) .x %||% .y)
  }

  x
}

.rename_variables_in_expression <- function(rows, id, tbl, merge_vars) {
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
    setdiff(merge_vars) %>%
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

.rename_variables_in_pattern <- function(pattern, id, tbl, merge_vars) {
  # get all variable names in expression to be renamed
  columns <- tbl$table_styling$header$column
  var_list <-
    str_extract_all(pattern, "\\{.*?\\}") %>%
    map(~ str_remove_all(.x, pattern = "}", fixed = TRUE)) %>%
    map(~ str_remove_all(.x, pattern = "{", fixed = TRUE)) %>%
    unlist() %>%
    setdiff(merge_vars) %>%
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

.check_merge_likeness <- function(tbls, merge_vars) {
  tbl1_nrow <- nrow(tbls[[1]]$table_body)

  # check the number of rows is the same in all tbls
  for (i in seq_along(tbls)[-1]) {
    if (tbl1_nrow != nrow(tbls[[i]]$table_body)) {
      cli::cli_inform(
        c("The number rows in the tables to be merged do not match,
           which {.emph may} result in rows appearing out of order.",
          i = "See {.help [{.fun tbl_merge}](gtsummary::tbl_merge)} help file for details.
               Use {.code quiet=TRUE} to silence message."),
        call = get_cli_abort_call()
      )
      return(invisible())
    }
  }

  # check merge-on variables uniquely identify the rows
  for (i in seq_along(tbls)) {
    if (nrow(tbls[[i]]$table_body) != nrow(dplyr::distinct(tbls[[i]]$table_body[merge_vars]))) {
      cli::cli_inform(
        c("The {.arg merge_vars} columns to do uniquely identify rows in all {.arg tbls},
           which may result in rows appearing out of order.",
          i = "See {.help [{.fun tbl_merge}](gtsummary::tbl_merge)} help file for details.
               Use {.code quiet=TRUE} to silence message."),
        call = get_cli_abort_call()
      )
      return(invisible())
    }
  }
}

