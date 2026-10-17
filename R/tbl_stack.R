#' Stack tables
#'
#' Assists in patching together more complex tables. `tbl_stack()` appends two
#' or more gtsummary tables.
#'
#' @inheritParams tbl_merge
#' @param tbls (`list`)\cr
#'   List of gtsummary objects
#' @param group_header (`character`)\cr
#'   Character vector with table headers where length matches the length of `tbls`
#' @param attr_order (`integer`) \cr
#'   Set the order table attributes are set.
#'   Tables are stacked in the order they are passed in the `tbls` argument:
#'   use `attr_order` to specify the order the table attributes take precedent.
#'   For example, to use the header from the second table specify `attr_order=2`.
#'   Default is to set precedent in the order tables are passed.
#' @param quiet (scalar `logical`)\cr
#'   Logical indicating whether to suppress additional messaging. Default is `FALSE`.
#' @param tbl_id_lbls (`vector`)\cr
#'   Optional vector of the same length `tbls`.
#'   When specified a new, hidden column is added to the returned `.$table_body`
#'   with these labels. _The most common use case of this argument is for
#'   the development of other functions._
#'
#' @author Daniel D. Sjoberg
#' @export
#' @return A `tbl_stack` object
#'
#' @examplesIf (identical(Sys.getenv("NOT_CRAN"), "true") || identical(Sys.getenv("IN_PKGDOWN"), "true")) && gtsummary:::is_pkg_installed("survival", ref = "cardx")
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
#' tbl_stack(list(t1, t2))
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
#' # first merging, then stacking
#' row1 <- tbl_merge(list(t1, t3), tab_spanner = c("Tumor Response", "Death"))
#' row2 <- tbl_merge(list(t2, t4))
#'
#' tbl_stack(list(row1, row2), group_header = c("Unadjusted Analysis", "Adjusted Analysis"))
tbl_stack <- function(tbls,
                      group_header = NULL,
                      quiet = FALSE,
                      attr_order = seq_along(tbls),
                      tbl_ids = NULL,
                      tbl_id_lbls = NULL) {
  set_cli_abort_call()

  # check inputs ---------------------------------------------------------------
  check_class(tbls, "list")
  walk(tbls, ~check_class(.x, "gtsummary", message = "Each element of the list {.arg tbls} must be class {.cls gtsummary}."))
  check_scalar_logical(quiet)
  check_integerish(attr_order)
  check_range(attr_order, range = c(1L, length(tbls)), include_bounds = c(TRUE, TRUE))
  check_class(group_header, cls = "character", allow_empty = TRUE)
  check_length(group_header, length = length(tbls), allow_empty = TRUE)
  check_class(tbl_ids, cls = "character", allow_empty = TRUE)
  if (!is_empty(tbl_ids)) {
    check_identical_length(tbls, tbl_ids)
  }
  if (!is_empty(tbl_id_lbls)) {
    check_identical_length(tbls, tbl_id_lbls)
  }

  # stack tables ---------------------------------------------------------------
  # first, save a string of the new tbl ID column
  tbl_id_colname <- .tbl_id_varname(tbls)
  tbl_id_lbl_colname <- paste0(tbl_id_colname, "_lbl")

  # stack the `table_body` data frames
  results <- list()
  results$table_body <-
    map2(
      tbls, seq_along(tbls),
      function(tbl, id) {
        # adding a table ID and group header
        table_body <- tbl[["table_body"]]
        table_body[[tbl_id_colname]] <- id

        # add ID label column if specified
        if (!is_empty(tbl_id_lbls)) {
          table_body[[tbl_id_lbl_colname]] <- tbl_id_lbls[id]
        }

        if (!is.null(group_header)) {
          table_body[["groupname_col"]] <- group_header[id]
        }

        # equivalent to select(any_of("groupname_col"), matches("^tbl_id\\d+$"),
        #                      matches("^tbl_id\\d+_lbl$"), everything())
        nms <- names(table_body)
        first_cols <- c(
          intersect("groupname_col", nms),
          nms[grepl("^tbl_id[0-9]+$", nms)],
          nms[grepl("^tbl_id[0-9]+_lbl$", nms)]
        )
        table_body[c(first_cols, setdiff(nms, first_cols))]
      }
    ) %>%
    dplyr::bind_rows()

  # creating table styling -----------------------------------------------------
  # print message if column headers, footnotes, etc. are different among tbls
  if (identical(quiet, FALSE)) .print_stack_differences(tbls)

  header <-
    map(
      union(attr_order, seq_along(tbls)),
      ~ tbls[[.x]][["table_styling"]][["header"]]
    ) |>
    dplyr::bind_rows()
  results$table_styling$header <- header[!duplicated(header$column), ]

  # lazily-built, reusable data mask per table for `rows` evaluation (built at
  # most once per table, then reused across every style type below)
  lst_get_mask <- map(tbls, function(.tbl) {
    force(.tbl)
    mask <- NULL
    function() {
      if (is.null(mask)) mask <<- rlang::as_data_mask(.tbl$table_body)
      mask
    }
  })

  # cycle over each of the styling tibbles and stack them in reverse order -----
  for (style_type in c("spanning_header", "footnote_header", "footnote_body",
                       "footnote_spanning_header", "abbreviation", "source_note",
                       "fmt_fun", "post_fmt_fun", "text_format", "indent",
                       "fmt_missing", "cols_merge")) {
    results$table_styling[[style_type]] <-
      map(
        rev(union(attr_order, seq_along(tbls))),
        function(i) {
          df <- tbls[[i]]$table_styling[[style_type]]
          if ("rows" %in% names(df) && nrow(df) > 0) {
            # adding tbl_id to the rows specifications,
            # e.g. data$tbl_id == 1L & .data$row_type != "label"
            df$rows <-
              .add_tbl_id_to_quo_list(df$rows, lst_get_mask[[i]], i, tbl_id_colname)
          }
          # coerce shared columns to character so `bind_rows()` unifies types
          # (runs on 0-row tibbles too, where it can change a column's type)
          for (v in intersect(
            c("column", "text_interpret", "footnote", "format_type", "symbol"),
            names(df)
          )) {
            df[[v]] <- as.character(df[[v]])
          }
          df
        }
      ) |>
      dplyr::bind_rows()
  }

  # deduplicate header-level footnotes (no `rows` column) to avoid

  # duplicate superscripts when stacking tables with identical footnotes
  results$table_styling$footnote_header <-
    dplyr::distinct(results$table_styling$footnote_header)

  # combining rows spec for same column
  if (nrow(results$table_styling$cols_merge) > 0) {
    results$table_styling$cols_merge <-
      results$table_styling$cols_merge |>
      tidyr::nest(rows = "rows") |>
      dplyr::mutate(rows = map(.data$rows, ~ .x$rows |> unlist()))

    results$table_styling$cols_merge$rows <-
      map(
        results$table_styling$cols_merge$rows,
        ~ .x |> reduce(function(.x1, .y1) expr(!!quo_squash(.x1) | !!quo_squash(.y1)))
      )
  }

  # take the first non-NULL element from tbls[[.]]
  for (style_type in c("caption", "horizontal_line_above")) {
    results$table_styling[[style_type]] <-
      map(seq_along(tbls), ~ tbls[[.x]][["table_styling"]][[style_type]]) |>
      reduce(.f = \(.x, .y) .x %||% .y)
  }

  # adding label for grouping variable, if present -----------------------------
  class(results) <- c("tbl_stack", "gtsummary")

  # sync the header to the new `table_body` columns (tbl_id*, groupname_col),
  # then set the group-column attributes directly; equivalent to
  # `modify_table_styling(results, any_of("groupname_col"), label = ..., align = "left", hide = FALSE)`
  # without the selector/validation machinery (`call_list` is overwritten below either way)
  results <- .update_table_styling(results)
  if ("groupname_col" %in% results$table_styling$header$column) {
    idx <- results$table_styling$header$column == "groupname_col"
    results$table_styling$header$label[idx] <-
      get_theme_element("tbl_stack-str:group_header", default = "**Group**")
    results$table_styling$header$interpret_label[idx] <- "gt::md"
    results$table_styling$header$align[idx] <- "left"
    results$table_styling$header$hide[idx] <- FALSE
  }

  # add objects to the returned tbl --------------------------------------------
  results$call_list <- list(tbl_stack = match.call())
  results$tbls <- tbls

  # add tbl_ids, if specified --------------------------------------------------
  if (!is_empty(tbl_ids)) {
    names(results$tbls) <- tbl_ids
  }

  # returning results ----------------------------------------------------------
  results
}

# function prints changes to column labels and spanning headers
.print_stack_differences <- function(tbls) {
  any_header_difference <-
    lapply(
      tbls,
      FUN = \(x) {
        x[["table_styling"]][["header"]] |>
          dplyr::filter(!.data$hide) |>
          dplyr::select("column", "label")
      }
    ) |>
    dplyr::bind_rows() |>
    dplyr::mutate(
      .by = "column",
      label_difference = .data$label != .data$label[1]
    ) |>
    dplyr::pull("label_difference") |>
    any()

  # if there are difference, print them to the console
  if (any_header_difference) {
    cli::cli_inform(
      c("Column headers among stacked tables differ.",
        i = "Use {.fun modify_header} to update or {.code quiet = TRUE} to suppress this message.")
    )

    walk(
      seq_along(tbls),
      ~ tbls[[.x]] |>
        getElement("table_styling") |>
        getElement("header") |>
        dplyr::filter(!hide) |>
        dplyr::select("column", "label") |>
        dplyr::mutate(label =  cli::cli_format(.data$label)) |>
        tibble_as_cli(label = list(column = glue("Table {.x} Column Name"), label = "Header"))
    )
  }

  return(invisible())
}

# apply `.add_tbl_id_to_quo()` over a list of `rows` expressions, computing each
# unique expression only once (many styling rows share the same `rows` quosure).
# `.add_tbl_id_to_quo()` is a pure function of its arguments, so identical inputs
# yield identical outputs.
.add_tbl_id_to_quo_list <- function(rows_list, get_mask, tbl_id, tbl_id_colname) {
  n <- length(rows_list)
  result <- vector("list", n)
  seen_in <- list()
  seen_out <- list()

  for (j in seq_len(n)) {
    hit <- NULL
    for (k in seq_along(seen_in)) {
      if (identical(seen_in[[k]], rows_list[[j]])) {
        hit <- k
        break
      }
    }
    if (is.null(hit)) {
      out <- .add_tbl_id_to_quo(rows_list[[j]], get_mask, tbl_id, tbl_id_colname)
      seen_in[[length(seen_in) + 1L]] <- rows_list[[j]]
      seen_out[[length(seen_out) + 1L]] <- out
      result[[j]] <- out
    } else {
      result[[j]] <- seen_out[[hit]]
    }
  }

  result
}

.add_tbl_id_to_quo <- function(x, get_mask, tbl_id, tbl_id_colname) {
  # cheap short-circuit for the common literal-NULL cases, avoiding evaluation;
  # otherwise fall back to evaluating against the (memoized) data mask, since a
  # quosure can evaluate to NULL without being the NULL quosure
  row_is_null <-
    is.null(x) ||
    (is_quosure(x) && quo_is_null(x)) ||
    is.null(eval_tidy(x, data = get_mask()))

  # if NULL, add the tbl_id condition
  if (row_is_null) {
    return(expr(!!sym(tbl_id_colname) == !!tbl_id))
  }

  # if quosure, add tbl_id
  if (inherits(x, "quosure")) {
    return(
      quo(!!sym(tbl_id_colname) == !!tbl_id & (!!rlang::f_rhs(x))) %>%
        structure(.Environment = attr(x, ".Environment"))
    )
  }

  # if expression, add tbl_id
  expr(!!sym(tbl_id_colname) == !!tbl_id & (!!x))
}


.tbl_id_varname <- function(tbls) {
  # get column names that begin with 'tbl_id##'
  tbl_id_colnames <-
    lapply(tbls, function(x) grep("^tbl_id[0-9]+$", names(x$table_body), value = TRUE)) |>
    unlist() |>
    unique()

  # return 'tbl_id1' if no columns found
  if (is_empty(tbl_id_colnames)) {
    return("tbl_id1")
  }

  # if there are other tbl_id columns, return the next in the sequence
  tbl_max_id <-
    sub(pattern = "^tbl_id", replacement = "", x = tbl_id_colnames) |>
    as.integer() |>
    max()

  return(paste0("tbl_id", tbl_max_id + 1L))
}
