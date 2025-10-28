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

  # will return call, and all arguments passed to tbl_stack
  func_inputs <- as.list(environment())

  # stack tables ---------------------------------------------------------------
  # first, save a string of the new tbl ID column
  tbl_id_colname <- .tbl_id_varname(tbls)

  # stack the `table_body` data frames
  results <- list()
  results$table_body <-
    map2(
      tbls, seq_along(tbls),
      function(tbl, id) {
        # adding a table ID and group header
        table_body <- tbl[["table_body"]] |> dplyr::mutate("{tbl_id_colname}" := id)

        # add ID label column if specified
        if (!is_empty(tbl_id_lbls)) {
          table_body <- table_body |>
            dplyr::mutate("{tbl_id_colname}_lbl" := tbl_id_lbls[id])
        }

        if (!is.null(group_header)) {
          table_body <-
            table_body |>
            dplyr::mutate(groupname_col = group_header[id])
        }

        table_body |>
          dplyr::select(any_of(c("groupname_col")), matches("^tbl_id\\d+$"), matches("^tbl_id\\d+_lbl$"), everything())
      }
    ) %>%
    dplyr::bind_rows()

  # creating table styling -----------------------------------------------------
  # print message if column headers, footnotes, etc. are different among tbls
  if (identical(quiet, FALSE)) .print_stack_differences(tbls)

  results$table_styling$header <-
    map(
      union(attr_order, seq_along(tbls)),
      ~ tbls[[.x]][["table_styling"]][["header"]]
    ) |>
    dplyr::bind_rows() |>
    dplyr::filter(.by = "column", dplyr::row_number() == 1)

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
              map(df$rows, ~ .add_tbl_id_to_quo(.x, tbls[[i]]$table_body, i, tbl_id_colname, style_type))
          }
          df |>
            dplyr::mutate_at(vars(any_of(c(
              "column", "text_interpret",
              "footnote", "format_type", "symbol"
            ))), as.character)
        }
      ) |>
      dplyr::bind_rows()
  }

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
  results <-
    modify_table_styling(
      results,
      any_of("groupname_col"),
      label = get_theme_element("tbl_stack-str:group_header", default = "**Group**"),
      align = "left",
      hide = FALSE
    )

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

.add_tbl_id_to_quo <- function(x, table_body, tbl_id, tbl_id_colname, style_type) {
  # if NULL AND style_type is a type that adds header changes (i.e. footnote in header),
  # then just return the NULL
  # the requires the stacking to pick one of the header footnotes and use it
  # the others will be discarded when printed.
  row_is_null <- eval_tidy(x, data = table_body) |> is.null()
  if (row_is_null && style_type %in% c("footnote", "footnote_abbrev")) {
    return(x)
  }

  # otherwise if NULL, add the tbl_id condition
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
    map(tbls, ~dplyr::select(.x$table_body, matches("^tbl_id\\d+$")) |> names()) |>
    unlist() |>
    unique() |>
    discard(is.na)

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
