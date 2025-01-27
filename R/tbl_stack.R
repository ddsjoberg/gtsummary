#' Stack tables
#'
#' Assists in patching together more complex tables. `tbl_stack()` appends two
#' or more gtsummary tables.
#' Column attributes, including number formatting and column footnotes, are
#' retained from the first passed gtsummary object.
#'
#' @param tbls (`list`)\cr
#'   List of gtsummary objects
#' @param group_header (`character`)\cr
#'   Character vector with table headers where length matches the length of `tbls`
#' @param quiet (scalar `logical`)\cr
#'   Logical indicating whether to suppress additional messaging. Default is `FALSE`.
#'
#' @author Daniel D. Sjoberg
#' @export
#' @return A `tbl_stack` object
#'
#' @examplesIf (identical(Sys.getenv("NOT_CRAN"), "true") || identical(Sys.getenv("IN_PKGDOWN"), "true")) && gtsummary:::is_pkg_installed("cardx") && gtsummary:::is_pkg_installed("survival", ref = "cardx")
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
tbl_stack <- function(tbls, group_header = NULL, header_type = c("auto", "grouped", "nested"), quiet = FALSE) {
  set_cli_abort_call()

  # check inputs ---------------------------------------------------------------
  check_class(tbls, "list")
  walk(tbls, ~check_class(.x, "gtsummary", message = "Each element of the list {.arg tbls} must be class {.cls gtsummary}."))
  check_scalar_logical(quiet)
  header_type <- rlang::arg_match(header_type)
  if (!is_empty(group_header) && header_type == "auto") {
    if (is.character(group_header)) header_type <- "grouped"
    else if (is.list(group_header)) header_type <- "nested"
    else {
      cli::cli_abort(
        "The {.arg group_header} argument must be `NULL`, a {.cls character} vector, or a {.cls list}.",
        call = get_cli_abort_call()
      )
    }
  }

  if (header_type == "grouped") {
    check_length(
      group_header,
      length = length(tbls),
      message = "When {.code header_type='grouped'}, the {.arg {arg_name}}
       argument must be the same length as the number of tables passed
      in the {.arg tbls} argument, that is, length {.val {length(tbls)}}."
    )
    check_class(
      group_header,
      cls = "character",
      message = "When {.code header_type='grouped'}, the {.arg group_header} must be a {.cls character} vector."
    )
  }
  else if (header_type == "nested") {
    .checks_for_nesting_stack(tbls, group_header)
  }


  # will return call, and all arguments passed to tbl_stack
  func_inputs <- as.list(environment())

  # stack tables ---------------------------------------------------------------
  # this function wraps another call to `tbl_stack()` while nesting the results
  if (header_type == "nested") {
    return(.create_a_nested_stack(tbls, group_header, call = match.call(), quiet))
  }

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
        if (!is.null(group_header)) {
          table_body <-
            table_body |>
            dplyr::mutate(groupname_col = group_header[id])
        }

        table_body |> dplyr::select(any_of(c("groupname_col")), matches("^tbl_id\\d+$"), everything())
      }
    ) %>%
      dplyr::bind_rows()

  # creating table styling -----------------------------------------------------
  # print message if column headers, footnotes, etc. are different among tbls
  if (identical(quiet, FALSE)) .print_stack_differences(tbls)

  results$table_styling$header <-
    map(tbls, ~ .x[["table_styling"]][["header"]]) |>
    dplyr::bind_rows() |>
    dplyr::filter(.by = "column", dplyr::row_number() == 1)

  # cycle over each of the styling tibbles and stack them in reverse order -----
  for (style_type in c("spanning_header", "footnote_header", "footnote_body",
                       "footnote_spanning_header", "abbreviation", "source_note",
                       "fmt_fun", "text_format", "indent", "fmt_missing", "cols_merge")) {
    results$table_styling[[style_type]] <-
      map(
        rev(seq_along(tbls)),
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

  # returning results ----------------------------------------------------------
  results$call_list <- list(tbl_stack = match.call())
  results$tbls <- tbls

  results
}

.create_a_nested_stack <- function(tbls, group_header, call, quiet) {
  # first non-hidden column
  first_non_hidden_col <- .first_unhidden_column(tbls[[1]])
  depth <- length(group_header[[1]])

  lst_df_group_headers <- .create_list_of_header_dfs(tbls, group_header)

  # add headers with their associated `tbl_indent_id`
  for (i in seq_along(tbls)) {
    # define indent ID for tbls
    tbls[[i]]$table_body$tbl_indent_id <- depth + 1L

    # indent the innermost table
    tbls[[i]]$table_styling$indent$n_spaces <- tbls[[i]]$table_styling$indent$n_spaces + depth * 4L

    # add nesting header rows
    tbls[[i]]$table_body <-
      dplyr::bind_rows(
        lst_df_group_headers[[i]]
        # dplyr::tibble(
        #   tbl_indent_id = seq_len(depth),
        #   "{first_non_hidden_col}" := group_header[[i]]
        # )
        ,
        tbls[[i]]$table_body
      )
  }

  # stack the tbls
  tbl <- tbl_stack(tbls = tbls, quiet = quiet)

  # cycle over the depth and indenting nesting headers
  for (d in seq_len(depth)) {
    tbl <- tbl |>
      gtsummary::modify_column_indent(
        columns = all_of(first_non_hidden_col),
        rows = !!rlang::expr(.data$tbl_indent_id == !!d),
        indent = (d - 1L) * 4L
      )
  }

  # add the function inputs
  tbl$call_list <- list(tbl_stack = call)

  # return final stacked tbl
  tbl
}

.checks_for_nesting_stack <- function(tbls, group_header) {
  check_class(group_header, cls = "list")
  walk(
    seq_along(group_header),
    \(i) {
      if (!is.character(group_header[[i]])) {
        cli::cli_abort(
          "When {.code header_type='nested'}, the {.arg group_header} must be a list of {.cls character} vectors.",
          call = get_cli_abort_call()
        )
      }
      if (.first_unhidden_column(tbls[[i]]) != .first_unhidden_column(tbls[[1]])) {
        cli::cli_abort(
          c("When {.code header_type='nested'}, the first column shown in each table must be the same.",
            "i" = "The first table prints the {.val {.first_unhidden_column(tbls[[1]])}}
                   in the first position, and table {.val {i}} has {.val {.first_unhidden_column(tbls[[i]])}}")
        )
      }
      if (!is.character(tbls[[i]]$table_body[[.first_unhidden_column(tbls[[i]])]])) {
        cli::cli_abort(
          "When {.code header_type='nested'}, the first column printed must be {.cls character},
           which is not the case for table {.val {i}} and column {.val {tbls[[i]]$table_body[[.first_unhidden_column(x)]]}}"
        )
      }
      if ("tbl_indent_id" %in% names(tbls[[i]]$table_body)) {
        cli::cli_abort(
          "Tables can only be stacked with {.code header_type='nested'} one time,
           and one or more of the tables passed in {.arg tbls} has been previously been stacked and nested."
        )
      }
    }
  )

  # check the lengths of the group_headers
  total_length <- map_int(group_header, length) |> reduce(.f = `*`)
  if (total_length != length(tbls)) {
      cli::cli_abort(
        "When {.code header_type='nested'}, the {.arg group_header} must be a list
         of {.cls character} vectors where the product of each of the lengths is
        equal to the number of tables passed in the {.arg tbls} argument.",
        call = get_cli_abort_call()
      )
  }
}


# function prints changes to column labels and spanning headers
.print_stack_differences <- function(tbls) {
  tbl_differences <-
    map2(
      tbls, seq_len(length(tbls)),
      ~ .x[["table_styling"]][["header"]] |>
        dplyr::mutate(..tbl_id.. = .y)
    ) |>
    dplyr::bind_rows() |>
    dplyr::select("..tbl_id..", "column", "label") |>
    tidyr::pivot_longer(cols = c("label")) |>
    dplyr::group_by(.data$column, .data$name) |>
    dplyr::mutate(
      new_value = .data$value[1],
      name_fmt = dplyr::case_when(
        name == "label" ~ "Column header"
      )
    ) |>
    dplyr::filter(.data$new_value != .data$value) |>
    dplyr::ungroup() |>
    dplyr::arrange(.data$name != "label", .data$name_fmt, .data$..tbl_id..)

  if (nrow(tbl_differences) > 0) {
    cli::cli_inform(
      c("Column headers among stacked tables differ. Headers from the first table are used.",
         i = "Use {.code quiet = TRUE} to suppress this message.")
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

# for nested stacking, this function returns a list of data frames to be used as the headers
.create_list_of_header_dfs <- function(tbls, group_header) {
  # create a data frame of all combinations of the grouping levels
  df_group_header <-
    tidyr::expand_grid(!!!setNames(group_header, as.character(1:2)))

  # remove the levels that do not need to be printed
  for (i in rev(seq_along(group_header))) {
    df_group_header <-
      df_group_header |>
      dplyr::mutate(
        .by = seq_len(i),
        "{i}" := ifelse(dplyr::row_number() == 1L, .data[[as.character(i)]], NA_character_)
      )
  }

  # return a list of data frames with the headers that will appear above each stacked tbl
  map(
    seq_len(nrow(df_group_header)),
    \(i) {
      df_group_header[i,] |>
        unlist() |>
        discard(is.na) %>%
        {dplyr::tibble(
          tbl_indent_id = names(.) |> as.integer(),
          "{.first_unhidden_column(tbls[[1]])}" := .

        )}
    }
  )
}
