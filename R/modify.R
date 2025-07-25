#' Modify column headers and spanning headers
#'
#' @description
#' These functions assist with modifying the aesthetics/style of a table.
#'
#' - `modify_header()` update column headers
#' - `modify_spanning_header()` update/add spanning headers
#'
#' The functions often require users to know the underlying column names.
#' Run `show_header_names()` to print the column names to the console.
#'
#' @param x (`gtsummary`)\cr
#'   A gtsummary object
#' @param ... [`dynamic-dots`][rlang::dyn-dots]\cr
#'   Used to assign updates to headers and spanning headers.
#'
#'   Use `modify_*(colname='new header')` to update a single column. Using a
#'   formula will invoke tidyselect, e.g. `modify_*(all_stat_cols() ~ "**{level}**")`.
#'   The dynamic dots allow syntax like `modify_header(x, !!!list(label = "Variable"))`.
#'   See examples below.
#'
#'   Use the `show_header_names()` to see the column names that can be modified.
#' @param text_interpret (`string`)\cr
#'   String indicates whether text will be interpreted with
#'   [`gt::md()`] or [`gt::html()`]. Must be `"md"` (default) or `"html"`.
#'   Applies to tables printed with `{gt}`.
#' @param level (`integer`)\cr
#'   An integer specifying which level to place the spanning header.
#' @param columns ([`tidy-select`][dplyr::dplyr_tidy_select])\cr
#'   Columns from which to remove spanning headers.
#' @param show_hidden (scalar `logical`)\cr
#'   Logical indicating whether to print hidden columns as well as printed columns.
#'   Default is `FALSE`.
#' @param update,quiet `r lifecycle::badge("deprecated")`
#' @param include_example `r lifecycle::badge("deprecated")`
#'
#' @author Daniel D. Sjoberg
#'
#' @return Updated gtsummary object
#' @name modify
#'
#' @section `tbl_summary()`, `tbl_svysummary()`, and `tbl_cross()`:
#' When assigning column headers and spanning headers,
#' you may use `{N}` to insert the number of observations.
#' `tbl_svysummary` objects additionally have `{N_unweighted}` available.
#'
#' When there is a stratifying `by=` argument present, the following fields are
#' additionally available to stratifying columns: `{level}`, `{n}`, and `{p}`
#' (`{n_unweighted}` and `{p_unweighted}` for `tbl_svysummary` objects)
#'
#' Syntax follows [`glue::glue()`], e.g. `all_stat_cols() ~ "**{level}**, N = {n}"`.
#' @section tbl_regression():
#' When assigning column headers for `tbl_regression` tables,
#' you may use `{N}` to insert the number of observations, and `{N_event}`
#' for the number of events (when applicable).
#'
#' @examplesIf (identical(Sys.getenv("NOT_CRAN"), "true") || identical(Sys.getenv("IN_PKGDOWN"), "true")) && gtsummary:::is_pkg_installed(c("cardx", "broom", "broom.helpers"))
#' # create summary table
#' tbl <- trial |>
#'   tbl_summary(by = trt, missing = "no", include = c("age", "grade", "trt")) |>
#'   add_p()
#'
#' # print the column names that can be modified
#' show_header_names(tbl)
#'
#' # Example 1 ----------------------------------
#' # updating column headers
#' tbl |>
#'   modify_header(label = "**Variable**", p.value = "**P**")
#'
#' # Example 2 ----------------------------------
#' # updating headers add spanning header
#' tbl |>
#'   modify_header(all_stat_cols() ~ "**{level}**, N = {n} ({style_percent(p)}%)") |>
#'   modify_spanning_header(all_stat_cols() ~ "**Treatment Received**")
NULL

#' @name modify
#' @export
modify_header <- function(x, ..., text_interpret = c("md", "html"),
                          quiet, update) {
  set_cli_abort_call()
  updated_call_list <- c(x$call_list, list(modify_footnote = match.call()))

  # checking inputs ------------------------------------------------------------
  check_class(x, "gtsummary")
  text_interpret <- arg_match(text_interpret)

  # process inputs -------------------------------------------------------------
  dots <- dots_list(...)
  dots <-
    .deprecate_modify_update_and_quiet_args(
      dots = dots, update = update, quiet = quiet, calling_fun = "modify_header"
    )


  cards::process_formula_selectors(data = scope_header(x$table_body, x$table_styling$header), dots = dots)
  cards::check_list_elements(
    x = dots,
    predicate = function(x) is_string(x),
    error_msg =
      c("All values passed in {.arg ...} must be strings.",
        "i" = "For example, {.code label='**Variable**'}"
      )
  )

  # evaluate the strings with glue
  dots <- .evaluate_string_with_glue(x, dots)

  # updated header meta data
  x <-
    modify_table_styling(
      x = x,
      columns = names(dots),
      label = unlist(dots),
      hide = FALSE,
      text_interpret = text_interpret
    )

  # return object
  x$call_list <- updated_call_list
  x
}

#' @name modify
#' @export
modify_spanning_header <- function(x, ..., text_interpret = c("md", "html"),
                                   level = 1L,
                                   quiet, update) {
  set_cli_abort_call()
  updated_call_list <- c(x$call_list, list(modify_spanning_header = match.call()))

  # checking inputs ------------------------------------------------------------
  check_class(x, "gtsummary")
  check_scalar_integerish(level)
  if (level < 1) {
    cli::cli_abort(
      "The {.arg level} argument must be a positive integer.",
      call = get_cli_abort_call()
    )
  }
  text_interpret <- arg_match(text_interpret)

  # process inputs -------------------------------------------------------------
  dots <- rlang::dots_list(...)
  dots <- .deprecate_modify_update_and_quiet_args(dots, update, quiet, calling_fun = "modify_spanning_header")

  cards::process_formula_selectors(data = scope_header(x$table_body, x$table_styling$header), dots = dots)
  cards::check_list_elements(
    x = dots,
    predicate = function(x) is_string(x) || is.na(x),
    error_msg =
      c("All values passed in {.arg ...} must be strings.",
        "i" = "For example, {.code all_stat_cols() ~ '**Treatment**'}"
      )
  )

  # evaluate the strings with glue
  dots <- .evaluate_string_with_glue(x, dots)

  # updated header meta data
  x <-
    .modify_spanning_header(
      x = x,
      level = level,
      columns = names(dots),
      spanning_header = unlist(dots),
      text_interpret = text_interpret,
      remove = FALSE
    )

  # return object
  x$call_list <- updated_call_list
  x
}

#' @name modify
#' @export
remove_spanning_header <- function(x, columns = everything(), level = 1L) {
  set_cli_abort_call()
  updated_call_list <- c(x$call_list, list(remove_spanning_header = match.call()))

  # checking inputs ------------------------------------------------------------
  check_class(x, "gtsummary")
  check_scalar_integerish(level)
  if (level < 1) {
    cli::cli_abort(
      "The {.arg level} argument must be a positive integer.",
      call = get_cli_abort_call()
    )
  }

  # process inputs -------------------------------------------------------------
  cards::process_selectors(data = scope_header(x$table_body, x$table_styling$header), columns = {{ columns }})

  # updated header meta data
  x <-
    .modify_spanning_header(
      x = x,
      level = level,
      columns = columns,
      spanning_header = rep_len(NA_character_, length.out = length(columns)),
      remove = TRUE
    )

  # return object
  x$call_list <- updated_call_list
  x
}

.modify_spanning_header <- function(x, columns, spanning_header, level = 1L, text_interpret = "md", remove = FALSE) {
  # add updates to `x$table_styling$spanning_header` ---------------------------
  x$table_styling$spanning_header <-
    x$table_styling$spanning_header |>
    dplyr::bind_rows(
      dplyr::tibble(
        level = level,
        column = columns,
        spanning_header = unname(spanning_header),
        text_interpret = paste0("gt::", text_interpret),
        remove = remove
      )
    )

  # return table ---------------------------------------------------------------
  x
}

#' @name modify
#' @export
show_header_names <- function(x, show_hidden = FALSE, include_example, quiet) {
  # checking input -------------------------------------------------------------
  check_class(x, "gtsummary")
  check_scalar_logical(show_hidden)

  # deprecated arguments -------------------------------------------------------
  if (!missing(include_example)) {
    lifecycle::deprecate_warn(
      when = "2.0.0",
      what = "gtsummary::show_header_names(include_example)",
      details = "Argument has been ignored."
    )
  }
  if (!missing(quiet)) {
    lifecycle::deprecate_warn(
      when = "2.0.0",
      what = "gtsummary::show_header_names(quiet)",
      details = "Argument has been ignored."
    )
  }

  # printing info --------------------------------------------------------------
  df_print <-
    x$table_styling$header %>%
    {case_switch(
      !show_hidden ~ dplyr::filter(., !.data$hide),
      .default = dplyr::mutate(., column = ifelse(.data$hide, paste0(.data$column, "\U2020"), .data$column))
    )} |>
    dplyr::select("column", "label", starts_with("modify_stat_"))

  # save column class abbreviations
  column_cls_abbreviation <- df_print |>
    dplyr::select(starts_with("modify_stat_")) |>
    map(.get_class_abbreviation)

  # round stats
  df_print <- df_print |>
    dplyr::mutate(
      across(where(is_integerish), label_style_number()),
      across(where(is.numeric), label_style_sigfig(digits = 3)),
      across(-c(where(is.integer) | where(is.numeric)), as.character),
      label = cli::cli_format(.data$label)
    )

  # append class abbreviations
  for (v in names(column_cls_abbreviation)) {
    df_print[[v]] <-
      ifelse(
        !is.na(df_print[[v]]),
        paste(df_print[[v]], column_cls_abbreviation[[v]]),
        df_print[[v]]
      )
  }

  # if any columns begin with 'modify_stat_', then rename
  if (any(str_detect(names(df_print), "^modify_stat_"))) {
    df_print <- df_print |>
      dplyr::mutate(
        across(
          .cols = starts_with("modify_stat_"),
          .fns = \(.x) {
            ifelse(
              !is.na(.x),
              str_pad(.x, width = max(nchar(.x), na.rm = TRUE), side = "left", pad = " "),
              .x
            )
          }
        )
      ) |>
      dplyr::rename_with(
        .fn = ~ str_remove(., pattern = "^modify_stat_") |> paste0("*"),
        .cols = starts_with("modify_stat_")
      )
  }

  tibble_as_cli(df_print, label = list(column = "Column Name", label = "Header"))

  msg <-
    c("* These values may be dynamically placed into headers (and other locations).",
      "i" = "Review the {.help [{.fun modify_header}](gtsummary::modify_header)} help for examples."
    )
  if (show_hidden) {
    msg <- append(msg, values = "\U2020 Hidden columns", after = 2)
  }

  cat("\n")
  cli::cli_inform(message = msg)
}

.get_class_abbreviation <- function(x) {
  dplyr::case_when(
    inherits(x, "integer") ~ "<int>",
    inherits(x, "numeric") ~ "<dbl>",
    inherits(x, "character") ~ "<chr>",
    inherits(x, "factor") ~ "<fct>",
    inherits(x, "logical") ~ "<lgl>",
    inherits(x, "Date") ~ "<date>",
    inherits(x, "POSIXct") ~ "<dttm>",
    inherits(x, "POSIXlt") ~ "<dttm>",
    .default = "<???>"
  )
}


.evaluate_string_with_glue <- function(x, dots) {
  # only keep values that are in the table_body
  dots <- dots[intersect(names(dots), x$table_styling$header$column)]

  df_header_subset <-
    x$table_styling$header |>
    dplyr::select("column", starts_with("modify_stat_")) |>
    dplyr::rename_with(
      .fn = function(x) gsub("^modify_stat_", "", x),
      .cols = starts_with("modify_stat_")
    )

  imap(
    dots,
    function(value, variable) {
      df_header_subset <-
        df_header_subset |>
        dplyr::filter(.data$column %in% .env$variable) |>
        dplyr::select(-"column")

      glued_value <-
        cards::eval_capture_conditions(
          case_switch(
            is.na(value) ~ NA_character_,
            .default = expr(glue::glue(value))
          ),
          data = df_header_subset
        )

      if (!is.null(glued_value$result)) {
        return(glued_value$result)
      }

      cli::cli_abort(
        c("There was an error in the {.fun glue::glue} evaluation of {.val {value}} for column {.val {variable}}.",
          i = "Run {.fun gtsummary::show_header_names} for information on values available for glue interpretation."
        ),
        call = get_cli_abort_call()
      )
    }
  )
}

.deprecate_modify_update_and_quiet_args <- function(dots, update, quiet, calling_fun) {
  # deprecated arguments
  if (!missing(update) || (!is_empty(dots) && is.list(dots[[1]]))) {
    lifecycle::deprecate_warn(
      "2.0.0", glue("gtsummary::{calling_fun}(update=)"),
      details =
        glue("Use `{calling_fun}(...)` input instead.
            Dynamic dots allow for syntax like `{calling_fun}(!!!list(...))`."),
      env = get_cli_abort_call()
    )

    if (!is_empty(dots) && is.list(dots[[1]])) dots <- c(dots[-1], dots[[1]]) # styler: off
    if (!missing(update)) dots <- c(dots, update) # styler: off

    dots
  }
  if (!missing(quiet)) {
    lifecycle::deprecate_warn(
      "2.0.0", glue("gtsummary::{calling_fun}(quiet=)"),
      details = "Argument has been ignored."
    )
  }

  dots
}
