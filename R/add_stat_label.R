#' Add statistic labels
#'
#' `r lifecycle::badge('questioning')`\cr
#' Adds or modifies labels describing the summary statistics presented for
#' each variable in a [`tbl_summary()`] table.
#'
#' @param x (`tbl_summary`)\cr
#'   Object with class `'tbl_summary'` or with class `'tbl_svysummary'`
#' @param location (`string`)\cr
#'   Location where statistic label will be included.
#'  `"row"` (the default) to add the statistic label to the variable label row,
#'  and `"column"` adds a column with the statistic label.
#' @param label ([`formula-list-selector`][syntax])\cr
#'   indicates the updates to the statistic label, e.g. `label = all_categorical() ~ "No. (%)"`.
#'   When not specified, the default statistic labels are used.
#' @inheritParams rlang::args_dots_empty
#'
#' @section Tips:
#'
#' When using `add_stat_label(location='row')` with subsequent `tbl_merge()`,
#' it's important to have somewhat of an understanding of the underlying
#' structure of the gtsummary table.
#' `add_stat_label(location='row')` works by adding a new column called
#' `"stat_label"` to `x$table_body`. The `"label"` and `"stat_label"`
#' columns are merged when the gtsummary table is printed.
#' The `tbl_merge()` function merges on the `"label"` column (among others),
#' which is typically the first column you see in a gtsummary table.
#' Therefore, when you want to merge a table that has run `add_stat_label(location='row')`
#' you need to match the `"label"` column values before the `"stat_column"`
#' is merged with it.
#'
#' For example, the following two tables merge properly
#'
#' ```r
#' tbl1 <- trial %>% select(age, grade) |> tbl_summary() |> add_stat_label()
#' tbl2 <- lm(marker ~ age + grade, trial) |> tbl_regression()
#'
#' tbl_merge(list(tbl1, tbl2))
#' ```
#'
#' The addition of the new `"stat_label"` column requires a default
#' labels for categorical variables, which is  `"No. (%)"`. This
#' can be changed to either desired text or left blank using `NA_character_`.
#' The blank option is useful in the `location="row"` case to keep the
#' output for categorical variables identical what was produced without
#' a `"add_stat_label()"` function call.
#'
#' @author Daniel D. Sjoberg
#' @name add_stat_label
#' @return A `tbl_summary` or `tbl_svysummary` object
#'
#' @examples
#' tbl <- trial |>
#'   dplyr::select(trt, age, grade, response) |>
#'   tbl_summary(by = trt)
#'
#' # Example 1 ----------------------------------
#' # Add statistic presented to the variable label row
#' tbl |>
#'   add_stat_label(
#'     # update default statistic label for continuous variables
#'     label = all_continuous() ~ "med. (iqr)"
#'   )
#'
#' # Example 2 ----------------------------------
#' tbl |>
#'   add_stat_label(
#'     # add a new column with statistic labels
#'     location = "column"
#'   )
#'
#' # Example 3 ----------------------------------
#' trial |>
#'   select(age, grade, trt) |>
#'   tbl_summary(
#'     by = trt,
#'     type = all_continuous() ~ "continuous2",
#'     statistic = all_continuous() ~ c("{median} ({p25}, {p75})", "{min} - {max}"),
#'   ) |>
#'   add_stat_label(label = age ~ c("IQR", "Range"))
NULL

#' @export
#' @rdname add_stat_label
add_stat_label <- function(x, ...) {
  check_not_missing(x)
  check_class(x, "gtsummary")
  UseMethod("add_stat_label")
}

#' @export
#' @rdname add_stat_label
add_stat_label.tbl_summary <- function(x, location = c("row", "column"), label = NULL, ...) {
  set_cli_abort_call()
  check_dots_empty()
  updated_call_list <- c(x$call_list, list(add_stat_label = match.call()))

  # check inputs ---------------------------------------------------------------
  check_not_missing(label)
  if ("add_stat_label" %in% names(x$call_list)) {
    cli::cli_inform("{.code add_stat_label()} has previously been applied. Returning {.pkg gtsummary} table unaltered.")
    return(x)
  }

  # process arguments ----------------------------------------------------------
  if (missing(location)) {
    location <- get_theme_element("add_stat_label-arg:location", default = location)
    location <- arg_match(location)
  }

  cards::process_formula_selectors(
    scope_table_body(x$table_body),
    label = label
  )
  # check the label values are all string (except continuous2)
  imap(
    label[intersect(names(label), x$inputs$type |> discard(~.x == "continuous2") |> names())],
    function(.x, .y) {
      if (!is_string(.x)) {
        cli::cli_abort(
          "Elements of the {.arg label} argument for variable {.val {.y}} must be a string of length 1.",
          all = get_cli_abort_call()
        )
      }
    }
  )
  updated_variables <- names(label) # variable whose stat labels were updated from the defaults
  cards::fill_formula_selectors(
    scope_table_body(x$table_body),
    label = .add_stat_label_default_label_arg(x, statistic = x$inputs$statistic)
  )

  # create df_stat_label to merge into `.$table_body`,
  # (does not include continuous2 variables)
  df_stat_label <-
    x$table_body[c("variable", "var_type")] |>
    dplyr::distinct() |>
    dplyr::filter(!.data$var_type %in% "continuous2") |>
    dplyr::mutate(
      stat_label = map_chr(.data$variable, ~label[[.x]]),
      row_type =
        ifelse(
          .env$location %in% "column" & .data$var_type %in% "categorical",
          "level",
          "label"
        ) |>
        as.character()
    )

  if (location %in% "column") {
    df_stat_label <-
      df_stat_label %>%
      dplyr::bind_rows(
        dplyr::select(., "variable", "var_type") |>
          dplyr::mutate(
            row_type = "missing",
            stat_label =
              glue::glue(
                x$inputs$missing_stat,
                .envir = list(N_obs = "N",
                              N_miss = "n",
                              N_nonmiss = "N - n",
                              p_miss = "p",
                              p_nonmiss = "1 - p")
              )
          )
      )
  }

  # update the label column for continuous2 variables --------------------------
  cont2_vars_with_new_label <- names(keep(x$inputs$type, ~ .x == "continuous2")) |> intersect(updated_variables)
  if (!is_empty(cont2_vars_with_new_label)) {
    # first, check the dimension of the passed value
    walk(
      cont2_vars_with_new_label,
      function(.x) {
        if (!is.character(label[[.x]]) || length(label[[.x]]) != length(x$inputs$statistic[[.x]])) {
          cli::cli_abort(
            "The element of the {.arg label} argument for variable {.val {.x}}
             must be a string of length {.val {length(x$inputs$statistic[[.x]])}}.",
            call = get_cli_abort_call()
          )
        }
      }
    )

    # now update the label column of .$table_body
    for (variable in cont2_vars_with_new_label) {
      x$table_body$label[x$table_body$variable %in% variable & x$table_body$row_type %in% "level"] <-
        label[[variable]]
    }
  }

  # add df_stat_label to `.$table_body` ----------------------------------------
  x <- x |>
    modify_table_body(
      ~ dplyr::left_join(
        .x,
        df_stat_label,
        by = c("variable", "var_type", "row_type")
      ) |>
        dplyr::relocate("stat_label", .after = "label")
    ) |>
    modify_table_styling(
      columns = "stat_label",
      hide = location %in% "row",
      label = paste0("**", translate_string("Statistic"), "**")
    ) |>
    # removing stat footnote, since it's in the table now
    modify_table_styling(
      columns = all_stat_cols(),
      footnote = NA_character_
    )

  if (location %in% "row") {
    x <- x |>
      modify_table_styling(
        columns = "label",
        rows = !is.na(.data$stat_label),
        cols_merge_pattern = "{label}, {stat_label}"
      )
  }

  # keeping track of all functions previously run ------------------------------
  # fill in the Ns in the header table modify_stat_* columns
  x <- .fill_table_header_modify_stats(x)
  x$call_list <- updated_call_list

  x
}

#' @export
#' @rdname add_stat_label
add_stat_label.tbl_svysummary <- add_stat_label.tbl_summary

.add_stat_label_default_label_arg <- function(x, statistic) {
  statistic |>
    imap(
      function(full_statistic, variable) {
        map_chr(
          full_statistic,
          function(sub_statistic) {
            eval_tidy(
              expr(glue::glue(gsub("\\{(p|p_miss|p_nonmiss|p_unweighted)\\}%", "{\\1}", x = sub_statistic))),
              cards::get_ard_statistics(
                x$cards[[1]] |>
                  dplyr::filter(.data$variable %in% variable) |>
                  dplyr::distinct(.data$stat_name, .data$stat_label),
                .column = "stat_label"
              )
            )
          }
        )
      }
    )
}
