#' Likert Summary
#'
#' `r lifecycle::badge("experimental")`\cr
#' Create a table of ordered categorical variables in a wide format.
#'
#' @inheritParams tbl_summary
#' @param statistic (`string`)\cr
#'   a string indicating the statistics to include.
#'   Use glue syntax to place statistics and select among the `'n'`, `'N'`, and `'p'`
#'   statistics. Default is `"{n} ({p}%)"`.
#' @param digits (named `list`)\cr
#'   named list the formatting functions to apply to the statistics.
#' @param sort (`string`)\cr
#'   indicates whether levels of variables should be placed in
#'   ascending order (the default) or descending.
#'
#' @return a 'tbl_likert' gtsummary table
#' @export
#'
#' @examples
#' levels <- c("Strongly Disagree", "Disagree", "Agree", "Strongly Agree")
#' df_likert <- data.frame(
#'   recommend_friend = sample(levels, size = 20, replace = TRUE) |> factor(levels = levels),
#'   regret_purchase = sample(levels, size = 20, replace = TRUE) |> factor(levels = levels)
#' )
#'
#' # Example 1 ----------------------------------
#' tbl_likert_ex1 <-
#'   df_likert |>
#'   tbl_likert() |>
#'   add_n()
#' tbl_likert_ex1
#'
#' # Example 2 ----------------------------------
#' # Add continuous summary of the likert scores
#' list(
#'   tbl_likert_ex1,
#'   tbl_wide_summary(
#'     df_likert |> dplyr::mutate(dplyr::across(everything(), as.numeric)),
#'     statistic = c("{mean}", "{sd}"),
#'     type = ~"continuous"
#'   )
#' ) |>
#'   tbl_merge(tab_spanner = FALSE)
tbl_likert <- function(data,
                       statistic = "{n} ({p}%)",
                       label = NULL,
                       digits = list(n = label_style_number(),
                                     N = label_style_number(),
                                     p = label_style_percent()),
                       include = everything(),
                       sort = c("ascending", "descending")) {
  set_cli_abort_call()

  # process inputs -------------------------------------------------------------
  check_not_missing(data)
  check_data_frame(data)
  check_class(digits, "list", allow_empty = TRUE)
  sort <- arg_match(sort)
  check_string(statistic)
  if (is_empty(.extract_glue_elements(statistic))) {
    cli::cli_abort("The {.arg statistic} argument string does not contain any glue element, e.g. {.val {{n}} ({{p}}%)}.",
                   call = get_cli_abort_call())
  }
  if (any(!.extract_glue_elements(statistic) %in% c("n", "N", "p"))) {
    not_valid_stat <- .extract_glue_elements(statistic) |> setdiff(c("n", "N", "p"))
    cli::cli_abort(c("Statistic(s) {.val {not_valid_stat}} are not valid.",
                     i = "Select one or more of {.val {c('n', 'N', 'p')}}"),
                   call = get_cli_abort_call())
  }

  cards::process_selectors(data, include = {{ include }})
  cards::process_formula_selectors(data, label = label)
  cards::check_list_elements(
    x = label,
    predicate = \(x) is_string(x),
    error_msg = "Values pass in {.arg label} argument must be strings."
  )

  # save processed function inputs ---------------------------------------------
  tbl_likert_inputs <- as.list(environment())
  call <- match.call()

  # check all variables are factors --------------------------------------------
  if (some(include, ~!inherits(data[[.x]], "factor"))) {
    not_fct <- map_chr(include, ~ifelse(!inherits(data[[.x]], "factor"), .x, NA_character_)) |> discard(is.na)
    cli::cli_abort(
      c("All variables in the {.arg include} argument must be {.cls factor}.",
        i = "Variables {.val {not_fct}} are not class {.cls factor}.")
    )
  }

  if (!is_named(digits)) {
    cli::cli_abort("The {.arg digits} argument must be a named list.", call = get_cli_abort_call())
  }
  cards::check_list_elements(
    digits,
    predicate = \(x) is_scalar_integerish(x) || is_function(x),
    error_msg = "The elements of the {.arg digits} argument list must be a scalar {.cls integer} or {.cls function}."
  )
  if (!missing(digits)) {
    digits <- eval(formals(gtsummary::tbl_likert)[["digits"]]) |>
      utils::modifyList(digits)
  }

  # check all factors have the same levels -------------------------------------
  walk(
    include[-1],
    \(x) {
      if (!identical(levels(data[[include[1]]]), levels(data[[x]]))) {
        cli::cli_abort(
          c("All variables in the {.arg include} argument must have the same factor levels.",
            i = "Variable {.val {include[1]}} has levels {.val {levels(data[[include[1]]])}}.",
            i = "Variable {.val {x}} has levels {.val {levels(data[[x]])}}.",
            i = "Use {.fun forcats::fct_unify} to unify levels."),
          call = get_cli_abort_call()
        )
      }
    }
  )

  # reverse the order of the levels if indicated -------------------------------
  if (sort == "descending") {
    data <- data |> dplyr::mutate(across(all_of(include), fct_rev))
  }

  # tabulate results -----------------------------------------------------------
  cards <-
    dplyr::bind_rows(
      cards::ard_attributes(
        data = data,
        variables = all_of(include),
        label = label
      ),
      cards::ard_missing(data, variables = all_of(include)),
      cards::ard_categorical(
        data = data,
        variables = all_of(include),
        fmt_fn = ~digits,
        denominator = "column",
        stat_label = ~ default_stat_labels()
      )
    ) |>
    cards::replace_null_statistic()

  # print all warnings and errors that occurred while calculating requested stats
  cards::print_ard_conditions(cards)

  # translate statistic labels -------------------------------------------------
  cards$stat_label <- translate_vector(cards$stat_label)

  # add the gtsummary column names to ARD data frame ---------------------------
  cards <-
    cards |>
    dplyr::left_join(
      dplyr::tibble(
        variable_level = factor(levels(data[[include[1]]]), levels = levels(data[[include[1]]])) |> as.list(),
        gts_colname = paste0("stat_", seq_along(levels(data[[include[1]]])))
      ),
      by = "variable_level"
    )

  x <-
    brdg_likert(cards = cards, variables = include, statistic =  statistic) |>
    append(
      list(
        cards = list(tbl_likert = cards),
        inputs = tbl_likert_inputs
      )
    ) |>
    structure(class = c("tbl_summary", "gtsummary"))

  # adding styling -------------------------------------------------------------
  x <- x |>
    # updating the headers for the stats columns
    modify_header(all_stat_cols() ~ "**{level}**")

  # return tbl_summary table ---------------------------------------------------
  x$call_list <- list(tbl_likert = call)

  x

}

brdg_likert <- function(cards,
                        variables,
                        statistic) {
  set_cli_abort_call()

  # create table_body
  table_body <-
    pier_likert(cards = cards, variables = variables, statistic = statistic)

  # construct default table_styling --------------------------------------------
  x <- .create_gtsummary_object(table_body)

  # add info to x$table_styling$header for dynamic headers ---------------------
  x$table_styling$header <-
    x$table_styling$header |>
    dplyr::left_join(
      cards |>
        dplyr::filter(!is.na(.data$gts_colname)) |>
        dplyr::select(column = "gts_colname", modify_stat_level = "variable_level") |>
        unique() |>
        dplyr::mutate(modify_stat_level = unlist(.data$modify_stat_level) |> as.character()),
      by = "column"
    )


  # adding styling -------------------------------------------------------------
  x <- x |>
    # add header to label column and add default indentation
    modify_table_styling(
      columns = "label",
      label = glue("**{translate_string('Characteristic')}**")
    )

  x |>
    structure(class = "gtsummary") |>
    modify_column_unhide(columns = all_stat_cols())
}

pier_likert <- function(cards, variables, statistic) {
  set_cli_abort_call()

  cards |>
    dplyr::filter(!is.na(.data$gts_colname))  |>
    cards::apply_fmt_fn() |>
    dplyr::group_by(.data$variable, .data$variable_level, .data$gts_colname) |>
    dplyr::group_map(
      function(df_variable_stats, df_groups_and_variable) {
        lst_variable_stats <-
          cards::get_ard_statistics(df_variable_stats, .column = "stat_fmt")

        df_groups_and_variable |>
          dplyr::mutate(
            stat = glue::glue(
              statistic,
              .envir =
                cards::get_ard_statistics(df_variable_stats, .column = "stat_fmt")
            ) |>
              as.character(),
            var_type = "continuous",
            row_type = "label",
            var_label = cards |>
              dplyr::filter(.data$context %in% "attributes",
                            .data$stat_name %in% "label",
                            .data$variable %in% .env$df_groups_and_variable$variable) |>
              dplyr::pull("stat") |>
              getElement(1L),
            label = .data$var_label
          )
      }
    ) |>
    dplyr::bind_rows() |>
    tidyr::pivot_wider(
      id_cols = c("variable", "row_type", "var_label", "label"),
      values_from = "stat",
      names_from = "gts_colname"
    )
}
