#' Likert Summary
#'
#' `r lifecycle::badge("experimental")`\cr
#' Create a table of ordered categorical variables in a wide format.
#'
#' @inheritParams tbl_summary
#' @param statistic ([`formula-list-selector`][syntax])\cr
#'   Used to specify the summary statistics for each variable.
#'   The default is `everything() ~ "{n} ({p}%)"`.
#' @param digits ([`formula-list-selector`][syntax])\cr
#'  Specifies how summary statistics are rounded. Values may be either integer(s)
#'   or function(s). If not specified, default formatting is assigned
#'   via `assign_summary_digits()`.
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
#'   tbl_likert(include = c(recommend_friend, regret_purchase)) |>
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
#'     type = ~"continuous",
#'     include = c(recommend_friend, regret_purchase)
#'   )
#' ) |>
#'   tbl_merge(tab_spanner = FALSE)
tbl_likert <- function(data,
                       statistic = ~"{n} ({p}%)",
                       label = NULL,
                       digits = NULL,
                       include = everything(),
                       sort = c("ascending", "descending")) {
  set_cli_abort_call()

  # process inputs -------------------------------------------------------------
  check_not_missing(data)
  check_data_frame(data)
  sort <- arg_match(sort)

  cards::process_selectors(data, include = {{ include }})

  cards::process_formula_selectors(data[include], label = label, statistic = statistic, digits = digits)
  cards::check_list_elements(
    x = label,
    predicate = \(x) is_string(x),
    error_msg = "Values pass in {.arg label} argument must be strings."
  )
  cards::check_list_elements(
    x = statistic,
    predicate = \(x) is_string(x),
    error_msg = "Values pass in {.arg statistic} argument must be strings."
  )

  # fill in unspecified variables
  cards::fill_formula_selectors(
    data[include],
    statistic = eval(formals(gtsummary::tbl_likert)[["statistic"]])
  )

  # fill each element of digits argument
  if (!missing(digits)) {
    digits <-
      assign_summary_digits(data[include], statistic, type = rep_named(include, list("categorical")), digits = digits)
  }

  .check_haven_labelled(data[include])
  .check_tbl_summary_args(
    data = data, label = label, statistic = statistic,
    digits = digits, type = rep_named(include, list("categorical")), value = NULL
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
        fmt_fn = digits,
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
        gts_column = paste0("stat_", seq_along(levels(data[[include[1]]])))
      ),
      by = "variable_level"
    )

  x <-
    brdg_likert(cards = cards, variables = include, statistic = statistic) |>
    append(
      list(
        cards = list(tbl_likert = cards),
        inputs = tbl_likert_inputs
      )
    ) |>
    structure(class = c("tbl_likert", "gtsummary"))

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

  # check the ARD has all the requested statistics -----------------------------
  walk(
    variables,
    \(variable) {
      specified_stats <- .extract_glue_elements(statistic[[variable]])
      available_stats <- dplyr::filter(cards, .data$variable %in% .env$variable, !is.na(.data$gts_column))$stat_name |> unique()

      if (is_empty(specified_stats)) {
        cli::cli_abort("The {.arg statistic} argument string does not contain any
                        glue element for variable {.val {variable}}, e.g. {.val {{n}} ({{p}}%)}.",
                       call = get_cli_abort_call())
      }
      if (any(!specified_stats %in% available_stats)) {
        not_valid_stat <- specified_stats[!specified_stats %in% available_stats]
        cli::cli_abort(c("Statistic(s) {.val {not_valid_stat}} are not valid.",
                         i = "Select one or more of {.val {available_stats}}"),
                       call = get_cli_abort_call())
      }
    }
  )

  # create table_body ----------------------------------------------------------
  table_body <-
    pier_likert(cards = cards, variables = variables, statistic = statistic)

  # construct default table_styling --------------------------------------------
  x <- .create_gtsummary_object(table_body)

  # add info to x$table_styling$header for dynamic headers ---------------------
  x$table_styling$header <-
    x$table_styling$header |>
    dplyr::left_join(
      cards |>
        dplyr::filter(!is.na(.data$gts_column)) |>
        dplyr::select(column = "gts_column", modify_stat_level = "variable_level") |>
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

  pier_summary_continuous(
    cards =
      cards |>
      dplyr::filter(!is.na(.data$gts_column) | .data$context %in% "attributes") |>
      dplyr::mutate(group1 = .data$variable, group1_level = .data$variable_level),
    variables = variables,
    statistic = statistic
  )
}
