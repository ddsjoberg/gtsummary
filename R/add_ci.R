#' Add CI Column
#'
#' Add a new column with the confidence intervals for proportions, means, etc.
#'
#' @param x (`tbl_summary`)\cr
#'   a summary table of class `'tblsummary'`
#' @param statistic ([`formula-list-selector`][syntax])\cr
#'   Indicates how the confidence interval will be displayed.
#'   Default is `list(all_continuous() ~ "{conf.low}, {conf.high}", all_categorical() ~ "{conf.low}%, {conf.high}%")`
#' @param method ([`formula-list-selector`][syntax])\cr
#'   Confidence interval method. Default is
#'   `list(all_continuous() ~ "t.test", all_categorical() ~ "wilson")`.
#'   See details below.
#' @param conf.level (scalar `real`)\cr
#'   Confidence level. Default is `0.95`
#' @param style_fun (`function`)\cr
#'   Function to style upper and lower bound of confidence interval. Default is
#'   `list(all_continuous() ~ label_style_sigfig(), all_categorical() ~ label_style_sigfig(scale =  100))`.
#' @param pattern (`string`)\cr
#'   Indicates the pattern to use to merge the CI with
#'   the statistics cell. The default is NULL, where no columns are merged.
#'   The two columns that will be merged are the statistics column,
#'   represented by `"{stat}"` and the CI column represented by `"{ci}"`,
#'   e.g. `pattern = "{stat} ({ci})"` will merge the two columns with the CI
#'   in parentheses. Default is `NULL`, and no merging is performed.
#' @inheritParams rlang::args_dots_empty
#' @inheritParams tbl_summary
#'
#' @section method argument:
#'
#' Must be one of
#' - `"wilson"`, `"wilson.no.correct"` calculated via `prop.test(correct = c(TRUE, FALSE))` for **categorical** variables
#' - `"exact"` calculated via `stats::binom.test()` for **categorical** variables
#' - `"wald"`, `"wald.no.correct"` calculated via `cardx::proportion_ci_wald(correct = c(TRUE, FALSE)` for **categorical** variables
#' - `"agresti.coull"` calculated via `cardx::proportion_ci_agresti_coull()` for **categorical** variables
#' - `"jeffreys"` calculated via `cardx::proportion_ci_jeffreys()` for **categorical** variables
#' - `"t.test"` calculated via `stats::t.test()` for **continuous** variables
#' - `"wilcox.test"` calculated via `stats::wilcox.test()` for **continuous** variables
#'
#' @return gtsummary table
#' @name add_ci
#'
#' @examplesIf gtsummary:::is_pkg_installed("cardx", reference_pkg = "gtsummary") && gtsummary:::is_pkg_installed("broom", reference_pkg = "cardx")
#' # Example 1 ----------------------------------
#' trial |>
#'   tbl_summary(
#'     missing = "no",
#'     statistic = all_continuous() ~ "{mean} ({sd})",
#'     include = c(marker, response, trt)
#'   ) |>
#'   add_ci()
#'
#' # Example 2 ----------------------------------
#' trial |>
#'   select(response, grade) %>%
#'   tbl_summary(
#'     statistic = all_categorical() ~ "{p}%",
#'     missing = "no",
#'     include = c(response, grade)
#'   ) |>
#'   add_ci(pattern = "{stat} ({ci})") |>
#'   modify_footnote(everything() ~ NA)
NULL

#' @rdname add_ci
#' @export
add_ci <- function(x, ...) {
  check_not_missing(x)
  check_class(x, "gtsummary")
  UseMethod("add_ci")
}

#' @rdname add_ci
#' @export
add_ci.tbl_summary <- function(x,
                               method = list(all_continuous() ~ "t.test", all_categorical() ~ "wilson"),
                               include = everything(),
                               statistic =
                                 list(all_continuous() ~ "{conf.low}, {conf.high}",
                                      all_categorical() ~ "{conf.low}%, {conf.high}%"),
                               conf.level = 0.95,
                               style_fun =
                                 list(all_continuous() ~ label_style_sigfig(),
                                      all_categorical() ~ label_style_sigfig(scale =  100)),
                               pattern = NULL,
                               ...) {
  set_cli_abort_call()
  check_dots_used()
  updated_call_list <- c(x$call_list, list(add_ci = match.call()))

  # check inputs ---------------------------------------------------------------
  check_scalar_range(conf.level, range = c(0, 1))
  check_string(pattern, allow_empty = TRUE)
  if (!"column" %in% x$inputs$percent) {
    cli::cli_inform("The {.fun add_ci} function is meant to work with {.code tbl_summary(percent={cli::cli_format('column')})},
                     but {.code tbl_summary(percent={cli::cli_format(x$inputs$percent)})} was used.")
  }

  # process inputs -------------------------------------------------------------
  cards::process_selectors(
    data = scope_table_body(x$table_body),
    include = {{ include }}
  )

  cards::process_formula_selectors(
    data = scope_table_body(x$table_body |> dplyr::filter(.data$variable %in% .env$include)),
    method = method,
    statistic = statistic,
    style_fun = style_fun
  )
  cards::fill_formula_selectors(
    data = scope_table_body(x$table_body |> dplyr::filter(.data$variable %in% .env$include)),
    method = eval(formals(asNamespace("gtsummary")[["add_ci.tbl_summary"]])[["method"]]),
    statistic = eval(formals(asNamespace("gtsummary")[["add_ci.tbl_summary"]])[["statistic"]]),
    style_fun = eval(formals(asNamespace("gtsummary")[["add_ci.tbl_summary"]])[["style_fun"]])
  )

  # check statistic and style_fun arg ------------------------------------------
  cards::check_list_elements(
    x = statistic,
    predicate = \(x) {
      is_string(x) && # must be a string
        !is_empty(.extract_glue_elements(x)) && # must contain at least one glue element
        is_empty(setdiff(.extract_glue_elements(x), c("conf.low", "conf.high"))) # glue elements must be conf.low and conf.high only
    },
    error_msg = c(
      "There was an error in the values passed for the {.arg statistic} argument.",
      i = "The value must be a string of length {.val {1}}.",
      i = "The value must contain glue-style elements and only {.val {{conf.low}}} and {.val {{conf.high}}} may be included."
    )
  )
  cards::check_list_elements(
    x = style_fun,
    predicate = \(x) is.function(x),
    error_msg = "The values passed in the {.arg style_fun} argument must be functions."
  )

  # check the method values match the summary types ----------------------------
  walk(
    include,
    \(variable) {
      if (x$input$type[[variable]] %in% c("continuous", "continuous2") &&
          !method[[variable]] %in% c("t.test", "wilcox.test")) {
        cli::cli_abort(
          "The value of the {.arg method} argument for continuous variable
           {.val {variable}} must be one of {.val {c('t.test', 'wilcox.test')}}",
          call = get_cli_abort_call()
        )
      }
      else if (x$input$type[[variable]] %in% c("categorical", "dichotomous") &&
               !method[[variable]] %in% c("wald", "wald.no.correct", "exact", "wilson", "wilson.no.correct", "agresti.coull", "jeffreys", "asymptotic")) {
        cli::cli_abort(
          'The value of the {.arg method} argument for categorical variable
           {.val {variable}} must be one of {.val {c("wald", "wald.no.correct", "exact", "wilson", "wilson.no.correct", "agresti.coull", "jeffreys")}}',
          call = get_cli_abort_call()
        )
      }
    }
  )

  # check if mean CI selected for summary without a mean -----------------------
  variables_with_mean_ci <- keep(method, ~.x == "t.test") |> names()
  walk(
    variables_with_mean_ci,
    function(variable) {
      if (!"mean" %in% .extract_glue_elements(x[["inputs"]][["statistic"]][[variable]])) {
        cli::cli_inform(
          "A confidence interval for the mean in variable {.val {variable}} was
           requested; however, the primary table does not contain a mean."
        )
      }
    }
  )

  # calculate ARD CIs ----------------------------------------------------------
  x$cards$add_ci <-
    .calculate_add_ci_cards_summary(data = x$inputs$data, include = include, by = x$inputs$by,
                                    method = method, style_fun = style_fun, conf.level = conf.level,
                                    overall= "add_overall" %in% names(x$call_list),
                                    value = x$inputs$value)



  # finalize styling of the table ----------------------------------------------
  brdg_add_ci(x, pattern = pattern, statistic = statistic, include = include,
              conf.level = conf.level, updated_call_list = updated_call_list)
}



brdg_add_ci <- function(x, pattern, statistic, include, conf.level, updated_call_list) {
  # check pattern argument -----------------------------------------------------
  if (!is_empty(pattern)) {
    if (!rlang::is_empty(.extract_glue_elements(pattern) |> setdiff(c("stat", "ci")))) {
      cli::cli_abort(
        "The {.arg pattern} argument allows only for elements {.val {c('stat', 'ci')}} to be included in curly brackets.",
        call = get_cli_abort_call()
      )
    }
    if (!setequal(.extract_glue_elements(pattern), c("stat", "ci"))) {
      cli::cli_abort(
        "The {.arg pattern} argument must include references to both {.val {c('{stat}', '{ci}')}}",
        call = get_cli_abort_call()
      )
    }
  }

  # evaluate glue string on the statistics -------------------------------------
  df_glued <-
    x$cards$add_ci |>
    dplyr::group_by(dplyr::pick(c(cards::all_ard_groups(), cards::all_ard_variables()))) |>
    dplyr::group_map(
      \(.x, .y) {
        .y$glued_statistic <-
          eval_tidy(
            expr = expr(glue(!!statistic[[.y$variable]])),
            data = cards::get_ard_statistics(.x, .column = "stat_fmt")
          )
        .y
      }
    ) |>
    dplyr::bind_rows()

  # format results to merge into primary table ---------------------------------
  df_prepped_for_merge <-
    df_glued |>
    dplyr::group_by(dplyr::pick(cards::all_ard_groups())) %>%
    dplyr::mutate(
      gts_colname =
        if (is_empty(x$inputs$by)) "ci_stat_0"
      else {
        ifelse(
          !is.na(.data[["group1"]]),
          paste0("ci_stat_", dplyr::cur_group_id()),
          "ci_stat_0" # this accounts for overall stats if run after `add_overall()`
        )
      }
    ) |>
    tidyr::pivot_wider(
      id_cols = cards::all_ard_variables(),
      names_from = "gts_colname",
      values_from = "glued_statistic"
    )

  # add CI stats to primary table ----------------------------------------------
  x <- x |>
    add_stat(
      fns =
        all_of(include) ~ \(variable, ...) dplyr::filter(df_prepped_for_merge, .data$variable %in% .env$variable) |> dplyr::select(matches("^ci_stat_\\d+$")),
      location = list(everything() ~ "label", all_categorical(FALSE) ~ "level")
    ) |>
    # moving the CI cols to after the original stat cols (when `by=` variable present)
    # also renaming CI columns
    modify_table_body(
      function(.x) {
        # rename ci columns
        .x <-
          .x %>%
          dplyr::rename_with(
            .fn = ~ vec_paste0(
              "ci_",
              sub(x = ., pattern = "_ci$", replacement = "")
            ),
            .cols = matches("^stat_\\d+_ci$")
          )

        # reorder the columns
        stat_cols_in_order <-
          .x |>
          select(all_stat_cols()) |>
          names() |>
          lapply(\(x) c(x, paste0("ci_", x))) |>
          unlist()

        .x |>
          dplyr::relocate(all_of(stat_cols_in_order), .after = all_of(stat_cols_in_order[1]))
      }
    )

  # if we are not merging the CIs with other cells, assign headers
  if (is.null(pattern)) {
    x <- x |>
      # updating CI column headers and footnotes
      modify_header(
        matches("^ci_stat_\\d+$") ~ paste0("**", conf.level * 100, "% ", translate_string("CI"), "**")
      ) |>
      modify_footnote(
        matches("^ci_stat_\\d+$") ~ translate_string("CI = Confidence Interval"),
        abbreviation = TRUE
      )
  }
  else {
    # get the stat column index numbers, eg get the 1 and 2 from stat_1 and stat_2
    stat_column_names <-
      x$table_body |>
      select(all_stat_cols()) |>
      names()
    chr_index <- gsub(x = stat_column_names, pattern = "^stat_", replacement = "")

    # create list of column merging expressions
    cols_merge_expr <-
      chr_index |>
      map(
        ~ expr(
          modify_table_styling(
            columns = !!glue("stat_{.x}"),
            rows = !is.na(!!sym(paste0("ci_stat_", .x))),
            cols_merge_pattern =
              !!glue::glue_data(
                .x = list(stat = paste0("{stat_", .x, "}"), ci = paste0("{ci_stat_", .x, "}")),
                pattern
              )
          )
        )
      )

    # merge columns
    x <-
      cols_merge_expr |>
      reduce(\(.x, .y) inject(!!.x %>% !!.y), .init = x) |>
      modify_footnote(
        all_stat_cols() ~ translate_string("CI = Confidence Interval"),
        abbreviation = TRUE
      )

    # updating header using `pattern=` argument
    x$table_styling$header <-
      x$table_styling$header |>
      dplyr::rowwise() %>%
      dplyr::mutate(
        label =
          ifelse(
            .data$column %in% .env$stat_column_names,
            glue::glue_data(
              .x = list(stat = .data$label, ci = paste0("**", .env$conf.level * 100, "% CI**")),
              pattern
            ),
            .data$label
          )
      ) |>
      dplyr::ungroup()
  }

  # return gtsummary table -----------------------------------------------------
  # fill in the Ns in the header table modify_stat_* columns
  x <- .fill_table_header_modify_stats(x)
  x$call_list <- updated_call_list
  x
}

.calculate_add_ci_cards_summary <- function(data, include, by, method, style_fun, conf.level, overall= FALSE, value) {
  lst_cards <-
    lapply(
      include,
      FUN = \(v) .calculate_one_ci_ard_summary(data = data, variable = v, by = by, method = method, conf.level = conf.level, value = value)
    ) |>
    set_names(include)

  # if we need to add an overall calculation if `add_overall()` has been previously run
  if (isTRUE(overall)) {
    lst_cards <-
      lapply(
        include,
        FUN = \(v) .calculate_one_ci_ard_summary(data = data, variable = v, by = NULL, method = method, conf.level = conf.level, value = value)
      ) |>
      set_names(include) |>
      append(lst_cards)
  }


  lst_cards |>
    # update the formatting function and apply the function
    imap(
      ~dplyr::mutate(
        .x,
        fmt_fn = ifelse(.data$stat_name %in% c("estimate", "conf.low", "conf.high"),
                        list(style_fun[[.y]]),
                        .data$fmt_fn)
      ) |>
        cards::apply_fmt_fn()
    ) |>
    dplyr::bind_rows() |>
    cards::tidy_ard_column_order()
}

.calculate_one_ci_ard_summary <- function(data, variable, by, method, conf.level, value) {
  switch(
    method[[variable]],
    # continuous variables
    "t.test" = cardx::ard_stats_t_test_onesample(data, variables = all_of(variable), by = any_of(by), conf.level = conf.level),
    "wilcox.test" = cardx::ard_stats_wilcox_test_onesample(data, variables = all_of(variable), by = any_of(by), conf.level = conf.level, conf.int = TRUE),

    # categorical variables
    "wald" = cardx::ard_categorical_ci(data, variables = all_of(variable), by = any_of(by), method = "waldcc", conf.level = conf.level, value = value),
    "wald.no.correct" = cardx::ard_categorical_ci(data, variables = all_of(variable), by = any_of(by), method = "wald", conf.level = conf.level, value = value),
    "exact" = cardx::ard_categorical_ci(data, variables = all_of(variable), by = any_of(by), method = "clopper-pearson", conf.level = conf.level, value = value),
    "wilson" = cardx::ard_categorical_ci(data, variables = all_of(variable), by = any_of(by), method = "wilsoncc", conf.level = conf.level, value = value),
    "wilson.no.correct" = cardx::ard_categorical_ci(data, variables = all_of(variable), by = any_of(by), method = "wilson", conf.level = conf.level, value = value),
    "agresti.coull" = cardx::ard_categorical_ci(data, variables = all_of(variable), by = any_of(by), method = "agresti-coull", conf.level = conf.level, value = value),
    "jeffreys" = cardx::ard_categorical_ci(data, variables = all_of(variable), by = any_of(by), method = "jeffreys", conf.level = conf.level, value = value),

    # Documentation for 'asymptotic' was removed in v2.0.0
    "asymptotic" = cardx::ard_categorical_ci(data, variables = all_of(variable), by = any_of(by), method = "wald", conf.level = conf.level, value = value)
  )
}
