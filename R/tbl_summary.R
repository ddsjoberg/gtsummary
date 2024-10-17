#' Summary table
#'
#' The `tbl_summary()` function calculates descriptive statistics for
#' continuous, categorical, and dichotomous variables.
#' Review the
#' \href{https://www.danieldsjoberg.com/gtsummary/articles/tbl_summary.html}{tbl_summary vignette}
#' for detailed examples.
#'
#' @param data (`data.frame`)\cr A data frame.
#' @param by ([`tidy-select`][dplyr::dplyr_tidy_select])\cr
#'   A single column from `data`. Summary statistics will be stratified by this variable.
#'   Default is `NULL`.
#' @param label ([`formula-list-selector`][syntax])\cr
#'   Used to override default labels in summary table, e.g. `list(age = "Age, years")`.
#'   The default for each variable is the column label attribute, `attr(., 'label')`.
#'   If no label has been set, the column name is used.
#' @param statistic ([`formula-list-selector`][syntax])\cr
#'   Used to specify the summary statistics for each variable.
#'   The default is
#'   `list(all_continuous() ~ "{median} ({p25}, {p75})", all_categorical() ~ "{n} ({p}%)")`.
#'   See below for details.
#' @param digits  ([`formula-list-selector`][syntax])\cr
#'   Specifies how summary statistics are rounded. Values may be either integer(s)
#'   or function(s). If not specified, default formatting is assigned
#'   via `assign_summary_digits()`. See below for details.
#' @param type ([`formula-list-selector`][syntax])\cr
#'   Specifies the summary type. Accepted value are
#'   `c("continuous", "continuous2", "categorical", "dichotomous")`.
#'   If not specified, default type is assigned via
#'   `assign_summary_type()`. See below for details.
#' @param value ([`formula-list-selector`][syntax])\cr
#'   Specifies the level of a variable to display on a single row.
#'   The gtsummary type selectors, e.g. `all_dichotomous()`, cannot be used
#'   with this argument. Default is `NULL`. See below for details.
#' @param statistic ([`formula-list-selector`][syntax])\cr
#'   Specifies summary statistics to display for each variable.  The default is
#'   `list(all_continuous() ~ "{median} ({p25}, {p75})", all_categorical() ~ "{n} ({p}%)")`.
#'   See below for details.
#' @param missing,missing_text,missing_stat
#'   Arguments dictating how and if missing values are presented:
#'   - `missing`: must be one of `c("ifany", "no", "always")`
#'   - `missing_text`: string indicating text shown on missing row. Default is `"Unknown"`
#'   - `missing_stat`: statistic to show on missing row. Default is `"{N_miss}"`.
#'     Possible values are `N_miss`, `N_obs`, `N_nonmiss`, `p_miss`, `p_nonmiss`.
#' @param sort ([`formula-list-selector`][syntax])\cr
#'   Specifies sorting to perform for categorical variables.
#'   Values must be one of `c("alphanumeric", "frequency")`.
#'   Default is `all_categorical(FALSE) ~ "alphanumeric"`.
#' @param percent (`string`)\cr
#'   Indicates the type of percentage to return.
#'   Must be one of `c("column", "row", "cell")`. Default is `"column"`.
#' @param include ([`tidy-select`][dplyr::dplyr_tidy_select])\cr
#'   Variables to include in the summary table. Default is `everything()`.
#'
#' @return a gtsummary table of class `"tbl_summary"`
#' @export
#'
#' @section statistic argument:
#' The statistic argument specifies the statistics presented in the table. The
#' input dictates the summary statistics presented in the table. For example,
#' `statistic = list(age ~ "{mean} ({sd})")` would report the mean and
#' standard deviation for age; `statistic = list(all_continuous() ~ "{mean} ({sd})")`
#' would report the mean and standard deviation for all continuous variables.
#'
#' The values are interpreted using  [`glue::glue()`] syntax:
#' a name that appears between curly brackets will be interpreted as a function
#' name and the formatted result of that function will be placed in the table.
#'
#' For categorical variables, the following statistics are available to display:
#' `{n}` (frequency), `{N}` (denominator), `{p}` (percent).
#'
#' For continuous variables, **any univariate function may be used**.
#' The most commonly used functions are `{median}`, `{mean}`, `{sd}`, `{min}`,
#' and `{max}`.
#' Additionally, `{p##}` is available for percentiles, where `##` is an integer from 0 to 100.
#' For example, `p25: quantile(probs=0.25, type=2)`.
#'
#' When the summary type is `"continuous2"`, pass a vector of statistics.
#' Each element of the vector will result in a separate row in the summary table.
#'
#' For both categorical and continuous variables, statistics on the number of
#' missing and non-missing observations and their proportions are available to
#' display.
#' \itemize{
#'   \item `{N_obs}` total number of observations
#'   \item `{N_miss}` number of missing observations
#'   \item `{N_nonmiss}` number of non-missing observations
#'   \item `{p_miss}` percentage of observations missing
#'   \item `{p_nonmiss}` percentage of observations not missing
#' }
#'
#' @section digits argument:
#' The digits argument specifies the the number of digits (or formatting function)
#' statistics are rounded to.
#'
#' The values passed can either be a single integer, a vector of integers, a
#' function, or a list of functions. If a single integer or function is passed,
#' it is recycled to the length of the number of statistics presented.
#' For example, if the statistic is `"{mean} ({sd})"`, it is equivalent to
#' pass `1`, `c(1, 1)`, `label_style_number(digits=1)`, and
#' `list(label_style_number(digits=1), label_style_number(digits=1))`.
#'
#' Named lists are also accepted to change the default formatting for a single
#' statistic, e.g. `list(sd = label_style_number(digits=1))`.
#'
#' @section type and value arguments:
#' There are four summary types. Use the `type` argument to change the default summary types.
#'    - `"continuous"` summaries are shown on a *single row*. Most numeric
#'    variables default to summary type continuous.
#'    - `"continuous2"` summaries are shown on *2 or more rows*
#'    - `"categorical"` *multi-line* summaries of nominal data. Character variables,
#'    factor variables, and numeric variables with fewer than 10 unique levels default to
#'    type categorical. To change a numeric variable to continuous that
#'    defaulted to categorical, use `type = list(varname ~ "continuous")`
#'    - `"dichotomous"` categorical variables that are displayed on a *single row*,
#'    rather than one row per level of the variable.
#'    Variables coded as `TRUE`/`FALSE`, `0`/`1`, or `yes`/`no` are assumed to be dichotomous,
#'    and the `TRUE`, `1`, and `yes` rows are displayed.
#'    Otherwise, the value to display must be specified in the `value`
#'    argument, e.g. `value = list(varname ~ "level to show")`
#'
#' @export
#' @return A table of class `c('tbl_summary', 'gtsummary')`
#'
#' @family tbl_summary tools
#' @seealso See \href{https://www.danieldsjoberg.com/gtsummary/articles/tbl_summary.html}{tbl_summary vignette} for detailed tutorial
#' @seealso See \href{https://www.danieldsjoberg.com/gtsummary/articles/gallery.html}{table gallery} for additional examples
#' @seealso Review [list, formula, and selector syntax][syntax] used throughout gtsummary
#' @author Daniel D. Sjoberg
#' @examplesIf identical(Sys.getenv("NOT_CRAN"), "true") || identical(Sys.getenv("IN_PKGDOWN"), "true")
#' # Example 1 ----------------------------------
#' trial |>
#'   select(age, grade, response) |>
#'   tbl_summary()
#'
#' # Example 2 ----------------------------------
#' trial |>
#'   select(age, grade, response, trt) |>
#'   tbl_summary(
#'     by = trt,
#'     label = list(age = "Patient Age"),
#'     statistic = list(all_continuous() ~ "{mean} ({sd})"),
#'     digits = list(age = c(0, 1))
#'   )
#'
#' # Example 3 ----------------------------------
#' trial |>
#'   select(age, marker) |>
#'   tbl_summary(
#'     type = all_continuous() ~ "continuous2",
#'     statistic = all_continuous() ~ c("{median} ({p25}, {p75})", "{min}, {max}"),
#'     missing = "no"
#'   )
tbl_summary <- function(data,
                        by = NULL,
                        label = NULL,
                        statistic = list(
                          all_continuous() ~ "{median} ({p25}, {p75})",
                          all_categorical() ~ "{n} ({p}%)"
                        ),
                        digits = NULL,
                        type = NULL,
                        value = NULL,
                        missing = c("ifany", "no", "always"),
                        missing_text = "Unknown",
                        missing_stat = "{N_miss}",
                        sort = all_categorical(FALSE) ~ "alphanumeric",
                        percent = c("column", "row", "cell"),
                        include = everything()) {
  set_cli_abort_call()

  # data argument checks -------------------------------------------------------
  check_not_missing(data)
  check_data_frame(data)
  .data_dim_checks(data)

  # process arguments ----------------------------------------------------------
  cards::process_selectors(data, by = {{ by }}, include = {{ include }})
  check_scalar(
    by,
    allow_empty = TRUE,
    message = c("The {.arg {arg_name}} argument must be length {.val {1}} or empty.",
                i = "Use {.fun tbl_strata} for more than one {.arg by} variable."
    )
  )
  data <- dplyr::ungroup(data) |> .drop_missing_by_obs(by = by) # styler: off
  include <- setdiff(include, by) # remove by variable from list vars included


  if (missing(missing)) {
    missing <- get_theme_element("tbl_summary-arg:missing", default = missing) # styler: off
  }

  missing <- arg_match(missing, values = c("ifany", "no", "always"))

  if (missing(missing_text)) {
    missing_text <- get_theme_element("tbl_summary-arg:missing_text", default = translate_string(missing_text)) # styler: off
  }
  check_string(missing_text)


  if (missing(percent))
    percent <- get_theme_element("tbl_summary-arg:percent", default = percent) # styler: off
  percent <- arg_match(percent, values = c("column", "row", "cell"))

  cards::process_formula_selectors(
    data = data[include],
    value =
      case_switch(
        missing(value) ~ get_theme_element("tbl_summary-arg:value", default = value),
        .default = value
      )
  )


  # assign summary type --------------------------------------------------------
  if (!is_empty(type)) {
    # first set default types, so selectors like `all_continuous()` can be used
    # to recast the summary type, e.g. make all continuous type "continuous2"
    default_types <- assign_summary_type(data, include, value)
    # process the user-passed type argument
    cards::process_formula_selectors(
      data = scope_table_body(.list2tb(default_types, "var_type"), data[include]),
      type =
        case_switch(
          missing(type) ~ get_theme_element("tbl_summary-arg:type", default = type),
          .default = type
        )
    )
    # fill in any types not specified by user
    type <- utils::modifyList(default_types, type)
  } else {
    type <- assign_summary_type(data, include, value)
  }

  value <-
    scope_table_body(.list2tb(type, "var_type"), data[include]) |>
    .assign_default_values(value, type)

  # evaluate the remaining list-formula arguments ------------------------------
  # processed arguments are saved into this env
  cards::process_formula_selectors(
    data = scope_table_body(.list2tb(type, "var_type"), data[include]),
    statistic =
      case_switch(
        missing(statistic) ~ get_theme_element("tbl_summary-arg:statistic", default = statistic),
        .default = statistic
      ),
    include_env = TRUE
  )

  # add the calling env to the statistics
  statistic <- .add_env_to_list_elements(statistic, env = caller_env())

  cards::process_formula_selectors(
    scope_table_body(.list2tb(type, "var_type"), data[include]),
    label =
      case_switch(
        missing(label) ~ get_deprecated_theme_element("tbl_summary-arg:label", default = label),
        .default = label
      ),
    sort =
      case_switch(
        missing(sort) ~ get_theme_element("tbl_summary-arg:sort", default = sort),
        .default = sort
      )
  )

  cards::process_formula_selectors(
    scope_table_body(.list2tb(type, "var_type"), data[include]),
    digits =
      case_switch(
        missing(digits) ~ get_theme_element("tbl_summary-arg:digits", default = digits),
        .default = digits
      )
  )

  # fill in unspecified variables
  cards::fill_formula_selectors(
    scope_table_body(.list2tb(type, "var_type"), data[include]),
    statistic =
      get_theme_element("tbl_summary-arg:statistic", default = eval(formals(gtsummary::tbl_summary)[["statistic"]])),
    sort =
      get_theme_element("tbl_summary-arg:sort", default = eval(formals(gtsummary::tbl_summary)[["sort"]])),
    digits =
      get_theme_element("tbl_summary-arg:digits", default = eval(formals(gtsummary::tbl_summary)[["digits"]]))
  )

  # fill each element of digits argument
  if (!missing(digits)) {
    digits <-
      scope_table_body(.list2tb(type, "var_type"), data[include]) |>
      assign_summary_digits(statistic, type, digits = digits)
  }

  # check inputs ---------------------------------------------------------------
  check_string(missing_stat)
  check_string(missing_text)
  .check_haven_labelled(data[c(include, by)])
  .check_tbl_summary_args(
    data = data, label = label, statistic = statistic,
    digits = digits, type = type, value = value, sort = sort
  )
  .check_statistic_type_agreement(statistic, type)

  # sort requested columns by frequency
  data <- .sort_data_infreq(data, sort)

  # save processed function inputs ---------------------------------------------
  tbl_summary_inputs <-
    as.list(environment()) |>
    utils::modifyList(list(default_types = NULL))
  call <- match.call()


  # construct cards ------------------------------------------------------------
  cards <-
    cards::bind_ard(
      # tabulate categorical summaries
      cards::ard_categorical(
        scope_table_body(.list2tb(type, "var_type"), data),
        by = all_of(by),
        variables = all_categorical(FALSE),
        fmt_fn = digits,
        denominator = percent,
        stat_label = ~ default_stat_labels()
      ),
      # tabulate dichotomous summaries
      cards::ard_dichotomous(
        scope_table_body(.list2tb(type, "var_type"), data),
        by = all_of(by),
        variables = all_dichotomous(),
        fmt_fn = digits,
        denominator = percent,
        value = value,
        stat_label = ~ default_stat_labels()
      ),
      # calculate continuous summaries
      cards::ard_continuous(
        scope_table_body(.list2tb(type, "var_type"), data),
        by = all_of(by),
        variables = all_continuous(),
        statistic =
          .continuous_statistics_chr_to_fun(
            statistic[select(scope_table_body(.list2tb(type, "var_type"), data), all_continuous()) |> names()]
          ),
        fmt_fn = digits,
        stat_label = ~ default_stat_labels()
      ),
      cards::ard_attributes(data, variables = all_of(c(include, by)), label = label),
      cards::ard_missing(data,
                         variables = all_of(include),
                         by = all_of(by),
                         fmt_fn = digits,
                         stat_label = ~ default_stat_labels()
      ),
      # adding total N
      cards::ard_total_n(data),
      # tabulate by variable for header stats
      case_switch(
        !is_empty(by) ~
          cards::ard_categorical(
            data,
            variables = all_of(by),
            stat_label = ~ default_stat_labels()
          ),
        .default = NULL
      )
    ) %>%
    structure(., class = c("card", class(.))) |>
    cards::replace_null_statistic()

  # print all warnings and errors that occurred while calculating requested stats
  cards::print_ard_conditions(cards)

  # check the requested stats are present in ARD data frame
  .check_stats_available(cards = cards, statistic = statistic)

  # translate statistic labels -------------------------------------------------
  cards$stat_label <- translate_vector(cards$stat_label)

  # add the gtsummary column names to ARD data frame ---------------------------
  cards <- .add_gts_column_to_cards_summary(cards, include, by)

  # construct initial tbl_summary object ---------------------------------------
  x <-
    brdg_summary(
      cards = cards,
      by = by,
      variables = include,
      statistic = statistic,
      type = type,
      missing = missing,
      missing_stat = missing_stat,
      missing_text = missing_text
    ) |>
    append(
      list(
        cards = list(tbl_summary = cards),
        inputs = tbl_summary_inputs
      )
    ) |>
    structure(class = c("tbl_summary", "gtsummary"))

  # adding styling -------------------------------------------------------------
  x <- x |>
    # updating the headers for the stats columns
    modify_header(
      all_stat_cols() ~
        ifelse(
          is_empty(by),
          get_theme_element("tbl_summary-str:header-noby",
                            default = "**N = {style_number(N)}**"),
          get_theme_element("tbl_summary-str:header-withby",
                            default = "**{level}**  \nN = {style_number(n)}")
        )
    )

  # return tbl_summary table ---------------------------------------------------
  x$call_list <- list(tbl_summary = call)
  # running any additional mods
  x <-
    get_theme_element("tbl_summary-fn:addnl-fn-to-run", default = identity) |>
    do.call(list(x))

  x
}

.add_gts_column_to_cards_summary <- function(cards, variables, by) {
  if ("gts_column" %in% names(cards)) {
    cli::cli_inform("The {.val gts_column} column is alread present. Defining the column has been skipped.")
    return(cards)
  }

  # adding the name of the column the stats will populate
  if (is_empty(by)) {
    cards$gts_column <-
      ifelse(
        !cards$context %in% "attributes" & !cards$variable %in% "..ard_total_n..",
        "stat_0",
        NA_character_
      )
  } else {
    # styler: off
    cards <-
      cards %>%
      {dplyr::left_join(
        .,
        dplyr::filter(
          .,
          .data$variable %in% .env$variables,
          !cards$context %in% "attributes",
        ) |>
          dplyr::select(cards::all_ard_groups(), "variable", "context") |>
          dplyr::distinct() %>%
          {.[order(unlist(.$group1_level)), ]} |> # styler: off
          dplyr::mutate(
            .by = cards::all_ard_groups(),
            gts_column = paste0("stat_", dplyr::cur_group_id())
          ),
        by = names(dplyr::select(., cards::all_ard_groups(), "variable", "context"))
      )}
    #styler: on
  }

  cards
}

.drop_missing_by_obs <- function(data, by) {
  if (is_empty(by) || !any(is.na(data[[by]]))) {
    return(data)
  }

  obs_to_drop <- is.na(data[[by]])
  cli::cli_inform("{.val {sum(obs_to_drop)}} missing rows in the {.val {by}} column have been removed.")
  dplyr::filter(data, !obs_to_drop)
}

.check_stats_available <- function(cards, statistic) {
  statistic |>
    imap(
      function(pre_glue_stat, variable) {
        extracted_stats <- .extract_glue_elements(pre_glue_stat)
        available_stats <-
          cards |>
          dplyr::filter(.data$variable %in% .env$variable, !.data$context %in% "attributes") |>
          dplyr::pull("stat_name")

        missing_stats <- extracted_stats[!extracted_stats %in% available_stats]
        if (!is_empty(missing_stats)) {
          cli::cli_abort(
            c("Statistic {.val {missing_stats}} is not available for variable {.val {variable}}.",
              i = "Select among {.val {unique(available_stats)}}."
            ),
            call = get_cli_abort_call()
          )
        }
      }
    )
}

.add_env_to_list_elements <- function(x, env) {
  lapply(
    x,
    function(.x) {
      attr(.x, ".Environment") <- attr(.x, ".Environment") %||% env
      .x
    }
  )
}

.sort_data_infreq <- function(data, sort) {
  # if no frequency sorts requested, just return data frame
  if (every(sort, function(x) x %in% "alphanumeric")) {
    return(data)
  }

  for (i in seq_along(sort[intersect(names(sort), names(data))])) {
    if (sort[[i]] %in% "frequency") {
      data[[names(sort)[i]]] <- fct_infreq(data[[names(sort)[i]]])
    }
  }

  data
}

.construct_summary_footnote <- function(card, include, statistic, type) {
  include |>
    lapply(
      function(variable) {
        if (type[[variable]] %in% "continuous2") {
          return(NULL)
        }
        card |>
          dplyr::filter(.data$variable %in% .env$variable) |>
          dplyr::select("stat_name", "stat_label") |>
          dplyr::distinct() %>%
          {stats::setNames(as.list(.$stat_label), .$stat_name)} |> # styler: off
          glue::glue_data(
            gsub("\\{(p|p_miss|p_nonmiss|p_unweighted)\\}%", "{\\1}", x = statistic[[variable]])
          )
      }
    ) |>
    stats::setNames(include) |>
    compact() |>
    unlist() |>
    unique() %>%
    {switch(!is.null(.), paste(., collapse = "; "))} # styler: off
}



.get_variables_by_type <- function(x, type) {
  names(x)[unlist(x) %in% type]
}

.assign_default_values <- function(data, value, type) {
  lapply(
    names(data),
    function(variable) {
      # if user passed value, then use it
      if (!is.null(value[[variable]])) {
        return(value[[variable]])
      }
      # if not a dichotomous summary type, then return NULL
      if (!type[[variable]] %in% "dichotomous") {
        return(NULL)
      }

      # otherwise, return default value
      default_value <- .get_default_dichotomous_value(data[[variable]])
      if (!is.null(default_value)) {
        return(default_value)
      }
      cli::cli_abort(c(
        "Error in argument {.arg value} for variable {.val {variable}}.",
        "i" = "Summary type is {.val dichotomous} but no summary value has been assigned."
      ))
    }
  ) |>
    stats::setNames(names(data))
}


.data_dim_checks <- function(data) {
  # cannot be empty data frame
  if (nrow(data) == 0L || ncol(data) == 0L) {
    cli::cli_abort(
      "Expecting {.arg data} argument to have at least 1 row and 1 column.",
      call = get_cli_abort_call()
    )
  }
  invisible()
}

.check_haven_labelled <- function(data) {
  if (some(data, ~ inherits(., "haven_labelled"))) {
    # list of columns with haven_labelled
    haven_labelled_vars <-
      map_lgl(data, ~ inherits(.x, "haven_labelled")) %>%
      keep(identity) %>%
      names()

    cnvt_funs <-
      c("haven::as_factor", "labelled::to_factor", "labelled::unlabelled", "unclass")

    hyperlinks <- c(
      "https://haven.tidyverse.org/articles/semantics.html",
      "https://larmarange.github.io/labelled/articles/intro_labelled.html#unlabelled"
    )

    c(
      "!" = "Column(s) {.val {haven_labelled_vars}} are class {.val haven_labelled}.",
      "i" = "This is an intermediate datastructure not meant for analysis.",
      "i" = "Convert columns with {.fun {cnvt_funs}}. Failure to convert may have unintended consequences or result in error.",
      paste0("{.url ", hyperlinks, "}")
    ) |>
      cli::cli_inform()
  }

  invisible()
}

.continuous_statistics_chr_to_fun <- function(statistics) {
  # vector of all the categorical summary function names
  # these are defined internally, and we don't need to convert them to true fns
  chr_protected_cat_names <- .categorical_summary_functions() |>
    names() |>
    c("N")

  # this chunk converts the character strings that define the statistics
  # into named lists of functions,
  # e.g. list(age = "{mean}") becomes list(age = list(mean = function(x) mean(x)))
  lst_stat_fns_chr <-
    lapply(
      statistics,
      function(x) {
        chr_fun_names <-
          .extract_glue_elements(x) |>
          unlist() |>
          setdiff(chr_protected_cat_names)
        lgl_fun_is_quantile <- grepl(x = chr_fun_names, pattern = "^p\\d{1,3}$")

        map2(
          chr_fun_names, lgl_fun_is_quantile,
          function(chr_fun_name, is_quantile) {
            # if it's a quantile function, return the quantile function
            if (is_quantile) {
              return(.str_to_quantile_fun(chr_fun_name))
            }
            # otherwise, convert the string to an expr and evaluate
            tryCatch(
              eval(parse_expr(chr_fun_name), envir = attr(x, ".Environment") %||% current_env()),
              error = function(e) {
                cli::cli_abort(c(
                  "Problem with the {.arg statistic} argument.",
                  "Error converting string {.val {chr_fun_name}} to a function.",
                  i = "Is the name spelled correctly and available?"
                ), call = get_cli_abort_call())
              }
            )
          }
        ) |>
          set_names(chr_fun_names)
      }
    )

  lst_stat_fns_chr
}

.str_to_quantile_fun <- function(str) {
  function(x) stats::quantile(x, probs = as.numeric(substr(str, 2, nchar(str))) / 100, type = 2) |> unname()
}


.check_tbl_summary_args <- function(data, label, statistic, digits, type, value, sort = NULL) {
  # first check the structure of each of the inputs ----------------------------
  type_accepted <- c("continuous", "continuous2", "categorical", "dichotomous")

  cards::check_list_elements(
    x = label,
    predicate = function(x) is_string(x),
    error_msg = "Error in argument {.arg {arg_name}} for column {.val {variable}}: value must be a string."
  )

  cards::check_list_elements(
    x = statistic,
    predicate = function(x) is.character(x),
    error_msg = "Error in argument {.arg {arg_name}} for column {.val {variable}}: value must be a character vector."
  )

  cards::check_list_elements(
    x = type,
    predicate = function(x) is_string(x) && x %in% c("continuous", "continuous2", "categorical", "dichotomous"),
    error_msg =
      "Error in argument {.arg {arg_name}} for column {.val {variable}}: value must be one of {.val {c('continuous', 'continuous2', 'categorical', 'dichotomous')}}."
  )

  cards::check_list_elements(
    x = value,
    predicate = function(x) is.null(x) || length(x) == 1L,
    error_msg = "Error in argument {.arg {arg_name}} for column {.val {variable}}: value must be either {.val {NULL}} or a scalar."
  )

  cards::check_list_elements(
    x = sort,
    predicate = function(x) is.null(x) || (is_string(x) && x %in% c("alphanumeric", "frequency")),
    error_msg = "Error in argument {.arg {arg_name}} for column {.val {variable}}: value must be one of {.val {c('alphanumeric', 'frequency')}}."
  )

}

.check_statistic_type_agreement <- function(statistic, type) {
  # statistic must be a string for types "continuous", "categorical", "dichotomous" and character for "continuous2"
  imap(
    statistic,
    \(stat, variable) {
      if (type[[variable]] %in% c("continuous", "categorical", "dichotomous") && !is_string(stat)) {
        msg <- "The {.arg statistic} argument value for variable {.val {variable}} must be a string, but is {.obj_type_friendly {stat}}."
        if (type[[variable]] == "continuous" && is.character(stat)) {
          msg <- c(msg, i = "Did you mean to set {.code type = list({variable} = {.val continuous2})} for a multi-line summary?")
        }
        cli::cli_abort(msg, call = get_cli_abort_call())
      }
      else if (type[[variable]] %in% "continuous2" && !is.character(stat)) {
        cli::cli_abort(
          "The {.arg statistic} argument value for variable {.val {variable}} must be {.cls character}, but is {.obj_type_friendly {stat}}.",
          call = get_cli_abort_call()
        )
      }
    }
  )
}
