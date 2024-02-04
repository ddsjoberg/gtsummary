#' Summary statistics table
#'
#' The `tbl_summary()` function calculates descriptive statistics for
#' continuous, categorical, and dichotomous variables.
#' Review the
#' \href{https://www.danieldsjoberg.com/gtsummary/articles/tbl_summary.html}{tbl_summary vignette}
#' for detailed examples.
#'
#' @param data (`data.frame`)\cr A data frame
#' @param by ([`tidy-select`][dplyr::dplyr_tidy_select])\cr
#'   A single column from `data`. Summary statistics will be stratified by this variable.
#'   Default is `NULL`
#' @param label ([`formula-list-selector`][syntax])\cr
#'   Used to override default labels in summary table, e.g. `list(age = "Age, years")`.
#'   The default for each variable is the column label attribute, `attr(., 'label')`.
#'   If no label has been set, the column name is used.
#' @param statistic ([`formula-list-selector`][syntax])\cr
#'   Used to specify the summary statistics for each variable.
#'   The default is
#'   `list(all_continuous() ~ "{median} ({p25}, {p75})", all_categorical() ~ "{n} ({p}%)")`.
#'   See below for details.
#' @param digits TODO:
#' @param type TODO:
#' @param value TODO:
#' @param missing TODO:
#' @param missing_text TODO:
#' @param missing_stat TODO:
#' @param sort TODO:
#' @param percent TODO:
#' @param include TODO:
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
#' For categorical variables, the following statistics are available to display.
#' \itemize{
#'   \item `{n}` frequency
#'   \item `{N}` denominator, or cohort size
#'   \item `{p}` formatted percentage
#' }
#'
#' For continuous variables, **any univariate function may be used**. Below is a list
#' of the _most commonly_ used statistics.
#' \itemize{
#'   \item `{median}` median
#'   \item `{mean}` mean
#'   \item `{sd}` standard deviation
#'   \item `{var}` variance
#'   \item `{min}` minimum
#'   \item `{max}` maximum
#'   \item `{sum}` sum
#'   \item `{p##}` any integer percentile, where `##` is an integer from 0 to 100
#' }
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
#' @export
#' @return A table of class `c('tbl_summary', 'gtsummary')`
#'
#' @family tbl_summary tools
#' @seealso See \href{https://www.danieldsjoberg.com/gtsummary/articles/tbl_summary.html}{tbl_summary vignette} for detailed tutorial
#' @seealso See \href{https://www.danieldsjoberg.com/gtsummary/articles/gallery.html}{table gallery} for additional examples
#' @seealso Review [list, formula, and selector syntax][syntax] used throughout gtsummary
#' @author Daniel D. Sjoberg
#' @examples
#' # Example 1 ----------------------------------
#' tbl_summary_ex1 <-
#'   trial |>
#'   select(age, grade, response) |>
#'   tbl_summary()
#'
#' # Example 2 ----------------------------------
#' tbl_summary_ex2 <-
#'   trial |>
#'   select(age, grade, response, trt) |>
#'   tbl_summary(
#'     by = trt,
#'     label = list(age = "Patient Age"),
#'     statistic = list(all_continuous() ~ "{mean} ({sd})"),
#'     digits = list(age ~ c(0, 1))
#'   )
#'
#' # Example 3 ----------------------------------
#' tbl_summary_ex3 <-
#'   trial |>
#'   select(age, marker) |>
#'   tbl_summary(
#'     type = all_continuous() ~ "continuous2",
#'     statistic = all_continuous() ~ c("{median} ({p25}, {p75})", "{min}, {max}"),
#'     missing = "no"
#'   )
tbl_summary <- function(data,
                        by = NULL,
                        label = NULL,
                        statistic = list(all_continuous() ~ "{mean} ({sd})",
                                         all_categorical() ~ "{n} ({p}%)"),
                        digits = assign_summary_digits(data, statistic, type),
                        type = assign_summary_type(data, include, value),
                        value = NULL,
                        missing = c("ifany", "no", "always"),
                        missing_text = "Unknown",
                        missing_stat = "{N_miss}",
                        sort = all_categorical(FALSE) ~ "alphanumeric",
                        percent = c("column", "row", "cell"),
                        include = everything()) {
  # data argument checks -------------------------------------------------------
  check_not_missing(data)
  check_class_data_frame(data)
  .data_dim_checks(data)

  # process arguments ----------------------------------------------------------
  data <- dplyr::ungroup(data)
  cards::process_selectors(data, by = {{ by }}, include = {{ include }})
  include <- setdiff(include, by) # remove by variable from list vars included
  missing <- arg_match(arg = missing)
  percent <- arg_match(arg = percent)
  cards::process_formula_selectors(data = data[include], value = value)

  # assign summary type --------------------------------------------------------
  if (!missing(type)) {
    # first set default types, so selectors like `all_continuous()` can be used
    # to recast the summary type, e.g. make all continuous type "continuous2"
    default_types <- formals(gtsummary::tbl_summary)[["type"]] |> eval()
    data <- .add_summary_type_as_attr(data, default_types)
    # process the user-passed type argument
    cards::process_formula_selectors(data = data[include], type = type)
    # fill in any types not specified by user
    type <- utils::modifyList(default_types, type)
  }
  data <- .add_summary_type_as_attr(data, type)

  value <- .assign_default_values(data[include], value, type)

  # evaluate the remaining list-formula arguments ------------------------------
  # processed arguments are saved into this env
  cards::process_formula_selectors(
    data = data[include],
    label = label,
    statistic =
      .ifelse1(
        missing(statistic),
        get_theme_element("TODO:fill-this-in", default = statistic),
        statistic
      ),
    sort =
      .ifelse1(
        missing(sort),
        get_theme_element("TODO:fill-this-in", default = sort),
        sort
      )
  )

  cards::process_formula_selectors(
    data = data[include],
    digits =
      .ifelse1(
        missing(digits),
        get_theme_element("TODO:fill-this-in", default = digits),
        digits
      )
  )

  # fill in unspecified variables
  cards::fill_formula_selectors(
    data[include],
    statistic =
      get_theme_element("TODO:fill-this-in", default = eval(formals(gtsummary::tbl_summary)[["statistic"]])),
    sort =
      get_theme_element("TODO:fill-this-in", default = eval(formals(gtsummary::tbl_summary)[["sort"]])),
    digits =
      get_theme_element("TODO:fill-this-in", default = eval(formals(gtsummary::tbl_summary)[["digits"]]))
  )

  # fill each element of digits argument
  # TODO: this needs to be updated to account for the scenario where there is a template override that may not fill in all the values
  if (!missing(digits)) {
    digits <- assign_summary_digits(data[include], statistic, type, digits = digits)
  }

  # check inputs ---------------------------------------------------------------
  check_class(missing_text, class = "character", length = 1L)
  check_class(missing_stat, class = "character", length = 1L)
  .check_haven_labelled(data[c(include, by)])
  .check_tbl_summary_args(
    data = data, label = label, statistic = statistic,
    digits = digits, type = type, value = value, sort = sort
  )

  # sort requested columns by frequency
  # TODO: Does this break the type attribute? If so, need to re-apply
  data <- .sort_data_infreq(data, sort)

  # save processed function inputs ---------------------------------------------
  tbl_summary_inputs <- as.list(environment())
  call <- match.call()


  # construct cards ------------------------------------------------------------
  cards <-
    cards::bind_ard(
      cards::ard_attributes(data, variables = all_of(c(include, by)), label = label),
      cards::ard_missing(data, variables = all_of(include), by = all_of(by)),
      # tabulate by variable for header stats
      if (!rlang::is_empty(by)) cards::ard_categorical(data, variables = all_of(by)),
      # tabulate categorical summaries
      cards::ard_categorical(
        data,
        by = all_of(by),
        variables = all_categorical(),
        fmt_fn = digits,
        denominator = percent
      ),
      # calculate categorical summaries
      cards::ard_continuous(
        data,
        by = all_of(by),
        variables = all_continuous(),
        fmt_fn = digits
      )
    )

  # construct initial tbl_summary object ---------------------------------------
  x <-
    list(
      cards = cards,
      inputs = tbl_summary_inputs,
      call_list = list(tbl_summary = call)
    ) |>
    brdg_summary() |>
    structure(class = c("tbl_summary", "gtsummary"))

  # adding styling -------------------------------------------------------------
  x <-
    modify_table_styling(
      x,
      columns = "label",
      label = "**Characteristic**",
      rows = .data$row_type %in% c("level", "missing"),
      indentation = 4L
    ) |>
    modify_table_styling(
      columns = all_stat_cols(),
      footnote =
        .construct_summary_footnote(x$cards, x$inputs$include, x$inputs$statistic, x$inputs$type)
    )

  x <-
    modify_header(
      x,
      all_stat_cols() ~ ifelse(is_empty(by), "**N = {N}**", "**{level}**  \nN = {n}")
    )

  # return object
  x
}

.sort_data_infreq <- function(data, sort) {
  # if no frequency sorts requested, just return data frame
  if (every(sort, function(x) x %in% "alphanumeric")) return(data)

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
        if (type[[variable]] %in% "continuous2") return(NULL)
        card |>
          dplyr::filter(.data$variable %in% .env$variable) |>
          dplyr::select("stat_name", "stat_label") |>
          dplyr::distinct() %>%
          {as.list(.$stat_label) |> stats::setNames(.$stat_name)}|>
          glue::glue_data(statistic[[variable]]) %>%
          {gsub(pattern = "(%%)+", replacement = "%", x = .)}
      }
    ) |>
    stats::setNames(include) |>
    compact() |>
    unlist() |>
    unique() %>%
    {switch(!is.null(.), paste(collapse = ", "))}
}



.get_variables_by_type <- function(x, type) {
  names(x)[unlist(x) %in% type]
}

.assign_default_values <- function(data, value, type) {
  lapply(
    names(data),
    function(variable) {
      # if user passed value, then use it
      if (!is.null(value[[variable]])) return(value[[variable]])
      # if not a dichotomous summary type, then return NULL
      if (!type[[variable]] %in% "dichotomous") return(NULL)

      # otherwise, return default value
      default_value <- .get_default_dichotomous_value(data[[variable]])
      if (!is.null(default_value)) return(default_value)
      cli::cli_abort(c(
        "Error in argument {.arg value} for variable {.val {variable}}.",
        "i" = "Summary type is {.val dichotomous} but no summary value has been assigned."
      ))
    }
  ) |>
    stats::setNames(names(data))
}


.data_dim_checks <- function(data, call = rlang::caller_env()) {
  # cannot be empty data frame
  if (nrow(data) == 0L || ncol(data) == 0L) {
    cli::cli_abort("Expecting {.arg data} argument to have at least 1 row and 1 column.", call = call)
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


.check_tbl_summary_args <- function(data, label, statistic, digits, type, value, sort, env = parent.frame()) {
  # first check the structure of each of the inputs ----------------------------
  type_accepted <- c("continuous", "continuous2", "categorical", "dichotomous")
  sort_accepted <- c("alphanumeric", "frequency")

  cards::check_list_elements(
    x = label,
    predicate = function(x) is_string(x),
    error_msg = "Error in argument {arg_name} for column {variable}: value must be a string.",
    env = env
  )

  cards::check_list_elements(
    x = statistic,
    predicate = function(x) is.character(x),
    error_msg = "Error in argument {arg_name} for column {variable}: value must be a character vector.",
    env = env
  )

  cards::check_list_elements(
    x = type,
    predicate = function(x) is_string(x) && x %in% type_accepted,
    error_msg = "Error in argument {arg_name} for column {variable}: value must be one of {.val {type_accepted}}.",
    env = env
  )

  cards::check_list_elements(
    x = value,
    predicate = function(x) is.null(x) || length(x) == 1L,
    error_msg = "Error in argument {arg_name} for column {variable}: value must be either {.val {NULL}} or a scalar.",
    env = env
  )

  cards::check_list_elements(
    x = sort,
    predicate = function(x) is.null(x) || (is_string(x)  && x %in% sort_accepted),
    error_msg = "Error in argument {arg_name} for column {variable}: value must be one of {.val {sort_accepted}}.",
    env = env
  )
}
