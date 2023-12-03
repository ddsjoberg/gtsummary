#' Title
#'
#' @param data TODO:
#' @param by TODO:
#' @param label TODO:
#' @param statistic TODO:
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
#' @examples
#' tbl <-
#'   tbl_summary(
#'     data = mtcars,
#'     include = c("cyl", "am", "mpg", "hp"),
#'     type =
#'       list(
#'         cyl = "categorical",
#'         am = "dichotomous",
#'         mpg = "continuous",
#'         hp = "continuous2"
#'       ),
#'     value = list(am = 1),
#'     statistic =
#'       list(
#'         c(cyl, am) ~ "{n} ({p}%)",
#'         mpg = "{mean} ({sd})",
#'         hp = c("{mean}", "{median}")
#'       )
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
                        sort = all_categorical() ~ "alphanumeric",
                        percent = c("column", "row", "cell"),
                        include = everything()) {
  # process arguments ----------------------------------------------------------
  data <- dplyr::ungroup(data)
  cards::process_selectors(data, by = {{by}}, include = {{include}})
  include <- setdiff(include, by) # remove by variable from list vars included
  include <- .remove_na_columns(data, include)
  missing <- rlang::arg_match(arg = missing)
  percent <- rlang::arg_match(arg = percent)
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

  # evaluate the remaining list-formula arguments ------------------------------
  # processed arguments are saved into this env
  cards::process_formula_selectors(
    data = .add_summary_type_as_attr(data[include], type),
    label = label,
    statistic =
      .ifelse1(
        missing(statistic),
        get_theme_element("TODO:fill-this-in", default = statistic),
        statistic
      ) ,
    digits = digits,
    sort = sort
  )

  # fill in unspecified variables
  cards::fill_formula_selectors(
    data[include],
    statistic =
      get_theme_element("TODO:fill-this-in", default = eval(formals(gtsummary::tbl_summary)[["statistic"]]))
  )

  # save processed function inputs ---------------------------------------------
  tbl_summary_inputs <- as.list(environment())
  call <- match.call()

  # construct cards ------------------------------------------------------------
  cards <-
    cards::bind_ard(
      cards::ard_attributes(data, variables = all_of(c(include, by)), label = label),
      # tabulate by variable for header stats
      if (!rlang::is_empty(by)) cards::ard_categorical(data, variables = all_of(by)),
      # tabulate categorical summaries
      cards::ard_categorical(
        data,
        by = by,
        variables = .get_variables_by_type(type, c("categorical", "dichotomous"))
      ),
      # calculate categorical summaries
      cards::ard_continuous(
        data,
        by = by,
        variables = .get_variables_by_type(type, c("continuous", "continuous2"))
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
      text_format = "indent"
    )

  x <-
    modify_header(
      x,
      all_stat_cols() ~ ifelse(is_empty(by), "**N = {N}**", "**{level}**  \nN = {n}")
    )

  # return object
  x
}

.get_variables_by_type <- function(x, type) {
  names(x)[unlist(x) %in% type]
}

.remove_na_columns <- function(data, include) {
  is_all_na <- map_lgl(include, function(x) all(is.na(x)))
  if (any(is_all_na)) {
    paste("Columns {.val {include[is_all_na]}} are all {.cls NA}",
          "and have been removed from the summary table.") |>
    cli::cli_inform()
  }

  include[!is_all_na]
}
