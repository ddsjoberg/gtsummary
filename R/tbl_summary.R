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
tbl_summary <- function(data, by = NULL, label = NULL, statistic = NULL,
                        digits = NULL, type = NULL, value = NULL,
                        missing = c("ifany", "no", "always"),
                        missing_text = "Unknown", missing_stat = "{N_miss}",
                        sort = NULL, percent = c("column", "row", "cell"),
                        include = c(everything(), -by)) {
  # process arguments ----------------------------------------------------------
  data <- dplyr::ungroup(data)
  cards::process_selectors(data, by = {{by}}, include = {{include}})
  include <- setdiff(include, by) # remove by variable from list vars included
  missing <- rlang::arg_match(arg = missing)
  percent <- rlang::arg_match(arg = percent)
  cards::process_formula_selectors(data = data[include], value = value)

  cards::process_formula_selectors(
    data = data[include],
    label = label,
    statistic = statistic,
    digits = digits,
    type = type
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

.guess_summary_type <- function(data,
                                variables,
                                value,
                                data_name = caller_arg(data),
                                env = caller_env()) {
  map(
    variables,
    function(variable) {
      # logical variables are dichotomous
      if (inherits(data[[variable]], "logical")) {
        return("dichotomous")
      }

      # if all missing
      if (sum(is.na(data[[variable]])) == nrow(data)) {
        if (inherits(data[[variable]], base_numeric_classes)) {
          return("continuous")
        }
        if (inherits(data[[variable]], "character")) {
          return("dichotomous")
        }
        if (inherits(data[[variable]], "factor") &&
            !rlang::is_empty(attr(data[[variable]], "levels"))) {
          return("categorical")
        }
        if (inherits(data[[variable]], "factor") &&
            rlang::is_empty(attr(data[[variable]], "levels"))) {
          return("dichotomous")
        }
      }

      # numeric variables that are 0 and 1 only, will be dichotomous
      if (inherits(data[[variable]], c("integer", "numeric")) &&
          length(setdiff(stats::na.omit(data[[variable]]), c(0, 1))) == 0) {
        return("dichotomous")
      }

      # factor variables that are "No" and "Yes" only, will be dichotomous
      if (inherits(data[[variable]], "factor") &&
          setequal(attr(data[[variable]], "levels"), c("No", "Yes"))) {
        return("dichotomous")
      }
      if (inherits(data[[variable]], "factor") &&
          setequal(attr(data[[variable]], "levels"), c("no", "yes"))) {
        return("dichotomous")
      }
      if (inherits(data[[variable]], "factor") &&
          setequal(attr(data[[variable]], "levels"), c("NO", "YES"))) {
        return("dichotomous")
      }

      # character variables that are "No" and "Yes" only, will be dichotomous
      if (inherits(data[[variable]], "character") &&
          setequal(stats::na.omit(data[[variable]]), c("No", "Yes"))) {
        return("dichotomous")
      }
      if (inherits(data[[variable]], "character") &&
          setequal(stats::na.omit(data[[variable]]), c("no", "yes"))) {
        return("dichotomous")
      }
      if (inherits(data[[variable]], "character") &&
          setequal(stats::na.omit(data[[variable]]), c("NO", "YES"))) {
        return("dichotomous")
      }

      # factors and characters are categorical (except when all missing)
      if (inherits(data[[variable]], c("factor", "character"))) {
        return("categorical")
      }

      # numeric variables with fewer than 10 levels will be categorical
      if (inherits(data[[variable]], base_numeric_classes) &&
          length(unique(stats::na.omit(data[[variable]]))) < 10) {
        return("categorical")
      }

      # all other numeric classes are continuous
      if (inherits(data[[variable]], base_numeric_classes)) {
        return(get_theme_element("tbl_summary-str:default_con_type", default = "continuous"))
      }
    }
  ) |>
    stats::setNames(variables)

}
