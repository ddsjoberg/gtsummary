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
  percent <- rlang::arg_match0(arg = percent)

  cards::process_formula_selectors(
    data = data[include],
    label = label,
    statistic = statistic,
    digits = digits,
    type = type
  )

  # save processed function inputs ---------------------------------------------
  tbl_summary_inputs <- as.list(environment())

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

  # return object
  list(
    cards = cards,
    calls = list(tbl_summary = tbl_summary_inputs)
  ) |>
    structure(class = c("tbl_summary", "gtsummary")) |>
    construct_gtsummary() |>
    modify_table_styling(
      columns = "label",
      rows = .data$row_type %in% c("level", "missing"),
      text_format = "indent"
    )
}

.get_variables_by_type <- function(x, type) {
  names(x)[unlist(x) %in% type]
}
