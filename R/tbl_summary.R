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
                        sort = all_categorical(FALSE) ~ "alphanumeric",
                        percent = c("column", "row", "cell"),
                        include = everything()) {
  # data argument checks -------------------------------------------------------
  check_not_missing(data)
  check_class_data_frame(data)

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

  value <- .assign_default_values(data[include], value, type)

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
      ),
    sort =
      .ifelse1(
        missing(sort),
        get_theme_element("TODO:fill-this-in", default = sort),
        sort
      )
  )
  # fill in unspecified variables
  cards::fill_formula_selectors(
    data[include],
    statistic =
      get_theme_element("TODO:fill-this-in", default = eval(formals(gtsummary::tbl_summary)[["statistic"]]))
  )

  cards::process_formula_selectors(
    data = .add_summary_type_as_attr(data[include], type),
    digits = digits
  )

  # sort requested columns by frequency
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
        by = by,
        variables = .get_variables_by_type(type, c("categorical", "dichotomous")),
        fmt_fn = digits
      ),
      # calculate categorical summaries
      cards::ard_continuous(
        data,
        by = by,
        variables = .get_variables_by_type(type, c("continuous", "continuous2")),
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
    unique() |>
    paste(collapse = ", ")
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
