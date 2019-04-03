#' Tabulate survival probabilites for time-to-event endpoints
#'
#' @param x survival object
#' @param ... further arguments passed to or from other methods.
#' @author Daniel D. Sjoberg
#' @seealso \code{\link{tbl_survival.survfit}},
#' \code{\link{inline_text.tbl_regression}},
#' \link{inline_text.tbl_uvregression}
#' @export
tbl_survival <- function(x, ...) {
  ellipsis::check_dots_used()
  UseMethod("tbl_survival")
}

#' Tabulate Kaplan-Meier survival probabilites for time-to-event endpoints
#'
#' Function accepts a `survfit` object, and provides a tablulated and formmated
#' summary of the results
#'
#' @param x `survfit` object
#' @param times numeric vector of survival times
#' @param time_label string defining the label shown for the time column.
#' Default is "{time}".  The input uses \code{\link[glue]{glue}} notation to
#' convert the string into a label.  A common label may be "{time} Months", which
#' would resolve to "6 Months" or "12 Months" depending on the times requested.
#' @param level_label used when the input 'x' results are stratified by a variable.
#' It is a string defining the label shown.  The input uses
#' \code{\link[glue]{glue}} notation to convert the string into a label.
#' The default is "{level}", which will print the level of the stratifying
#' variable.  Other information available to call are {n}', '{variable}',
#' and '{strata}'.
#' @param ... not used
#' @importFrom stringr str_split
#' @author Daniel D. Sjoberg
#' @export
#' @examples
#' library(survival)
#' lung %>%
#' dplyr::mutate(
#'   sex_fct = factor(sex, labels = c("Male", "Female"))
#' ) %>%
#' survfit(Surv(time, status) ~ sex_fct, .) %>%
#' tbl_survival(times = c(30, 60, 90), time_label = "{time} Days")

tbl_survival.survfit <- function(x, times, time_label = "{time}",
                                 level_label = "{level}", ...) {

  # getting survival estimates
  x_summary <- x %>%
    summary(times = times)

  # converting output into tibble
  table_body <-
    x_summary %>%
    {.[names(.) %in% c("strata", "time", "surv", "lower", "upper", "n.risk")]} %>%
    as_tibble()

  # if there are strata, extract n, name, levels of variable name
  if ("strata" %in% names(table_body)) {
    table_body <-
      table_body %>%
      left_join(
        tibble(
          strata = x_summary$strata %>% attr("levels") %>% factor(),
          n = x_summary$n
        ),
        by = "strata"
      ) %>%
      mutate(
        variable = str_split(.data$strata, pattern = "=", n = 2) %>% map_chr(pluck(1)),
        level = str_split(.data$strata, pattern = "=", n = 2) %>% map_chr(pluck(2)),
        groupname = glue(level_label)
      )
  }
  # else if no strata, adding n to table_body
  else {
    table_body <-
      table_body %>%
      mutate(
        n = x_summary$n
      )
  }

  # creating time label
  table_body <-
    table_body %>%
    mutate(
      time_label = glue(time_label)
    )

  # creating return results object
  result = list()
  result[["table_body"]] <- table_body
  result[["survfit"]] <- x
  result[["gt_calls"]] <- list(
    # first call to gt
    gt = glue("gt(data = x$table_body)"),
    # centering columns except time
    cols_align = glue("cols_align(align = 'center') %>%",
                      "cols_align(align = 'left', columns = vars(time_label))"),
    # hiding columns not for printing
    cols_hide = glue("cols_hide(columns = vars(time, n.risk, strata, variable, level, n))"),
    # reordering columns
    cols_move_to_start =
      glue("cols_move_to_start(columns = vars(time_label, surv, lower, upper))"),
    # labelling columns
    cols_label =
      glue('cols_label(time_label = "", surv = "Survival Probability", lower = "{x$conf.int*100}% CI")'),
    # styling the percentages
    fmt_percent =
      glue("fmt(columns = vars(surv, lower, upper), fns = partial(style_percent, symbol = TRUE))"),
    # mergin upper adn lower limits of CI
    cols_merge_ci =
      "cols_merge(col_1 = vars(lower), col_2 = vars(upper), pattern = '{1}, {2}')" %>%
      glue::as_glue(),
    # adding CI footnote
    footnote_abbreviation =
      glue("tab_footnote(footnote = 'CI = Confidence Interval',",
           "locations = cells_column_labels(columns = vars(lower)))")

  )

  class(result) <- "tbl_survival"
  result
}
