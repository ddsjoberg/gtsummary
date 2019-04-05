#' Tabulate Kaplan-Meier survival probabilites for time-to-event endpoints
#'
#' Function calculates a `survfit` object, and provides a tablulated and formmated
#' summary of the results
#'
#' @param data a data frame in which to interpret the variables named in the formula
#' @param formula a formula object, which must have a Surv object as the response on the
#' left of the ~ operator.  A single stratification variable maybe included
#' right of the ~ operator. For an unstratified survival estimate the
#' right hand side should be `~ 1`.
#' @param times numeric vector of survival times
#' @param time_label string defining the label shown for the time column.
#' Default is `"{time}"`.  The input uses \code{\link[glue]{glue}} notation to
#' convert the string into a label.  A common label may be `"{time} Months"`, which
#' would resolve to "6 Months" or "12 Months" depending on the times requested.
#' @param level_label used when survival results are stratified.
#' It is a string defining the label shown.  The input uses
#' \code{\link[glue]{glue}} notation to convert the string into a label.
#' The default is \code{"{level}, N = {n}"}.  Other information available to call are `'{n}'`, `'{variable}'`,
#' and `'{strata}'`.
#' @param header_time string to be displayed as column header of the time column.
#' Default is \code{md('**Time**')}
#' @param header_prob string to be displayed as column header of the Kaplan-Meier
#' estimate.  Default is \code{md('**Probability**')}
#' @inheritDotParams survival::survfit.formula -data -formula
#' @importFrom stringr str_split
#' @seealso \link[gt]{md}, \link[gt]{html}
#' @author Daniel D. Sjoberg
#' @export
#' @examples
#' tbl_strata <-
#'   tbl_survival(
#'     trial,
#'     Surv(ttdeath, death) ~ trt,
#'     times = c(12, 24),
#'     time_label = "{time} Months"
#'   )
#' tbl_nostrata <-
#'   tbl_survival(
#'     trial,
#'     Surv(ttdeath, death) ~ 1,
#'     times = c(12, 24),
#'     time_label = "{time} Months"
#'   )

tbl_survival <- function(data, formula, times,
                                 time_label = "{time}",
                                 level_label = "{level}, N = {n}",
                                 header_time = md('**Time**'),
                                 header_prob = md('**Probability**'),
                                 ...) {
  # converting headers to un-evaluated string ----------------------------------
  header_time <- deparse(substitute(header_time))
  header_prob <- deparse(substitute(header_prob))

  # checking that only a single stratification variable passed in formula
  if (all.vars(formula[-2]) %>% length() > 1) {
    stop("tbl_survival allows only a single stratification variable")
  }

  # estimating survfit object
  survfit <- survival::survfit(formula, data, ...)

  # getting survival estimates
  survfit_summary <- survfit %>%
    summary(times = times)

  # converting output into tibble
  table_body <-
    survfit_summary %>%
    {.[names(.) %in% c("strata", "time", "surv", "lower", "upper", "n.risk")]} %>%
    as_tibble()

  # if there are strata, extract n, name, levels of variable name
  if ("strata" %in% names(table_body)) {
    table_body <-
      table_body %>%
      left_join(
        tibble(
          strata = table_body$strata %>% unique(),
          n = survfit_summary$n
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
        n = survfit_summary$n
      )
  }

  # creating time label
  table_body <-
    table_body %>%
    mutate(
      time_label = glue(time_label)
    )  %>%
    select(c("time_label", "surv", "lower", "upper"), everything())

  # list of variable to hide when printing
  if ("strata" %in% names(table_body))
    cols_hide_list <- "time, n.risk, strata, variable, level, n"
  else cols_hide_list <- "time, n.risk, n"

  # creating return results object
  result = list()
  result[["table_body"]] <- table_body
  result[["survfit"]] <- survfit
  result[["gt_calls"]] <- list(
    # first call to gt
    gt = glue("gt(data = x$table_body)"),
    # centering columns except time
    cols_align = glue("cols_align(align = 'center') %>%" ,
                      "cols_align(align = 'left', columns = vars(time_label))"),
    # hiding columns not for printing
    cols_hide = glue("cols_hide(columns = vars({cols_hide_list}))"),
    # labelling columns
    cols_label =
      glue('cols_label(time_label = {header_time}, surv = {header_prob}, lower = md("**{survfit$conf.int*100}% CI**"))'),
    # styling the percentages
    fmt_percent =
      glue("fmt(columns = vars(surv, lower, upper), fns = partial(style_percent, symbol = TRUE))"),
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
