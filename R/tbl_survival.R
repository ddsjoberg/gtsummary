#' Creates table of univariate summary statistics for time-to-event endpoints
#'
#' \Sexpr[results=rd, stage=render]{lifecycle::badge("questioning")}
#' Please use [tbl_survfit].
#' Function takes a `survfit` object as an argument, and provides a
#' formatted summary of the results
#'
#' @param x A survfit object
#' @param ... Additional arguments passed to other methods
#' @seealso [tbl_survival.survfit]
#' @keywords internal
#' @export
tbl_survival <- function(x, ...) {
  UseMethod("tbl_survival")
}

#' Creates table of survival probabilities
#'
#' \Sexpr[results=rd, stage=render]{lifecycle::badge("questioning")}
#' Please use [tbl_survfit].
#' Function takes a `survfit` object as an argument, and provides a
#' formatted summary of the results
#'
#' @param x A survfit object with a no stratification
#' (e.g. `survfit(Surv(ttdeath, death) ~ 1, trial)`), or a single stratifying
#' variable (e.g. `survfit(Surv(ttdeath, death) ~ trt, trial)`)
#' @param times Numeric vector of times for which to return survival probabilities.
#' @param probs Numeric vector of probabilities with values in (0,1)
#' specifying the survival quantiles to return
#' @param label String defining the label shown for the time or prob column.
#' Default is `"{time}"` or `"{prob*100}%"`.  The input uses [glue::glue] notation to
#' convert the string into a label.  A common label may be `"{time} Months"`, which
#' would resolve to "6 Months" or "12 Months" depending on specified \code{times}.
#' @param level_label Used when survival results are stratified.
#' It is a string defining the label shown.  The input uses
#' [glue::glue] notation to convert the string into a label.
#' The default is \code{"{level}, N = {n}"}.  Other information available to
#' call are `'{n}'`, `'{level}'`, `'{n.event.tot}'`, `'{n.event.strata}'`, and
#' `'{strata}'`. See below for details.
#' @param header_label String to be displayed as column header.
#' Default is \code{'**Time**'} when `time` is specified, and
#' \code{'**Quantile**'} when `probs` is specified.
#' @param header_estimate String to be displayed as column header of the Kaplan-Meier
#' estimate.  Default is \code{'**Probability**'} when `time` is specified, and
#' \code{'**Time**'} when `probs` is specified.
#' @param failure Calculate failure probabilities rather than survival probabilities.
#' Default is `FALSE`.  Does NOT apply to survival quantile requests
#' @param missing String indicating what to replace missing confidence
#' limits with in output.  Default is `missing = "-"`
#' @param estimate_fun Function used to format the estimate and confidence
#' limits. The default is `style_percent(x, symbol = TRUE)` for survival
#' probabilities, and
#' `style_sigfig(x, digits = 3)` for time estimates.
#' @param ... Not used
#' @family tbl_survival tools
#' @author Daniel D. Sjoberg
#' @export
#' @keywords internal
#' @return A `tbl_survival` object
#' @examples
#' library(survival)
#' fit1 <- survfit(Surv(ttdeath, death) ~ trt, trial)
#' tbl_strata_ex1 <-
#'   tbl_survival(
#'     fit1,
#'     times = c(12, 24),
#'     label = "{time} Months"
#'   )
#'
#' fit2 <- survfit(Surv(ttdeath, death) ~ 1, trial)
#' tbl_nostrata_ex2 <-
#'   tbl_survival(
#'     fit2,
#'     probs = c(0.1, 0.2),
#'     header_estimate = "**Months**"
#'   )
#' @section level_label argument:
#' The `level_label` is used to modify the stratum labels. The default is
#' \code{level_label = "{level}, N = {n}"}. The quantities in the curly
#' brackets evaluate to stratum-specific values.  For example, in the trial
#' data set, there is a column called `trt` with levels 'Drug A' and 'Drug B'.
#' In this example, `{level}` would evaluate to either 'Drug A' or 'Drug B'
#' depending on the stratum.  Other quantities available to print are:
#' \itemize{
#'   \item `{level}` level of the stratification variable
#'   \item `{level_label}` label of level for the stratification variable
#'   \item `{n}` number of observations, or number within stratum
#'   \item `{n.event.tot}` total number of events (total across stratum, if applicable)
#'   \item `{n.event.strata}` total number of events within stratum, if applicable
#'   \item `{strata}` raw stratum specification from \code{survfit} object
#' }
#'
#' @section Example Output:
#' \if{html}{Example 1}
#'
#' \if{html}{\figure{tbl_strata_ex1.png}{options: width=40\%}}
#'
#' \if{html}{Example 2}
#'
#' \if{html}{\figure{tbl_nostrata_ex2.png}{options: width=40\%}}

tbl_survival.survfit <- function(x, times = NULL, probs = NULL,
                                 label = ifelse(is.null(probs), "{time}", "{prob*100}%"),
                                 level_label = "{level}, N = {n}",
                                 header_label = NULL,
                                 header_estimate = NULL,
                                 failure = FALSE,
                                 missing = "-",
                                 estimate_fun = NULL,
                                 ...) {

  # setting defaults -----------------------------------------------------------
  estimate_fun <-
    estimate_fun %||%
    getOption("gtsummary.tbl_survival.estimate_fun") %||%
    switch(
      is.null(times) + 1,
      partial(style_percent, symbol = TRUE),
      partial(style_sigfig, digits = 3)
    )

  # input checks ---------------------------------------------------------------
  if (c(is.null(times), is.null(probs)) %>% sum() != 1) {
    stop("One and only one of 'times' and 'probs' must be specified.")
  }
  if (!rlang::is_string(label) || !rlang::is_string(level_label) ||
    !rlang::is_string(header_label %||% "") ||
    !rlang::is_string(header_label %||% "")) {
    stop("'label', 'header_label', and 'level_label' arguments must be string of length one.")
  }

  # defining default headers ---------------------------------------------------
  header_label <-
    header_label %||% ifelse(is.null(probs), "**Time**", "**Quantile**")
  header_estimate <-
    header_estimate %||% ifelse(is.null(probs), "**Probability**", "**Time**")

  # defining estimate_fun ------------------------------------------------------
  # assigning a function to formating estimates and conf.high and conf.low
  estimate_fun <-
    estimate_fun %||%
    switch(
      is.null(times) + 1,
      partial(style_percent, symbol = TRUE),
      partial(style_sigfig, digits = 3)
    )

  # putting results into tibble -------------------------------------------------
  if (!is.null(probs)) {
    table_long <- surv_quantile(x, probs)
  } else if (!is.null(times)) table_long <- surv_time(x, times, failure)

  # adding additional information to the results table -------------------------
  # if the results are stratified
  if (!is.null(x$strata)) {
    table_long <-
      table_long %>%
      left_join(
        tibble(
          strata = x$strata %>% names(),
          n = x$n,
          n.event.tot = x$n.event %>% sum()
        ),
        by = "strata"
      ) %>%
      # merging in number of events within stratum
      left_join(
        summary(x) %>%
          {
            tibble::tibble(
              strata = .[["strata"]] %>% as.character(),
              n.event.strata = .[["n.event"]]
            )
          } %>%
          dplyr::group_by(.data$strata) %>%
          dplyr::summarise(
            n.event.strata = sum(.data$n.event.strata)
          ),
        by = "strata"
      ) %>%
      # parsing the stratum, and creating
      mutate(
        variable = str_split(.data$strata, pattern = "=", n = 2) %>% map_chr(pluck(1)),
        level = str_split(.data$strata, pattern = "=", n = 2) %>% map_chr(pluck(2)),
        level_label = glue(level_label)
      )
  }
  # if the results are NOT stratified
  else {
    table_long <-
      table_long %>%
      mutate(
        n = x$n,
        n.event.tot = x %>%
          summary(times = max(x$time)) %>%
          pluck("n.event")
      )
  }

  # IF NOT WIDE OPTIONS SPECIFIED, APPLY LABELS AND GT CALLS -------------------
  # creating label column
  table_long <-
    table_long %>%
    mutate(
      label = glue(label),
      ci = case_when(
        !is.na(.data$conf.low) & !is.na(.data$conf.high) ~
        glue("{estimate_fun(conf.low)}, {estimate_fun(conf.high)}"),
        is.na(.data$conf.low) & !is.na(.data$conf.high) ~
        glue("{missing}, {estimate_fun(conf.high)}"),
        !is.na(.data$conf.low) & is.na(.data$conf.high) ~
        glue("{estimate_fun(conf.low)}, {missing}"),
        is.na(.data$conf.low) & is.na(.data$conf.high) ~
        NA_character_
      )
    ) %>%
    select(starts_with("level_label"), c("label", "estimate", "conf.low", "conf.high", "ci"), everything())
  table_body <- table_long %>% mutate(row_type = "label")

  cols_hide_list <-
    c(
      "prob", "time", "strata", "n.risk", "n.event", "n", "n.event.tot",
      "n.event.strata", "variable", "level", "conf.low", "conf.high", "row_type"
    ) %>%
    intersect(names(table_body)) %>%
    paste(collapse = ", ")

  # table of column headers
  table_header <-
    tibble(column = names(table_body)) %>%
    table_header_fill_missing() %>%
    table_header_fmt_fun(estimate = estimate_fun) %>%
    mutate(
      footnote_abbrev = case_when(
        .data$column == "ci" ~ "CI = Confidence Interval",
        TRUE ~ .data$footnote_abbrev
      )
    )

  # creating object to return
  level_label <- intersect("level_label", names(table_body)) %>% list() %>% compact()
  result <- list()
  result[["table_body"]] <- table_body %>% group_by(!!!syms(level_label))
  result[["table_header"]] <- table_header
  result[["table_long"]] <- table_long
  result[["survfit"]] <- x
  result[["call_list"]] <- list(tbl_survival = match.call())
  # result[["gt_calls"]] <- eval(tbl_survival_gt_calls)
  # result[["kable_calls"]] <- eval(tbl_survival_kable_calls)

  # specifying labels
  result <-
    modify_header_internal(
      result,
      label = glue("{header_label}"),
      estimate = glue("{header_estimate}"),
      ci = glue("**{x$conf.int*100}% CI**")
    )

  if ("level_label" %in% names(result$table_body)) {
    result <-
      modify_header_internal(
        result,
        level_label = "**Group**"
      )
  }

  # # writing additional gt and kable calls with data from table_header
  # result <- update_calls_from_table_header(result)

  # returning results
  class(result) <- c("tbl_survival", "gtsummary")
  result
}




surv_time <- function(x, times, failure) {
  # getting survival estimates
  survfit_summary <- x %>%
    summary(times = times)

  # converting output into tibble
  table_body <-
    survfit_summary %>%
    {
      .[names(.) %in% c(
        "strata", "time", "surv",
        "lower", "upper", "n.risk", "n.event"
      )]
    } %>%
    as_tibble() %>%
    rename(
      estimate = .data$surv,
      conf.low = .data$lower,
      conf.high = .data$upper
    )

  # converting strata to character
  if ("strata" %in% names(table_body)) {
    table_body <-
      table_body %>%
      mutate(strata = as.character(.data$strata))
  }

  # converting probabilites to failure if requested
  if (failure == TRUE) {
    table_body <-
      table_body %>%
      mutate_at(vars(.data$estimate, .data$conf.low, .data$conf.high), ~ 1 - .) %>%
      rename(
        conf.low = .data$conf.high,
        conf.high = .data$conf.low
      )
  }

  table_body
}

surv_quantile <- function(x, probs) {
  # logical indicating whether estimates are stratified
  stratified <- !is.null(x$strata)

  # parsing results for stratified models
  if (stratified == TRUE) {
    # getting survival quantile estimates into tibble
    survfit_quantile <- x %>%
      stats::quantile(probs = probs) %>%
      purrr::imap(
        ~ t(.x) %>%
          {
            dplyr::bind_cols(
              tibble::tibble(prob = row.names(.)),
              tibble::as_tibble(.)
            )
          } %>%
          tidyr::gather("strata", !!.y, -prob)
      )

    # merging all result tibbles together
    table_body <-
      survfit_quantile[[1]] %>%
      dplyr::left_join(survfit_quantile[[2]], by = c("prob", "strata")) %>%
      dplyr::left_join(survfit_quantile[[3]], by = c("prob", "strata")) %>%
      rename(estimate = .data$quantile)
  }

  else {
    survfit_quantile <-
      x %>%
      stats::quantile(probs = probs) %>%
      purrr::imap(
        ~ tibble::tibble(
          prob = names(.x),
          !!.y := .x
        )
      )

    # merging all result tibbles together
    table_body <-
      survfit_quantile[[1]] %>%
      dplyr::left_join(survfit_quantile[[2]], by = "prob") %>%
      dplyr::left_join(survfit_quantile[[3]], by = "prob") %>%
      rename(estimate = .data$quantile)
  }

  table_body %>%
    mutate(prob = as.numeric(.data$prob) / 100) %>%
    rename(
      conf.low = .data$lower,
      conf.high = .data$upper
    )
}
