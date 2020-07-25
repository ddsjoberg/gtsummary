#' Creates table of survival probabilities
#'
#' \Sexpr[results=rd, stage=render]{lifecycle::badge("experimental")}
#' Function takes a `survfit` object as an argument, and provides a
#' formatted summary table of the results
#'
#' @param x survfit object. Object may have no stratification
#' (e.g. `survfit(Surv(ttdeath, death) ~ 1, trial)`), or a single stratifying
#' variable (e.g. `survfit(Surv(ttdeath, death) ~ trt, trial)`)
#' @param times numeric vector of times for which to return survival probabilities.
#' @param probs numeric vector of probabilities with values in (0,1)
#' specifying the survival quantiles to return
#' @param statistic string defining the statistics to present in the table.
#' Default is `"{estimate} ({conf.low}, {conf.high})"`
#' @param label string specifying variable or overall label. Default is
#' stratifying variable name or `"Overall"` when no stratifying variable present
#' @param label_header string specifying column labels above statistics. Default
#' is `"{prob} Percentile"` for survival percentiles, and `"Time {time}"` for n-year
#' survival estimates
#' @param estimate_fun function to format the Kaplan-Meier estimates. Default
#' is [style_percent] for survival probabilities and [style_sigfig] for
#' survival times
#' @param missing text to fill when estimate is not estimable. Default is `"--"`
#' @param conf.level Confidence level for confidence intervals. Default is 0.95
#' @param reverse Flip the probability reported, i.e. `1 - estimate`.
#' Default is `FALSE`.  Does not apply to survival quantile requests
#' @param failure DEPRECATED. Use `reverse=` instead.
#'
#' @export
#' @author Daniel D. Sjoberg
#' @examples
#' library(survival)
#' fit1 <- survfit(Surv(ttdeath, death) ~ trt, trial)
#' fit2 <- survfit(Surv(ttdeath, death) ~ 1, trial)
#'
#' # Example 1 ----------------------------------
#' tbl_survfit_ex1 <- tbl_survfit(
#'   fit1,
#'   times = c(12, 24),
#'   label = "Treatment",
#'   label_header = "**{time} Month**"
#' )
#'
#' # Example 2 ----------------------------------
#' tbl_survfit_ex2 <- tbl_survfit(
#'   fit2,
#'   probs = 0.5,
#'   label_header = "**Median Survival**"
#' )
#'
#' # Example 3 Competing Events Example ---------
#' # adding a competing event for death (cancer vs other causes)
#' library(dplyr)
#' trial2 <- trial %>%
#'   mutate(
#'   death_cr = case_when(
#'     death == 0 ~ "censor",
#'     runif(n()) < 0.5 ~ "death from cancer",
#'     TRUE ~ "death other causes"
#'   ) %>% factor()
#' )
#'
#' survfit_cr_ex3 <-
#'   survfit(Surv(ttdeath, death_cr) ~ grade, data = trial2) %>%
#'   tbl_survfit(times = c(12, 24), label = "Tumor Grade")
#' @section Example Output:
#' \if{html}{Example 1}
#'
#' \if{html}{\figure{tbl_survfit_ex1.png}{options: width=40\%}}
#'
#' \if{html}{Example 2}
#'
#' \if{html}{\figure{tbl_survfit_ex2.png}{options: width=27\%}}
#'
#' \if{html}{Example 3}
#'
#' \if{html}{\figure{survfit_cr_ex3.png}{options: width=27\%}}

tbl_survfit <- function(x, times = NULL, probs = NULL,
                        statistic = "{estimate} ({conf.low}, {conf.high})",
                        label = NULL, label_header = NULL, estimate_fun = NULL,
                        missing = "--", conf.level = 0.95, reverse = FALSE,
                        failure = NULL) {
  # deprecation notes ----------------------------------------------------------
  if (!is.null(failure)) {
    lifecycle::deprecate_warn(
      "1.3.1", "gtsummary::tbl_survfit(failure = )", "tbl_survfit(reverse = )")
    reverse <- failure
  }

  # setting defaults -----------------------------------------------------------
  statistic <-
    statistic %||%
    get_theme_element("tbl_survfit-arg:statistic") %||%
    "{estimate} ({conf.low}, {conf.high})"

  # input checks ---------------------------------------------------------------
  assert_package("survival", "tbl_survfit")
  if (!inherits(x, "survfit")) {
    stop("Argument `x=` must be class 'survfit' created from the `survival::survfit()` function.",
         call. = FALSE)
  }

  if (c(is.null(times), is.null(probs)) %>% sum() != 1) {
    stop("One and only one of `times=` and `probs=` must be specified.", call. = FALSE)
  }
  if (!rlang::is_string(statistic) || !rlang::is_string(label %||% "") ||
      !rlang::is_string(label_header %||% "")) {
    stop("`statistic=`, `label=`, and `label_header=` arguments must be strings of length one.",
         call. = FALSE)
  }
  if (reverse == TRUE && !is.null(probs)) {
    rlang::inform("`reverse=TRUE` argument ignored for survival quantile estimation.")
  }

  # setting defaults -----------------------------------------------------------
  estimate_type <- ifelse(is.null(times), "probs", "times")
  estimate_fun <-
    estimate_fun %||%
    switch(
      estimate_type,
      probs = getOption("gtsummary.tbl_survfit.probs.estimate_fun"),
      times = getOption("gtsummary.tbl_survfit.times.estimate_fun")
    ) %||%
    switch(
      estimate_type,
      probs = partial(style_sigfig, digits = 2),
      times = partial(style_percent, symbol = TRUE)
    )

  # will return call, and all object passed to in tbl_summary call -------------
  # the object func_inputs is a list of every object passed to the function
  tbl_survfit_inputs <- as.list(environment())

  # calculating estimates ------------------------------------------------------
  if (estimate_type == "times")
    df_stats <- survfit_time(x, times = times, label_header = label_header,
                             conf.level = conf.level, reverse = reverse)
  else if (estimate_type == "probs")
    df_stats <- survfit_prob(x, probs = probs, label_header = label_header,
                             conf.level = conf.level)

  # table_body -----------------------------------------------------------------
  strata <- intersect("strata", names(df_stats)) %>% list() %>% compact()
  table_body <-
    df_stats %>%
    mutate_at(vars(.data$estimate, .data$conf.low, .data$conf.high),
              ~ coalesce(as.character(estimate_fun(.)), missing)) %>%
    mutate(
      statistic = glue(.env$statistic) %>% as.character(),
      row_type = switch(length(strata) == 0, "label") %||% "level"
    ) %>%
    select(c("variable", "row_type", "label", "col_name", "statistic")) %>%
    tidyr::pivot_wider(id_cols = c(.data$variable, .data$row_type, .data$label),
                       names_from = c(.data$col_name),
                       values_from = c(.data$statistic))
  # adding label row, if needed
  if (nrow(table_body) > 1) {
    table_body <-
      table_body %>%
      select(.data$variable) %>%
      distinct() %>%
      mutate(row_type = "label",
             label = .env$label %||% .data$variable) %>%
      bind_rows(table_body)
  }

  # table_header ---------------------------------------------------------------
  table_header <-
    tibble(column = names(table_body)) %>%
    table_header_fill_missing()

  # finishing up ---------------------------------------------------------------
  # constructing final result
  results <- list(
    table_body = table_body,
    table_header = table_header,
    table_stats = df_stats,
    inputs = tbl_survfit_inputs,
    call_list = list(tbl_survfit = match.call())
  )

  # applying labels
  lbls <- as.list(unique(df_stats$col_label)) %>% set_names(unique(df_stats$col_name))
  results <-
    expr(modify_header_internal(results, label = !!paste0("**", translate_text("Characteristic"), "**"), !!!lbls)) %>%
    eval()

  # assigning class
  class(results) <- c("tbl_survfit", "gtsummary")

  results
}


# calculates and prepares survival quantile estimates for tbl
survfit_prob <- function(x, probs, label_header, conf.level) {

  strata <- intersect("strata", names(broom::tidy(x, conf.level = conf.level))) %>%
    list() %>% compact()

  # calculating survival quantiles, and adding estimates to pretty tbl
  df_stat <- imap_dfr(
    probs,
    ~stats::quantile(x, probs = .x) %>%
      as.data.frame() %>%
      tibble::rownames_to_column() %>%
      set_names(c("strata", "estimate", "conf.low", "conf.high")) %>%
      mutate(
        prob = .x,
        col_name = paste("stat", .y, sep = "_")
      )
  ) %>%
    # creating labels
    mutate(
      variable = switch(length(.env$strata) == 0, "..overall..") %||%
        stringr::word(strata, start = 1L, sep = "="),
      label = switch(length(.env$strata) == 0, translate_text("Overall")) %||%
        stringr::word(strata, start = 2L, sep = "="),
      col_label = .env$label_header %||%
        # for some languages, we show 'Percentile 50%' instead of '50% Percentile'
        switch(get_theme_element("pkgwide-str:language", default = "en") %in% "es",
               "**{style_percent(prob, symbol = TRUE)} {translate_text('Percentile')}**") %||%
        "**{style_percent(prob, symbol = TRUE)} {translate_text('Percentile')}**" %>%
        glue() %>% as.character()
    )

  # removing strata column if there are no stratum in survfit
  if (length(strata) == 0) df_stat <- select(df_stat, -.data$strata)

  df_stat
}

# calcualtes and prepares n-year survival estimates for tbl
survfit_time <- function(x, times, label_header, conf.level, reverse) {
  tidy <- broom::tidy(x, conf.level = conf.level)
  strata <- intersect("strata", names(tidy)) %>% list() %>% compact()
  multi_state <- inherits(x, "survfitms")
  if (multi_state == TRUE) {
    # selecting state to show
    state <- unique(tidy$state) %>%
      setdiff("(s0)") %>%
      purrr::pluck(1)

    rlang::inform(glue(
      "Multi-state model detected. Showing probabilities into state '{state}'"
    ))

    tidy <- dplyr::filter(tidy, .data$state == .env$state)
  }

  # adding time 0 to data frame
  tidy <-
    tidy %>%
    # if CI is missing, and SE is 0, making the CIs the estimate
    mutate_at(vars(.data$conf.high, .data$conf.low),
              ~ifelse(is.na(.) & .data$std.error == 0, .data$estimate, .)) %>%
    select(any_of(c("time", "estimate", "conf.high", "conf.low", "strata"))) %>%
    bind_rows(
      group_by(., !!!syms(strata)) %>%
        slice(1) %>%
        mutate(time = 0,
               estimate = ifelse(multi_state, 0, 1),
               conf.low = ifelse(multi_state, 0, 1),
               conf.high = ifelse(multi_state, 0, 1))
    ) %>%
    ungroup()



  # getting requested estimates
  df_stat <-
    tidy %>%
    # getting the latest time (not showing estimates after that time)
    group_by(., !!!syms(strata)) %>%
    mutate(time_max = max(.data$time)) %>%
    ungroup() %>%
    # adding in timepoints requested by user
    full_join(
      select(tidy, !!!syms(strata)) %>%
        distinct()  %>%
        mutate(
          time = list(.env$times),
          col_name = list(paste("stat", seq_len(length(.env$times)), sep = "_"))
        ) %>%
        unnest(cols = c(.data$time, .data$col_name)),
      by = unlist(c(strata, "time"))
    ) %>%
    # if the user-specifed time is unobserved, filling estimates with previous value
    arrange(!!!syms(strata), .data$time) %>%
    group_by(!!!syms(strata)) %>%
    tidyr::fill(.data$estimate, .data$conf.high, .data$conf.low,
                .data$time_max, .direction = "down") %>%
    ungroup() %>%
    # keeping obs of user-specified times
    filter(!is.na(.data$col_name)) %>%
    # if user-specified time is after the latest follow-up time, making it NA
    mutate_at(vars(.data$estimate, .data$conf.high, .data$conf.low),
              ~ifelse(.data$time > .data$time_max, NA_real_, .)) %>%
    mutate(
      variable = switch(length(.env$strata) == 0, "..overall..") %||%
        stringr::word(strata, start = 1L, sep = "="),
      label = switch(length(.env$strata) == 0, translate_text("Overall")) %||%
        stringr::word(strata, start = 2L, sep = "="),
      col_label = .env$label_header %||% paste0("**", translate_text("Time"), " {time}**") %>% glue() %>% as.character()
    ) %>%
    select(any_of(c("variable", "label", "strata", "col_name", "col_label")),
           everything(), -.data$time_max)

  # converting to reverse probs, if requested
  if (reverse == TRUE) {
    df_stat <-
      df_stat %>%
      mutate_at(vars(.data$estimate, .data$conf.low, .data$conf.high), ~ 1 - .) %>%
      dplyr::rename(conf.low = .data$conf.high, conf.high = .data$conf.low)
  }

  df_stat
}
