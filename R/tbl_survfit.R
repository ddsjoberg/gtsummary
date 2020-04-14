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
#' @param failure Calculate failure probabilities rather than survival probabilities.
#' Default is `FALSE`.  Does not apply to survival quantile requests
#'
#' @export
#' @author Daniel D. Sjoberg
#' @examples
#' library(survival)
#' fit1 <- survfit(Surv(ttdeath, death) ~ trt, trial)
#' fit2 <- survfit(Surv(ttdeath, death) ~ 1, trial)
#'
#' tbl_survfit_ex1 <- tbl_survfit(
#'   fit1,
#'   times = c(12, 24),
#'   label = "Treatment",
#'   label_header = "**{time} Month**"
#' )
#'
#' tbl_survfit_ex2 <- tbl_survfit(
#'   fit2,
#'   probs = 0.5,
#'   label_header = "**Median Survival**"
#' )
#' @section Example Output:
#' \if{html}{Example 1}
#'
#' \if{html}{\figure{tbl_survfit_ex1.png}{options: width=40\%}}
#'
#' \if{html}{Example 2}
#'
#' \if{html}{\figure{tbl_survfit_ex2.png}{options: width=40\%}}

tbl_survfit <- function(x, times = NULL, probs = NULL,
                        statistic = "{estimate} ({conf.low}, {conf.high})",
                        label = NULL, label_header = NULL, estimate_fun = NULL,
                        missing = "--", conf.level = 0.95, failure = FALSE) {

  # input checks ---------------------------------------------------------------
  if (c(is.null(times), is.null(probs)) %>% sum() != 1) {
    stop("One and only one of `times=` and `probs=` must be specified.", call. = FALSE)
  }
  if (!rlang::is_string(statistic) || !rlang::is_string(label %||% "") ||
      !rlang::is_string(label_header %||% "")) {
    stop("`statistic=`, `label=`, and `label_header=` arguments must be strings of length one.",
         call. = FALSE)
  }
  if (failure == TRUE && !is.null(probs)) {
    rlang::inform("`failure=TRUE` argument ignored for survival quantile estimation.")
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
                             conf.level = conf.level, failure = failure)
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
    expr(modify_header_internal(results, label = "**Characteristic**", !!!lbls)) %>%
    eval()

  # assigning class
  class(results) <- c("tbl_survfit", "gtsummary")

  results
}


# calcualtes and prepares survival quantile estimates for tbl
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
      label = switch(length(.env$strata) == 0, "Overall") %||%
        stringr::word(strata, start = 2L, sep = "="),
      col_label = .env$label_header %||%
        "**{style_percent(prob, symbol = TRUE)} Percentile**" %>%
        glue() %>% as.character()
    )

  # removing strata column if there are no stratum in survfit
  if (length(strata) == 0) df_stat <- select(df_stat, -.data$strata)

  df_stat
}


# calcualtes and prepares n-year survival estimates for tbl
survfit_time <- function(x, times, label_header, conf.level, failure) {
  tidy <- broom::tidy(x, conf.level = conf.level)
  strata <- intersect("strata", names(tidy)) %>% list() %>% compact()

  # adding time 0 to data frame
  tidy <-
    tidy %>%
    select(any_of(c("time", "estimate", "conf.high", "conf.low", "strata"))) %>%
    bind_rows(
      group_by(., !!!syms(strata)) %>%
        slice(1) %>%
        mutate(time = 0, estimate = 1, conf.low = 1, conf.high = 1)
    ) %>%
    ungroup()

  # getting requested estimates
  df_stat <-
    tidy %>%
    # adding in
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
    arrange(!!!syms(strata), .data$time) %>%
    group_by(!!!syms(strata)) %>%
    mutate_at(
      vars(.data$estimate, .data$conf.high, .data$conf.low),
      ~ifelse(is.na(.) & dplyr::row_number() != n(),
              dplyr::lag(., n = 1), .)
    ) %>%
    filter(.data$time %in% .env$times) %>%
    mutate(
      variable = switch(length(.env$strata) == 0, "..overall..") %||%
        stringr::word(strata, start = 1L, sep = "="),
      label = switch(length(.env$strata) == 0, "Overall") %||%
        stringr::word(strata, start = 2L, sep = "="),
      col_label = .env$label_header %||% "**Time {time}**" %>% glue() %>% as.character()
    ) %>%
    select(any_of(c("variable", "label", "strata", "col_name", "col_label")),
           everything()) %>%
    ungroup()

  # converting to failure probs, if requested
  if (failure == TRUE) {
    df_stat <-
      df_stat %>%
      mutate_at(vars(.data$estimate, .data$conf.low, .data$conf.high), ~ 1 - .) %>%
      dplyr::rename(conf.low = .data$conf.high, conf.high = .data$conf.low)
  }

  df_stat
}
