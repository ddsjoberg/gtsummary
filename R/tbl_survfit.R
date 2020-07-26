#' Creates table of survival probabilities
#'
#' \Sexpr[results=rd, stage=render]{lifecycle::badge("experimental")}
#' Function takes a `survfit` object as an argument, and provides a
#' formatted summary table of the results
#'
#' @param x a survfit object, list of survfit objects, or a data frame.
#' If a data frame is passed, a list of survfit objects is constructed using
#' each variable as a stratifying variable.
#' @param times numeric vector of times for which to return survival probabilities.
#' @param probs numeric vector of probabilities with values in (0,1)
#' specifying the survival quantiles to return
#' @param statistic string defining the statistics to present in the table.
#' Default is `"{estimate} ({conf.low}, {conf.high})"`
#' @param label List of formulas specifying variables labels,
#' e.g. `list(age ~ "Age, yrs", stage ~ "Path T Stage")`, or a string for a
#' single variable table.
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
#' @param y outcome call, e.g. `y = Surv(ttdeath, death)`
#' @param include Variable to include as stratifying variables.
#' @param failure DEPRECATED. Use `reverse=` instead.
#' @param ... Not used
#'
#' @export
#' @rdname tbl_survfit
#' @author Daniel D. Sjoberg
#' @examples
#' library(survival)
#'
#' # Example 1 ----------------------------------
#' # Pass single survfit() object
#' tbl_survfit_ex1 <- tbl_survfit(
#'   survfit(Surv(ttdeath, death) ~ trt, trial),
#'   times = c(12, 24),
#'   label_header = "**{time} Month**"
#' )
#'
#' # Example 2 ----------------------------------
#' # Pass a data frame
#' tbl_survfit_ex2 <- tbl_survfit(
#'   trial,
#'   y = survival::Surv(ttdeath, death),
#'   include = c(trt, grade),
#'   probs = 0.5,
#'   label_header = "**Median Survival**"
#' )
#'
#' # Example 3 ----------------------------------
#' # Pass a list of survfit() objects
#' tbl_survfit_ex3 <-
#'   list(survfit(Surv(ttdeath, death) ~ 1, trial),
#'        survfit(Surv(ttdeath, death) ~ trt, trial)) %>%
#'   tbl_survfit(times = c(12, 24))
#'
#' # Example 4 Competing Events Example ---------
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
#' survfit_cr_ex4 <-
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
#'
#' \if{html}{Example 4}
#'
#' \if{html}{\figure{survfit_cr_ex4.png}{options: width=27\%}}
tbl_survfit <- function(x, ...) {
  UseMethod("tbl_survfit")
}

#' @export
#' @rdname tbl_survfit
tbl_survfit.survfit <- function(x, times = NULL, probs = NULL,
                                statistic = "{estimate} ({conf.low}, {conf.high})",
                                label = NULL, label_header = NULL, estimate_fun = NULL,
                                missing = "\U2014", conf.level = 0.95, reverse = FALSE,
                                failure = NULL, ...) {
  # converting inputs to be compatible with the list method
  x <- list(x)
  if (rlang::is_string(label)) label <- expr(everything() ~ !!label) %>% eval()

  # passing all args to the list method of `tbl_survfit()`
  expr(tbl_survfit.list(!!!as.list(environment()))) %>% eval()
}

#' @export
#' @rdname tbl_survfit
tbl_survfit.data.frame <- function(x, y, times = NULL, probs = NULL,
                                   statistic = "{estimate} ({conf.low}, {conf.high})",
                                   label = NULL, label_header = NULL, estimate_fun = NULL,
                                   missing = "\U2014", conf.level = 0.95, reverse = FALSE,
                                   failure = NULL, include = everything(), ...) {
  include <- dplyr::select(x, {{include}}) %>% names()

  # checking inputs ------------------------------------------------------------
  # able to construct Surv() object?
  y <- enexpr(y)

  y_surv <-
    tryCatch(
      expr(with(!!x, !!y)) %>% eval(),
      error = function(e) {
        paste("There was are error constructing the `Surv()` object from the",
              "data frame passed in `x=`, and the outcome passed in `y=`.",
              "All columns in `y=` should appear in `x=`.\n\n") %>%
          stringr::str_wrap() %>%
          c(as.character(e)) %>%
          stop(call. = FALSE)
      }
    )

  # Surv object is indeed a of class Surv
  if (!inherits(y_surv, "Surv")) {
    paste("Together, the data frame in `x=`, and the survival outcome in `y=`",
          "must construct `Surv` oject, e.g. `with(trial, Surv(ttdeath, death))`") %>%
      stringr::str_wrap() %>%
      stop(call. = FALSE)
  }

  # getting list of all covariates ---------------------------------------------
  y_vars <- as.list(y)[-1] %>% map(deparse) %>% unlist()
  x_vars <- include %>% setdiff(y_vars)

  # construct list of survfits -------------------------------------------------
  survfit_expr_list <-
    tryCatch(
      map(
        x_vars,
        function(.x) expr(survival::survfit(!!y ~ !!sym(.x), data = !!x)) %>% eval()
      ),
      error = function(e) {
        paste("There was are error constructing the list `survfit()` objects from the",
              "data frame passed in `x=`, and the outcome passed in `y=`.") %>%
          stringr::str_wrap() %>%
          stop(call. = FALSE)
      }
    )

  # passing all args to the list method of `tbl_survfit()` ---------------------
  tbl_survfit_args <-
    as.list(environment())[!names(as.list(environment())) %in% c("x", "y", "y_surv", "y_vars",
                                                                 "x_vars", "survfit_expr_list",
                                                                 "include")]

  expr(tbl_survfit.list(x = survfit_expr_list, !!!tbl_survfit_args)) %>% eval()
}

#' @export
#' @rdname tbl_survfit
tbl_survfit.list <- function(x, times = NULL, probs = NULL,
                             statistic = "{estimate} ({conf.low}, {conf.high})",
                             label = NULL, label_header = NULL, estimate_fun = NULL,
                             missing = "\U2014", conf.level = 0.95, reverse = FALSE,
                             failure = NULL, ...) {
  # deprecation notes ----------------------------------------------------------
    if (!is.null(failure)) {
      lifecycle::deprecate_warn(
        "1.3.1", "gtsummary::tbl_survfit(failure = )", "tbl_survfit(reverse = )")
      reverse <- failure
      rm(failure)
    }

  # setting defaults -----------------------------------------------------------
  statistic <-
    statistic %||%
    get_theme_element("tbl_survfit-arg:statistic") %||%
    "{estimate} ({conf.low}, {conf.high})"

  # input checks ---------------------------------------------------------------
  if (purrr::every(x, ~!inherits(.x, "survfit"))) {
    stop("Argument `x=` must be class 'survfit' created from the `survival::survfit()` function.",
         call. = FALSE)
  }

  if (c(is.null(times), is.null(probs)) %>% sum() != 1) {
    stop("One and only one of `times=` and `probs=` must be specified.", call. = FALSE)
  }
  if (!rlang::is_string(statistic) || !rlang::is_string(label_header %||% "")) {
    stop("`statistic=` and `label_header=` arguments must be strings of length one.",
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
      probs = getOption("gtsummary.tbl_survfit.probs.estimate_fun") %||%
        partial(style_sigfig, digits = 2),
      times = getOption("gtsummary.tbl_survfit.times.estimate_fun") %||%
        partial(style_percent, symbol = TRUE)
    )

  # will return call, and all object passed to in tbl_summary call -------------
  # the object func_inputs is a list of every object passed to the function
  tbl_survfit_inputs <- as.list(environment())

  meta_data <-
    tibble(
      survfit = x,
      stratified = map_lgl(.data$survfit, is_survfit_stratified),
      variable = survfit_to_var(.data$survfit, .data$stratified)
    )

  # apply labels
  label <- tidyselect_to_list(
    .data = vctr_2_tibble(unique(meta_data$variable)),
    x = label, arg_name = "label"
  )
  meta_data <-
    meta_data %>%
    mutate(
      var_label = survfit_to_label(.data$survfit, .data$variable, .data$stratified, label)
    )

  meta_data <-
    meta_to_df_stats(meta_data, inputs = tbl_survfit_inputs,
                     estimate_type = estimate_type, estimate_fun = estimate_fun,
                     missing = missing, statistic = statistic)


  # table_header ---------------------------------------------------------------
  table_body <- map_dfr(meta_data$table_body, ~.x)
  table_header <-
    tibble(column = names(table_body)) %>%
    table_header_fill_missing()

  # finishing up ---------------------------------------------------------------
  # constructing final result
  results <- list(
    table_body = table_body,
    table_header = table_header,
    meta_data = meta_data,
    inputs = tbl_survfit_inputs,
    call_list = list(tbl_survfit = match.call())
  )

  # applying labels
  lbls <- as.list(unique(meta_data$df_stats[[1]]$col_label)) %>% set_names(unique(meta_data$df_stats[[1]]$col_name))
  results <-
    expr(modify_header_internal(results, label = !!paste0("**", translate_text("Characteristic"), "**"), !!!lbls)) %>%
    eval()

  # assigning class
  class(results) <- c("tbl_survfit", "gtsummary")

  results
}

# function that uses meta_data and inputs to finish tbl ----------------------
meta_to_df_stats <- function(meta_data, inputs, estimate_type, estimate_fun,
                             missing, statistic) {
  meta_data %>%
    mutate(
      df_stats = map(
        # calculating estimates ------------------------------------------------------
        .data$survfit,
        ~ switch(
          estimate_type,
          "times" = survfit_time(.x, times = inputs$times,
                                 label_header = inputs$label_header,
                                 conf.level = inputs$conf.level,
                                 reverse = inputs$reverse),
          "probs" = survfit_prob(.x, probs = inputs$probs,
                                 label_header = inputs$label_header,
                                 conf.level = inputs$conf.level)
        )
      ),
      # table_body -----------------------------------------------------------------
      table_body = map2(
        .data$df_stats, .data$var_label,
        function(df_stats, var_label) {
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
                     label = var_label) %>%
              bind_rows(table_body)
          }
          table_body
        }
      )
    )
}

# calculates and prepares survival quantile estimates for tbl
survfit_prob <- function(x, probs, label_header, conf.level) {

  strata <- intersect("strata", names(broom::tidy(x, conf.level = conf.level))) %>%
    list() %>% compact()

  # calculating survival quantiles, and adding estimates to pretty tbl
  df_stat <- purrr::map2_dfr(
    probs, seq_along(probs),
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

survfit_to_var <- function(survfit_list, stratified) {
  purrr::pmap_chr(
    list(survfit_list, seq_along(survfit_list), stratified),
    function(x, i, stratified) {
      # if not stratified, return var name ..overall..
      if (stratified == FALSE) {
        return(
          ifelse(length(survfit_list) == 1, "..overall..", glue("..overall_{i}.."))
        )
      }
      # returning variable name
      x$call %>% as.list() %>% pluck("formula") %>% eval() %>% stats::terms() %>%
        attr("variables") %>% as.list() %>% {.[-c(1, 2)]} %>% map_chr(deparse)
    }
  )
}

survfit_to_label <- function(survfit_list, varlist, stratified, label) {
  purrr::pmap_chr(
    list(survfit_list, varlist, seq_along(survfit_list), stratified),
    function(x, v, i, stratified) {
      if (!is.null(label[[v]])) return(label[[v]])
      if (stratified == FALSE) return("Overall")

      # try to extra label from data (if exists)
      data <- x$call %>% as.list() %>% pluck("data")
      label <- NULL
      if (!is.null(data)) {
        label <-  tryCatch(rlang::eval_tidy(data)[[v]] %>% attr("label"),
                           warning = function(w) NULL,
                           error = function(e) NULL)
      }

      # if couldn't get label from data, set variable name as label
      label %||% v
    }
  )
}

is_survfit_stratified <- function(x) {
  var <-
    x$call %>% as.list() %>% pluck("formula") %>% eval() %>% stats::terms() %>%
    attr("variables") %>% as.list() %>% {.[-c(1, 2)]} %>% map_chr(deparse)
  if (length(var) == 1) return(TRUE)
  if (length(var) == 0) return(FALSE)

  stop("`tbl_survfit()` supports a single stratifying variable on the RHS of `formula=`",
       call. = FALSE)
}
