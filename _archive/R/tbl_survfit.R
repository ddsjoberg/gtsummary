#' Creates table of survival probabilities
#'
#' \lifecycle{maturing}
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
#' is [style_percent()] for survival probabilities and [style_sigfig] for
#' survival times
#' @param missing text to fill when estimate is not estimable. Default is `"--"`
#' @param conf.level Confidence level for confidence intervals. Default is 0.95
#' @param reverse Flip the probability reported, i.e. `1 - estimate`.
#' Default is `FALSE`.  Does not apply to survival quantile requests
#' @param y outcome call, e.g. `y = Surv(ttdeath, death)`
#' @param include Variable to include as stratifying variables.
#' @param ... For [tbl_survfit.data.frame()]  and [tbl_survfit.survfit()] the arguments
#' are passed to [tbl_survfit.list()]. They are not used when [tbl_survfit.list()]
#' is called directly.
#' @inheritParams add_global_p
#' @export
#' @rdname tbl_survfit
#' @family tbl_survfit tools
#' @seealso Review [list, formula, and selector syntax][syntax] used throughout gtsummary
#' @author Daniel D. Sjoberg
#' @examplesIf broom.helpers::.assert_package("survival", pkg_search = "gtsummary", boolean = TRUE)
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
#'   y = Surv(ttdeath, death),
#'   include = c(trt, grade),
#'   probs = 0.5,
#'   label_header = "**Median Survival**"
#' )
#'
#' # Example 3 ----------------------------------
#' # Pass a list of survfit() objects
#' tbl_survfit_ex3 <-
#'   list(
#'     survfit(Surv(ttdeath, death) ~ 1, trial),
#'     survfit(Surv(ttdeath, death) ~ trt, trial)
#'   ) %>%
#'   tbl_survfit(times = c(12, 24))
#'
#' # Example 4 Competing Events Example ---------
#' # adding a competing event for death (cancer vs other causes)
#' set.seed(1123)
#' library(dplyr, warn.conflicts = FALSE, quietly = TRUE)
#' trial2 <- trial %>%
#'   mutate(
#'     death_cr = case_when(
#'       death == 0 ~ "censor",
#'       runif(n()) < 0.5 ~ "death from cancer",
#'       TRUE ~ "death other causes"
#'     ) %>% factor()
#'   )
#'
#' survfit_cr_ex4 <-
#'   survfit(Surv(ttdeath, death_cr) ~ grade, data = trial2) %>%
#'   tbl_survfit(times = c(12, 24), label = "Tumor Grade")
#' @section Example Output:
#' \if{html}{Example 1}
#'
#' \if{html}{\out{
#' `r man_create_image_tag(file = "tbl_survfit_ex1.png", width = "55")`
#' }}
#'
#' \if{html}{Example 2}
#'
#' \if{html}{\out{
#' `r man_create_image_tag(file = "tbl_survfit_ex2.png", width = "45")`
#' }}
#'
#' \if{html}{Example 3}
#'
#' \if{html}{\out{
#' `r man_create_image_tag(file = "tbl_survfit_ex3.png", width = "55")`
#' }}
#'
#' \if{html}{Example 4}
#'
#' \if{html}{\out{
#' `r man_create_image_tag(file = "survfit_cr_ex4.png", width = "55")`
#' }}
tbl_survfit <- function(x, ...) {
  UseMethod("tbl_survfit", x)
}

#' @export
#' @rdname tbl_survfit
tbl_survfit.list <- function(x, times = NULL, probs = NULL,
                             statistic = NULL, label = NULL, label_header = NULL,
                             estimate_fun = NULL, missing = NULL,
                             conf.level = 0.95, reverse = FALSE, quiet = NULL, ...) {
  check_dots_empty(error = function(e) inform(c(e$message, e$body)))
  # setting defaults -----------------------------------------------------------
  ci.sep <- get_theme_element("pkgwide-str:ci.sep", default = ", ")
  statistic <-
    statistic %||%
    get_theme_element("tbl_survfit-arg:statistic") %||%
    paste0("{estimate} ({conf.low}", ci.sep, "{conf.high})")
  missing <- missing %||% "\U2014"


  quiet <- quiet %||% get_theme_element("pkgwide-lgl:quiet") %||% FALSE

  # input checks ---------------------------------------------------------------
  if (rlang::is_string(label)) {
    label <- inject(everything() ~ !!label)
  }
  assert_package("survival", "tbl_survfit()")
  if (purrr::every(x, ~ !inherits(.x, "survfit"))) {
    stop("Argument `x=` must be class 'survfit' created from the `survival::survfit()` function.",
      call. = FALSE
    )
  }

  if (c(is.null(times), is.null(probs)) %>% sum() != 1) {
    stop("One and only one of `times=` and `probs=` must be specified.", call. = FALSE)
  }
  if (!rlang::is_string(statistic) || !rlang::is_string(label_header %||% "")) {
    stop("`statistic=` and `label_header=` arguments must be strings of length one.",
      call. = FALSE
    )
  }
  if (reverse == TRUE && !is.null(probs)) {
    rlang::inform("`reverse=TRUE` argument ignored for survival quantile estimation.")
  }

  # setting defaults -----------------------------------------------------------
  estimate_type <- ifelse(is.null(times), "probs", "times")
  estimate_fun <-
    estimate_fun %||%
    switch(estimate_type,
      probs = .get_deprecated_option("gtsummary.tbl_survfit.probs.estimate_fun") %||%
        function(x) style_sigfig(x, digits = 2),
      times = .get_deprecated_option("gtsummary.tbl_survfit.times.estimate_fun") %||%
        function(x) style_percent(x, symbol = TRUE)
    ) %>%
    gts_mapper("tbl_survfit(estimate_fun=)")

  # will return call, and all object passed to in tbl_summary call -------------
  # the object func_inputs is a list of every object passed to the function
  tbl_survfit_inputs <- as.list(environment())

  meta_data <-
    tibble(
      survfit = x,
      tidy = map(.data$survfit, ~ broom::tidy(.x)),
      stratified = map_lgl(.data$tidy, ~ "strata" %in% names(.x)),
      variable = survfit_to_var(.data$survfit, .data$stratified, .data$tidy, quiet)
    )

  # apply labels
  label <-
    .formula_list_to_named_list(
      x = label,
      var_info = meta_data_to_var_info(meta_data),
      arg_name = "label",
      type_check = chuck(type_check, "is_string", "fn"),
      type_check_msg = chuck(type_check, "is_string", "msg")
    )

  meta_data <-
    meta_data %>%
    mutate(
      var_label = survfit_to_label(.data$survfit, .data$variable, .data$stratified, label),
      var_type = map_chr(.data$stratified, ~ ifelse(.x, "categorical", "continuous"))
    )

  meta_data <-
    meta_to_df_stats(meta_data,
      inputs = tbl_survfit_inputs,
      estimate_type = estimate_type, estimate_fun = estimate_fun,
      missing = missing, statistic = statistic
    )

  # checking that the variable list is unique
  if (quiet == FALSE && !identical(meta_data$variable, unique(meta_data$variable))) {
    paste(
      "tbl_survfit: The variable list in `.$meta_data` is not unique.",
      "This could pose a problem with  subsequent `tbl_survfit` calls,",
      "e.g. `tbl_survfit() %>% add_p()`"
    ) %>%
      stringr::str_wrap() %>%
      rlang::inform()
  }

  # table_body -----------------------------------------------------------------
  table_body <-
    meta_data %>%
    select("var_type", "var_label", "table_body") %>%
    unnest("table_body") %>%
    select("variable", "var_label", everything())

  # finishing up ---------------------------------------------------------------
  # constructing final result
  x <-
    .create_gtsummary_object(
      table_body = table_body,
      meta_data = meta_data,
      inputs = tbl_survfit_inputs
    )

  # applying labels
  lbls <-
    meta_data$df_stats[[1]] %>%
    select("col_name", "col_label") %>%
    distinct() %>%
    tibble::deframe() %>%
    as.list()

  x <-
    expr(modify_header(x, label = !!paste0("**", translate_text("Characteristic"), "**"), !!!lbls)) %>%
    eval()

  # exporting results ----------------------------------------------------------
  x$call_list <- list(tbl_survfit = match.call())
  class(x) <- c("tbl_survfit", "gtsummary")

  x
}

#' @export
#' @rdname tbl_survfit
tbl_survfit.survfit <- function(x, ...) {
  # passing all args to the list method of `tbl_survfit()`
  tbl_survfit.list(list(x), ...)
}

#' @export
#' @rdname tbl_survfit
tbl_survfit.data.frame <- function(x, y, include = everything(), ...) {
  include <- dplyr::select(x, {{ include }}) %>% names()

  # checking inputs ------------------------------------------------------------
  check_haven_labelled(x, include)

  # able to construct Surv() object?
  y <- enexpr(y)

  y_surv <-
    tryCatch(
      expr(with(!!x, !!y)) %>% eval(),
      error = function(e) {
        paste(
          "There was are error constructing the `y = Surv()` object from the",
          "data frame passed in `x=`, and the outcome passed in `y=`.",
          "All columns in `y=` should appear in `x=`.\n\n"
        ) %>%
          stringr::str_wrap() %>%
          c(as.character(e)) %>%
          stop(call. = FALSE)
      }
    )

  # Surv object is indeed a of class Surv
  if (!inherits(y_surv, "Surv")) {
    paste(
      "Together, the data frame in `x=`, and the survival outcome in `y=`",
      "must construct `Surv` object, e.g. `with(trial, Surv(ttdeath, death))`"
    ) %>%
      stringr::str_wrap() %>%
      stop(call. = FALSE)
  }

  # getting list of all covariates ---------------------------------------------
  y_vars <- inject(all.vars(~ !!y))
  x_vars <- include %>% setdiff(y_vars)

  # construct list of survfits -------------------------------------------------
  survfit_expr_list <-
    tryCatch(
      map(
        x_vars,
        function(.x) expr(survival::survfit(!!y ~ !!sym(.x), data = !!x)) %>% eval()
      ),
      error = function(e) {
        paste(
          "There was are error constructing the list `survfit()` objects from the",
          "data frame passed in `x=`, and the outcome passed in `y=`."
        ) %>%
          stringr::str_wrap() %>%
          stop(call. = FALSE)
      }
    )

  tbl_survfit.list(x = survfit_expr_list, ...)
}

# function that uses meta_data and inputs to finish tbl ----------------------
meta_to_df_stats <- function(meta_data, inputs, estimate_type, estimate_fun,
                             missing, statistic) {
  meta_data %>%
    mutate(
      df_stats = pmap(
        # calculating estimates ------------------------------------------------
        list(.data$survfit, .data$variable, .data$tidy, .data$var_label),
        ~ switch(estimate_type,
          "times" = survfit_time(..1,
            variable = ..2, times = inputs$times,
            label_header = inputs$label_header,
            conf.level = inputs$conf.level,
            reverse = inputs$reverse,
            quiet = inputs$quiet, tidy = ..3,
            var_label = ..4,
            estimate_fun = estimate_fun
          ),
          "probs" = survfit_prob(..1,
            variable = ..2, probs = inputs$probs,
            label_header = inputs$label_header,
            conf.level = inputs$conf.level,
            quiet = inputs$quiet, tidy = ..3,
            var_label = ..4,
            estimate_fun = estimate_fun
          )
        )
      ),
      # table_body -------------------------------------------------------------
      table_body = map2(
        .data$df_stats, .data$var_label,
        function(df_stats, var_label) {
          strata <- intersect("strata", names(df_stats)) %>%
            list() %>%
            compact()

          table_body <-
            df_stats %>%
            mutate_at(
              vars("estimate", "conf.low", "conf.high"),
              ~ coalesce(as.character(estimate_fun(.)), missing)
            ) %>%
            mutate(
              statistic = glue(.env$statistic) %>% as.character(),
              row_type = switch(length(strata) == 0,
                "label"
              ) %||% "level"
            ) %>%
            select(c("variable", "row_type", "label", "col_name", "statistic")) %>%
            tidyr::pivot_wider(
              id_cols = c("variable", "row_type", "label"),
              names_from = "col_name",
              values_from = "statistic"
            )

          # adding label row, if needed
          if (nrow(table_body) > 1) {
            table_body <-
              table_body %>%
              select("variable") %>%
              distinct() %>%
              mutate(
                row_type = "label",
                label = var_label
              ) %>%
              bind_rows(table_body)
          }
          table_body
        }
      )
    )
}

# calculates and prepares survival quantile estimates for tbl
survfit_prob <- function(x, variable, probs, label_header, conf.level, quiet,
                         tidy, var_label, estimate_fun) {
  strata <- intersect("strata", names(tidy)) %>%
    list() %>%
    compact()

  # calculating survival quantiles, and adding estimates to pretty tbl
  df_stat <- purrr::map2_dfr(
    probs, seq_along(probs),
    ~ stats::quantile(x, probs = .x) %>%
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
      variable = .env$variable,
      label = switch(length(.env$strata) == 0,
        var_label
      ) %||%
        # take everything to the right of the first '='
        str_sub(strata, str_locate(strata, fixed("="))[1] + 1),
      col_label = .env$label_header %||%
        # for some languages, we show 'Percentile 50%' instead of '50% Percentile'
        switch(get_theme_element("pkgwide-str:language", default = "en") %in% "es",
          "**{style_percent(prob, symbol = TRUE)} {translate_text('Percentile')}**"
        ) %||%
        "**{style_percent(prob, symbol = TRUE)} {translate_text('Percentile')}**" %>%
        glue() %>% as.character()
    )
  if (length(strata) > 0) {
    df_stat$variable_levels <- df_stat$label
  }

  # removing strata column if there are no stratum in survfit
  if (length(strata) == 0) df_stat <- select(df_stat, -"strata")

  # add fmt_fun attribute
  for (column in c("estimate", "conf.low", "conf.high")) {
    attr(df_stat[[column]], "fmt_fun") <- estimate_fun
  }

  df_stat
}

# calculates and prepares n-year survival estimates for tbl
survfit_time <- function(x, variable, times, label_header, conf.level,
                         reverse, quiet, tidy, var_label, estimate_fun) {
  strata <- intersect("strata", names(tidy)) %>%
    list() %>%
    compact()
  multi_state <- inherits(x, "survfitms")
  if (multi_state == TRUE) {
    # selecting state to show
    state <- unique(tidy$state) %>%
      setdiff("(s0)") %>%
      purrr::pluck(1)

    if (identical(quiet, FALSE)) {
      rlang::inform(glue(
        "tbl_survfit: Multi-state model detected. Showing probabilities into state '{state}'"
      ))
    }

    tidy <- dplyr::filter(tidy, .data$state == .env$state)
  }

  # adding time 0 to data frame
  tidy <-
    tidy %>%
    # making strata a fct to preserve ordering
    mutate_at(vars(!!!strata), ~ factor(., levels = unique(.))) %>%
    # if CI is missing, and SE is 0, making the CIs the estimate
    mutate_at(
      vars("conf.high", "conf.low"),
      ~ ifelse(is.na(.) & .data$std.error == 0, .data$estimate, .)
    ) %>%
    select(any_of(c("time", "estimate", "conf.high", "conf.low", "strata"))) %>%
    bind_rows(
      group_by(., !!!syms(strata)) %>%
        slice(1) %>%
        mutate(
          time = 0,
          estimate = ifelse(multi_state, 0, 1),
          conf.low = ifelse(multi_state, 0, 1),
          conf.high = ifelse(multi_state, 0, 1)
        )
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
        distinct() %>%
        mutate(
          time = list(.env$times),
          col_name = list(paste("stat", seq_len(length(.env$times)), sep = "_"))
        ) %>%
        unnest(cols = c("time", "col_name")),
      by = unlist(c(strata, "time"))
    ) %>%
    # if the user-specifed time is unobserved, filling estimates with previous value
    arrange(!!!syms(strata), .data$time) %>%
    group_by(!!!syms(strata)) %>%
    tidyr::fill(
      "estimate", "conf.high", "conf.low", "time_max",
      .direction = "down"
    ) %>%
    ungroup() %>%
    # keeping obs of user-specified times
    filter(!is.na(.data$col_name)) %>%
    # if user-specified time is after the latest follow-up time, making it NA
    mutate_at(
      vars("estimate", "conf.high", "conf.low"),
      ~ ifelse(.data$time > .data$time_max, NA_real_, .)
    ) %>%
    mutate(
      variable = .env$variable,
      label = switch(length(.env$strata) == 0,
        var_label
      ) %||%
        # take everything to the right of the first '='
        str_sub(strata, str_locate(strata, fixed("="))[1] + 1),
      col_label = .env$label_header %||% paste0("**", translate_text("Time"), " {time}**") %>% glue() %>% as.character()
    ) %>%
    select(
      any_of(c("variable", "label", "strata", "col_name", "col_label")),
      everything(), -"time_max"
    )
  if (length(strata) > 0) {
    df_stat$variable_levels <- df_stat$label
  }

  # converting to reverse probs, if requested
  if (reverse == TRUE) {
    df_stat <-
      df_stat %>%
      mutate_at(vars("estimate", "conf.low", "conf.high"), ~ 1 - .) %>%
      dplyr::rename(conf.low = "conf.high", conf.high = "conf.low")
  }

  # add fmt_fun attribute
  for (column in c("estimate", "conf.low", "conf.high")) {
    attr(df_stat[[column]], "fmt_fun") <- estimate_fun
  }

  df_stat
}

survfit_to_var <- function(survfit_list, stratified, tidy, quiet) {
  purrr::pmap_chr(
    list(survfit_list, seq_along(survfit_list), stratified, tidy),
    function(x, i, stratified, tidy) {
      # if not stratified, return var name ..overall..
      if (stratified == FALSE) {
        return(
          ifelse(length(survfit_list) == 1, "..overall..", glue("..overall_{i}.."))
        )
      }
      var <- word(tidy$strata[1], 1, sep = fixed("="))
      if (quiet == FALSE &&
        # if you have more than one equal sign
        stringr::str_count(tidy$strata[1], pattern = fixed("=")) > 1 &&
        # there are more than one "word" with sep = ", ", then you likely have more than one var
        stringr::str_count(tidy$strata[1], pattern = fixed(", ")) >= 1) {
        paste(
          "The `tbl_survfit()` function supports `survfit()` objects with a",
          "single stratifying variable, and it looks like you may have more.",
          "Errors or unexpected output may occur."
        ) %>%
          str_wrap() %>%
          inform()
      }
      # returning variable name
      var
    }
  )
}

survfit_to_label <- function(survfit_list, varlist, stratified, label) {
  purrr::pmap_chr(
    list(survfit_list, varlist, seq_along(survfit_list), stratified),
    function(x, v, i, stratified) {
      if (!is.null(label[[v]])) {
        return(label[[v]])
      }
      if (stratified == FALSE) {
        return(translate_text("Overall"))
      }

      # try to extra label from data (if exists)
      data <- x$call %>%
        as.list() %>%
        pluck("data")
      label <- NULL
      if (!is.null(data)) {
        label <- tryCatch(rlang::eval_tidy(data)[[v]] %>% attr("label"),
          warning = function(w) NULL,
          error = function(e) NULL
        )
      }

      # if couldn't get label from data, set variable name as label
      label %||% v
    }
  )
}

safe_survfit_eval <- function(x) {
  tryCatch(
    eval(x),
    error = function(e) {
      paste(
        "There was an error executing {.code add_n()} or {.code add_p()}.",
        "The error may be a due to the construction of the original",
        "{.code survival::survfit()} object.",
        "Please visit this help file for a possible solution:",
        "{.code ?tbl_survfit_errors}"
      ) %>%
        cli_alert_danger()
      e
    }
  )
}
