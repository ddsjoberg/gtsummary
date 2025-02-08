#' Survival table
#'
#' @description
#' Function takes a `survfit` object as an argument, and provides a
#' formatted summary table of the results.
#'
#' No more than one stratifying variable is allowed in each model.
#' If you're experiencing unexpected errors using `tbl_survfit()`,
#' please review [?tbl_survfit_errors][tbl_survfit_errors] for a possible explanation.
#'
#' @param x (`survfit`, `list`, `data.frame`)\cr
#'   a survfit object, list of survfit objects, or a data frame.
#'   If a data frame is passed, a list of survfit objects is constructed using
#'   each variable as a stratifying variable.
#' @param times (`numeric`)\cr
#'   a vector of times for which to return survival probabilities.
#' @param probs (`numeric`)\cr
#'   a vector of probabilities with values in (0,1) specifying the survival quantiles to return.
#' @param statistic (`string`)\cr
#'   string defining the statistics to present in the table.
#'   Default is `"{estimate} ({conf.low}, {conf.high})"`
#' @param label ([`formula-list-selector`][syntax])\cr
#'   List of formulas specifying variables labels,
#'   e.g. `list(age = "Age, yrs", stage = "Path T Stage")`, or a string for a
#'   single variable table.
#' @param label_header (`string`)\cr
#'   string specifying column labels above statistics. Default
#'   is `"{prob} Percentile"` for survival percentiles, and `"Time {time}"` for n-year
#'   survival estimates
#' @param estimate_fun (`function`)\cr
#'   function to format the Kaplan-Meier estimates. Default
#'   is [`label_style_percent()`] for survival probabilities and [`label_style_sigfig()`] for
#'   survival times
#' @param missing (`string`)\cr
#'   text to fill when estimate is not estimable. Default is `"--"`
#' @param conf.level (scalar `numeric`)\cr ]
#'   Confidence level for confidence intervals. Default is `0.95`
#' @param type (`string` or `NULL`)\cr
#'   type of statistic to report. Available for Kaplan-Meier time estimates only, otherwise `type`
#'   is ignored. Default is `NULL`.
#'   Must be one of the following:
#'   ```{r, echo = FALSE}
#'   dplyr::tribble(
#'     ~type,          ~transformation,
#'     '`"survival"`', '`x`',
#'     '`"risk"`',     '`1 - x`',
#'     '`"cumhaz"`',   '`-log(x)`',
#'   ) %>%
#'   knitr::kable()
#'   ```
#' @param reverse `r lifecycle::badge("deprecated")`
#' @param y outcome call, e.g. `y = Surv(ttdeath, death)`
#' @param include Variable to include as stratifying variables.
#' @param ... For [`tbl_survfit.data.frame()`]  and [`tbl_survfit.survfit()`] the arguments
#' are passed to [tbl_survfit.list()]. They are not used when [tbl_survfit.list()]
#' is called directly.
#' @inheritParams add_global_p
#'
#' @export
#' @name tbl_survfit
#'
#' @section Formula Specification:
#' When passing a [`survival::survfit()`] object to `tbl_survfit()`,
#' the `survfit()` call must use an evaluated formula and not a stored formula.
#' Including a proper formula in the call allows the function to accurately
#' identify all variables included in the estimation. See below for examples:
#'
#' ```r
#' library(gtsummary)
#' library(survival)
#'
#' # include formula in `survfit()` call
#' survfit(Surv(time, status) ~ sex, lung) |> tbl_survfit(times = 500)
#'
#' # you can also pass a data frame to `tbl_survfit()` as well.
#' lung |>
#'   tbl_survfit(y = Surv(time, status), include = "sex", times = 500)
#' ```
#' You **cannot**, however, pass a stored formula, e.g. `survfit(my_formula, lung)`,
#' but you can use stored formulas with `rlang::inject(survfit(!!my_formula, lung))`.
#'
#' @author Daniel D. Sjoberg
#' @examplesIf gtsummary:::is_pkg_installed("survival")
#' library(survival)
#'
#' # Example 1 ----------------------------------
#' # Pass single survfit() object
#' tbl_survfit(
#'   survfit(Surv(ttdeath, death) ~ trt, trial),
#'   times = c(12, 24),
#'   label_header = "**{time} Month**"
#' )
#'
#' # Example 2 ----------------------------------
#' # Pass a data frame
#' tbl_survfit(
#'   trial,
#'   y = "Surv(ttdeath, death)",
#'   include = c(trt, grade),
#'   probs = 0.5,
#'   label_header = "**Median Survival**"
#' )
#'
#' # Example 3 ----------------------------------
#' # Pass a list of survfit() objects
#' list(survfit(Surv(ttdeath, death) ~ 1, trial),
#'      survfit(Surv(ttdeath, death) ~ trt, trial)) |>
#'   tbl_survfit(times = c(12, 24))
#'
#' # Example 4 Competing Events Example ---------
#' # adding a competing event for death (cancer vs other causes)
#' set.seed(1123)
#' library(dplyr, warn.conflicts = FALSE, quietly = TRUE)
#' trial2 <- trial |>
#'   dplyr::mutate(
#'     death_cr =
#'       dplyr::case_when(
#'         death == 0 ~ "censor",
#'         runif(n()) < 0.5 ~ "death from cancer",
#'         TRUE ~ "death other causes"
#'       ) |>
#'       factor()
#'   )
#'
#' survfit(Surv(ttdeath, death_cr) ~ grade, data = trial2) |>
#'   tbl_survfit(times = c(12, 24), label = "Tumor Grade")
NULL

#' @export
#' @rdname tbl_survfit
tbl_survfit <- function(x, ...) {
  UseMethod("tbl_survfit", x)
}

#' @export
#' @rdname tbl_survfit
tbl_survfit.survfit <- function(x, ...) {
  set_cli_abort_call()

  tbl_survfit.list(x = list(x), ...)
}

#' @export
#' @rdname tbl_survfit
tbl_survfit.data.frame <- function(x, y, include = everything(), conf.level = 0.95, ...) {
  set_cli_abort_call()
  check_pkg_installed("survival", ref = "cardx")

  # process inputs -------------------------------------------------------------
  check_scalar_range(conf.level, range = c(0, 1))
  # convert to a string, in case it wasn't passed this way originally
  y <- .process_x_and_y_args_as_string(x, enquo(y))
  cards::process_selectors(x, include = {{ include }})
  # remove any variables specified in arguments `y` from include
  include <- include |>
    setdiff(tryCatch(stats::reformulate(y) |> all.vars(), error = \(e) character()))

  if (is_empty(include)) {
    cli::cli_abort(
      "No variables were selected in the {.arg include} argument.",
      call = get_cli_abort_call()
    )
  }

  # build survfit models -------------------------------------------------------
  lst_survfits <-
    lapply(
      include,
      function(variable) {
        cardx::construct_model(
          data = x,
          formula = stats::reformulate(termlabels = cardx::bt(variable), response = y),
          method = "survfit",
          package = "survival",
          method.args = list(conf.int = conf.level)
        )
      }
    ) |>
    set_names(include)

  # pass the list of survfit objects to create the final table -----------------
  tbl_survfit.list(x = lst_survfits, ...)
}

#' @export
#' @rdname tbl_survfit
tbl_survfit.list <- function(x,
                             times = NULL,
                             probs = NULL,
                             statistic = "{estimate} ({conf.low}, {conf.high})",
                             label = NULL,
                             label_header = ifelse(!is.null(times), "**Time {time}**", "**{style_sigfig(prob, scale=100)}% Percentile**"),
                             estimate_fun = ifelse(!is.null(times), label_style_percent(suffix = "%"), label_style_sigfig()),
                             missing = "--",
                             type = NULL,
                             reverse = FALSE,
                             quiet = TRUE, ...) {
  set_cli_abort_call()
  check_dots_empty()

  # deprecation ----------------------------------------------------------------
  if (!missing(quiet)) {
    lifecycle::deprecate_warn(
      when = "2.0.0",
      what = "gtsummary::tbl_survfit(quiet)"
    )
  }
  if (isTRUE(reverse)) {
    lifecycle::deprecate_warn(
      when = "2.0.0",
      what = "gtsummary::tbl_survfit(reverse)",
      details = "Please use `type='risk'` instead."
    )
    type = "risk"
  }

  # check inputs ---------------------------------------------------------------
  check_pkg_installed("survival")
  check_class(x, "list")
  cards::check_list_elements(
    x,
    predicate = \(x) inherits(x, "survfit"),
    error_msg = "The values passed in the {.cls list} from argument {.arg x} must be class {.cls survfit}."
  )
  check_class(times, c("numeric", "integer"), allow_empty = TRUE)
  check_class(probs, "numeric", allow_empty = TRUE)
  if (is_empty(times) + is_empty(probs) != 1L) {
    cli::cli_abort(
      "Specify one and only one of arguments {.arg times} and {.arg probs}.",
      call = get_cli_abort_call()
    )
  }
  if (missing(statistic)) {
    statistic <-
      get_theme_element(
        "tbl_survfit-arg:statistic",
        default =
          paste0("{estimate} ({conf.low}", get_theme_element("pkgwide-str:ci.sep", default = ", "), "{conf.high})")
      )
  }
  check_string(statistic)
  if (is_string(label)) label <- inject(everything() ~ !!label)
  if (missing(label_header)) {
    label_header <- ifelse(
      !is.null(times),
      translate_string("Time {time}"),
      translate_string("{style_sigfig(prob, scale=100)}% Percentile")
    ) %>%
      paste0("**", ., "**")
  }
  check_string(label_header)
  estimate_fun <- as_function(estimate_fun)
  missing <- ifelse(missing(missing), "\U2014", check_string(missing))
  if (!is_empty(type)) type <- arg_match(type, values = c("survival", "risk", "cumhaz"))

  tbl_survfit_inputs <- as.list(environment())

  label <-
    case_switch(
      is_empty(label) ~ .default_survfit_labels(x),
      is.list(label) ~ append(.default_survfit_labels(x), label),
      is_formula(label) ~ append(.default_survfit_labels(x), list(label)),
      .default = label
    )



  # calculate cards objects ----------------------------------------------------
  cards <-
    lapply(
      x,
      \(x) {
        cardx::ard_survival_survfit(x, times = times, probs = probs, type = type)  |>
          cards::replace_null_statistic() |>
          dplyr::mutate(
            fmt_fn =
              pmap(
                list(.data$fmt_fn, .data$stat_name, .data$stat),
                \(fmt_fn, stat_name, stat) {
                  if (stat_name %in% c("estimate", "conf.low", "conf.high") && !is.na(stat)) return(estimate_fun)
                  else if (stat_name %in% c("estimate", "conf.low", "conf.high") && is.na(stat)) return(\(x, ...) missing)
                  else return(fmt_fn)
                }
              ),
            gts_column =
              case_switch(
                !is_empty(times) ~ dplyr::recode(unlist(variable_level), !!!set_names(paste0("stat_", seq_along(times)), times)),
                !is_empty(probs) ~ dplyr::recode(unlist(variable_level), !!!set_names(paste0("stat_", seq_along(probs)), probs))
              )
          )
      }
    )

  res <- brdg_survfit(
    cards = cards,
    statistic = statistic,
    label = label,
    label_header = label_header
  ) |>
    structure(class = c("tbl_survfit", "gtsummary"))

  res$call_list <- list(tbl_survfit = match.call())
  names(res$cards) <- "tbl_survfit"
  res$inputs <- tbl_survfit_inputs
  names(res$inputs$x) <- names(res$cards$tbl_survfit)

  res
}

brdg_survfit <- function(cards,
                         statistic = "{estimate} ({conf.low}, {conf.high})",
                         label = NULL,
                         label_header) {
  set_cli_abort_call()
  # grab information for the headers -------------------------------------------
  df_header_survfit <- cards[[1]] |>
    dplyr::filter(!.data$context %in% "attributes") |>
    dplyr::distinct(.data$variable, .data$variable_level, .data$gts_column)

  # assign a variable name to the cards list -----------------------------------
  univariate_survift_count <- 0L
  cards_names <- vector(mode = "list", length = length(cards))
  for (i in seq_along(cards)) {
    # extract stratifying variable names as vector
    cards_names[i] <- cards[[i]] |> dplyr::select(cards::all_ard_groups("names")) |> dplyr::slice(1L) |> unlist() |> list()
    # if univariate, assign variable Overall
    if (is_empty(cards_names[[i]])) {
      univariate_survift_count <- univariate_survift_count + 1L
      cards_names[[i]] <- paste0("..overall_", univariate_survift_count, "..")
    }

    # check if there are more than one stratifying variable
    if (length(cards_names[[i]]) > 1L) {
      cli::cli_abort(
        c("The {.fun tbl_survfit} function supports {.fun survival::survfit} objects with no more than one stratifying variable.",
          i = "The model is stratified by {.val {cards_names[[i]]}}."),
        call = get_cli_abort_call()
      )
    }
  }
  names(cards) <- unlist(cards_names)
  if (any(duplicated(names(cards)))) {
    cli::cli_inform(
      c("The {.cls survfit} objects are not uniquely identified by the stratifying variable names.",
        i = "This could cause issues in subsequent calls, such as, {.code tbl_survfit() |> add_p()}")
    )
  }

  # process the label argument -------------------------------------------------
  cards::process_formula_selectors(
    data = vec_to_df(names(cards)),
    label = label
  )
  cards::fill_formula_selectors(
    data = vec_to_df(names(cards)),
    label =
      as.list(names(cards)) |>
      set_names(names(cards)) |>
      utils::modifyList(
        val = rep_named(paste0("..overall_", seq_along(cards), ".."), list(translate_string("Overall")))
      )
  )

  # add attributes ARD to the cards data frame ---------------------------------
  for (i in seq_along(cards)) {
    if (nrow(dplyr::filter(cards[[i]], .data$context %in% "attributes")) == 0L) {
      cards[[i]] <- cards[[i]] |>
        dplyr::bind_rows(
          dplyr::tibble(
            variable = cards_names[[i]],
            context = "attributes",
            stat_name = "label",
            stat_label = "Variable Label",
            stat = label[cards_names[[i]]]
          )
        )
    }
  }

  # convert cards data frame to format for gtsummary table_body ----------------
  table_body <- imap(
    cards,
    function(x, variable) {
      # merge in gts_column
      x <- x |>
        dplyr::mutate(variable = .env$variable)

      # no stratifying variable, process as a continuous tbl_summary() variable
      if (dplyr::select(x, cards::all_ard_groups()) |> names() |> is_empty()) {
        pier <- pier_summary_continuous(
          cards = x,
          variables = variable,
          statistic = list(statistic) |> set_names(variable)
        )
      }
      else {
        pier <- pier_summary_categorical(
          cards = x |>
            dplyr::mutate(
              variable = .env$variable,
              variable_level = .data$group1_level
            ) |>
            dplyr::select(-cards::all_ard_groups()),
          variables = variable,
          statistic = list(statistic) |> set_names(variable)
        )
      }

    }
  ) |>
    dplyr::bind_rows()

  # construct gtsummary object -------------------------------------------------
  res <- .create_gtsummary_object(table_body, cards = list(brdg_survfit = cards))

  # add 'df_header_survfit' info to table_styling$header
  res$table_styling$header <-
    res$table_styling$header |>
    dplyr::left_join(
      df_header_survfit  |>
        dplyr::mutate(across(where(is.list), unlist)) %>%
        dplyr::rename(column = "gts_column",  "modify_stat_{.$variable[1]}" := "variable_level") |>
        dplyr::select(-"variable"),
      by = "column"
    )

  res |>
    # add header to label column and add default indentation
    modify_table_styling(
      columns = "label",
      label = glue("**{translate_string('Characteristic')}**"),
      rows = .data$row_type %in% c("level", "missing"),
      indent = 4L
    ) |>
    modify_header(all_stat_cols() ~ label_header) |>
    structure(class = c("card_survfit", "gtsummary"))
}


.default_survfit_labels <- function(x) {
  label <- list()
  for (i in seq_along(x)) {
    variable_i <-
      tryCatch(
        x[[i]]$call$formula |> rlang::f_rhs() |> all.vars() |> dplyr::first() |> discard(is.na),
        error = \(e) character(0)
      )
    if (!is_empty(variable_i)) {
      label[[variable_i]] <-
        tryCatch(
          eval(x[[i]]$call$data)[[variable_i]] |> attr("label"),
          error = \(e) variable_i # styler: off
        )
    }
  }

  compact(label)
}
