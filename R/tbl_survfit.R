#' Survival table
#'
#' Function takes a `survfit` object as an argument, and provides a
#' formatted summary table of the results
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
#'   is [`styfn_percent()`] for survival probabilities and [`styfn_sigfig()`] for
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
#' @param ... For [tbl_survfit.data.frame()]  and [tbl_survfit.survfit()] the arguments
#' are passed to [tbl_survfit.list()]. They are not used when [tbl_survfit.list()]
#' is called directly.
#' @inheritParams add_global_p
#'
#' @export
#' @name tbl_survfit
#'
#' @author Daniel D. Sjoberg
#' @examplesIf gtsummary:::is_pkg_installed("survival", reference_pkg = "gtsummary")
#' library(survival)
NULL

#' @export
#' @rdname tbl_survfit
tbl_survfit <- function(x, ...) {
  UseMethod("tbl_survfit", x)
}

#' @export
#' @rdname tbl_survfit
tbl_survfit.list <- function(x,
                             times = NULL,
                             probs = NULL,
                             statistic = "{estimate} ({conf.low}, {conf.high})",
                             label = NULL,
                             label_header = ifelse(!is.null(times), "Time {time}", "{prob} Percentile"),
                             estimate_fun = ifelse(!is.null(times), styfn_percent(), styfn_sigfig()),
                             missing = "--",
                             conf.level = 0.95,
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
  check_pkg_installed("survival", reference_pkg = "gtsummary")
  check_class(x, "list")
  cards::check_list_elements(
    x,
    predicate = \(x) inherits(x, "survfit"),
    error_msg = "The values passed in the {.cls list} from argument {.arg x} must be class {.cls survfit}."
  )
  check_class(times, "numeric", allow_empty = TRUE)
  check_class(probs, "numeric", allow_empty = TRUE)
  if (is_empty(times) + is_empty(probs) != 1L) {
    cli::cli_abort(
      "Specify one and only one of arguments {.arg times} and {.arg probs}.",
      call = get_cli_abort_call()
    )
  }
  if (missing(statistic)) {
    get_theme_element(
      "tbl_survfit-arg:statistic",
      default =
        paste0("{estimate} ({conf.low}", get_theme_element("pkgwide-str:ci.sep", default = ", "), "{conf.high})")
    )
  }
  check_string(statistic)
  if (is_string(label)) label <- ~label # styler: off
  if (missing(label_header)) {
    # TODO: Add translations here. Be sure to review the old version..it's a bit different
    # I think it may be best to add "{prob} Percentile" to the translation file, so we can easily invert it
  }
  check_string(label_header)
  estimate_fun <- as_function(estimate_fun)
  missing <- ifelse(missing(missing), "\U2014", check_string(missing))
  check_scalar_range(conf.level, range = c(0, 1))
  if (!is_empty(type)) type <- arg_match(type, values = c("survival", "risk", "cumhaz"))

  tbl_survfit_inputs <- as.list(environment())

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
              )
          )
      }
    )
  browser()

  card_survfit(
    cards = cards,
    statistic = statistic,
    label = label,
    label_header = label_header
  )

    browser()

}

card_survfit <- function(cards,
                         statistic = "{estimate} ({conf.low}, {conf.high})",
                         label = NULL,
                         label_header = ifelse(!is.null(times), "Time {time}", "{prob} Percentile")) {
  browser()
  # assign a variable name to the cards list -----------------------------------
  univariate_survift_count <- 0L
  cards_names <- vector(mode = "list", length = length(cards))
  for (i in seq_along(cards)) {
    # extract stratifying variable names as vector
    cards_names[[i]] <- cards[[i]] |> dplyr::select(cards::all_ard_groups("names")) |> dplyr::slice(1L) |> unlist()
    # if univariate, assign variable Overall
    if (is_empty(cards_names[[i]])) {
      univariate_survift_count <- univariate_survift_count + 1L
      cards_names[[i]] <- paste0("..overall_", univariate_survift_count, "..")
    }

    # check if there are more than one stratifying variable
    if (length(cards_names[[i]]) > 1L) {
      cli::cli_abort(
        c("The {.fun tbl_survfit} fucntion supports {.fun survival::survfit} objects with no more than one stratifying variable.",
          i = "The model is stratified by {.val {cards_names[[i]]}}.")
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

  # add attributes ARD to the cards data frame ---------------------------------
  for (i in seq_along(cards)) {
    if (nrow(dplyr::filter(cards[[i]], .data$context %in% "attributes")) == 0L) {
      cards[[i]] <- cards[[i]] |>
        dplyr::bind_rows(
          dplyr::tibble(
            variable = cards_names[i],
            context = "attributes",
            stat_name = "label",
            stat_label = "Variable Label",
            stat = list(label[[cards_names[i]]] %||% cards_names[i])
          )
        )
    }
  }

  # convert cards data frame to format for gtsummary table_body ----------------
  map(
    cards,
    function(x) {
      # assign gts_column
      x <- x %>%
        dplyr::left_join(
          dplyr::filter(., .data$context %in% "attributes") |>
            dplyr::distinct("variable", "variable_level") |>
            dplyr::mutate(gts_column = paste0("stat_", dplyr::row_number()))
        )


      # no stratifying variable, process as a continuous tbl_summary() variable

    }
  )
}
