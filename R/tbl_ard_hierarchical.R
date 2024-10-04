#' ARD Hierarchical Table
#'
#' @description `r lifecycle::badge('experimental')`\cr
#' *This is an preview of this function. There will be changes in the coming releases, and changes will not undergo a formal deprecation cycle.*
#'
#' Constructs tables from nested or hierarchical data  structures (e.g. adverse events).
#'
#' @inheritParams tbl_hierarchical
#' @inheritParams tbl_ard_summary
#'
#' @return a gtsummary table of class `"tbl_ard_hierarchical"`
#' @export
#'
#' @examples
#' ADAE_subset <- cards::ADAE |>
#'   dplyr::filter(
#'     AESOC %in% unique(cards::ADAE$AESOC)[1:5],
#'     AETERM %in% unique(cards::ADAE$AETERM)[1:5]
#'   )
#'
#' # Example 1: Event Rates  --------------------
#' # First, build the ARD
#' ard <-
#'   cards::ard_stack_hierarchical(
#'     data = ADAE_subset,
#'     variables = c(AESOC, AETERM),
#'     by = TRTA,
#'     denominator = cards::ADSL |> mutate(TRTA = ARM),
#'     id = USUBJID
#'   )
#'
#' # Second, build table from the ARD
#' tbl_ard_hierarchical(
#'   cards = ard,
#'   variables = c(AESOC, AETERM),
#'   by = TRTA
#' )
#'
#' # Example 2: Event Counts  -------------------
#' ard <-
#'   cards::ard_stack_hierarchical_count(
#'     data = ADAE_subset,
#'     variables = c(AESOC, AETERM),
#'     by = TRTA,
#'     denominator = cards::ADSL |> mutate(TRTA = ARM)
#'   )
#'
#' tbl_ard_hierarchical(
#'   cards = ard,
#'   variables = c(AESOC, AETERM),
#'   by = TRTA,
#'   statistic = "{n}"
#' )
tbl_ard_hierarchical <- function(cards,
                                 variables,
                                 by = NULL,
                                 include = everything(),
                                 statistic = "{n} ({p})",
                                 label = NULL) {
  set_cli_abort_call()

  # data argument checks -------------------------------------------------------
  check_not_missing(cards)
  check_class(
    cards, "card",
    message = c("The {.arg {arg_name}} argument must be class {.cls {'card'}}, not {.obj_type_friendly {x}}.",
                i = "Some operations cause a {.cls {'card'}} data frame to lose its class; use {.fun cards::as_card} to restore it as needed.")
  )
  check_not_missing(variables)
  check_string(statistic)

  # define a data frame based on the context of `card` -------------------------
  data <- bootstrap_df_from_cards(cards)

  cards::process_selectors(data, variables = {{ variables }}, by = {{ by }})
  cards::process_selectors(data[variables], include = {{ include }})

  # add the gtsummary column names to ARD data frame ---------------------------
  cards <- .add_gts_column_to_cards_summary(cards, variables, by, hierarchical = TRUE)

  # save arguments
  tbl_ard_hierarchical_inputs <- as.list(environment())
  tbl_ard_hierarchical_inputs[["data"]] <- NULL

  # fill in missing labels -----------------------------------------------------
  default_label <- default_label <- names(data) |> as.list() |> stats::setNames(names(data))
  label <- c(
    label, default_label[setdiff(names(default_label), names(label))]
  )[c(variables, if ("overall" %in% names(label)) "overall")]

  brdg_hierarchical(
    cards = cards,
    variables = variables,
    by = by,
    include = include,
    statistic = rep_named(include, list(statistic)),
    type = rep_named(variables, list("categorical")),
    overall_row = FALSE,
    count = FALSE,
    is_ordered = is.ordered(data[[dplyr::last(variables)]]),
    label = label
  ) |>
    append(
      list(
        cards = list(cards) |> stats::setNames("tbl_ard_hierarchical"),
        inputs = tbl_ard_hierarchical_inputs
      )
    ) |>
    structure(class = c("tbl_ard_hierarchical", "gtsummary"))
}
