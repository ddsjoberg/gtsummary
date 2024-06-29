#' Add event N
#'
#' For each `survfit()` object summarized with `tbl_survfit()` this function
#' will add the total number of events observed in a new column.
#'
#' @param x object of class 'tbl_survfit'
#' @param ... Not used
#' @export
#' @family tbl_survfit tools
#' @examplesIf gtsummary:::is_pkg_installed(c("survival", "broom"), reference_pkg = "gtsummary")
#' library(survival)
#' fit1 <- survfit(Surv(ttdeath, death) ~ 1, trial)
#' fit2 <- survfit(Surv(ttdeath, death) ~ trt, trial)
#'
#' # Example 1 ----------------------------------
#' list(fit1, fit2) |>
#'   tbl_survfit(times = c(12, 24)) |>
#'   add_n() |>
#'   add_nevent()
add_nevent.tbl_survfit <- function(x, ...) {
  set_cli_abort_call()
  check_dots_empty()
  updated_call_list <- c(x$call_list, list(add_nevent = match.call()))
  check_pkg_installed("broom", reference_pkg = "gtsummary")

  # checking survfit is a standard (not multi-state)
  if (some(x$inputs$x, ~ !rlang::is_empty(setdiff(class(.x), c("survfit", "survfit2"))))) {
    cli::cli_abort(
      c("Each of the {.fun survfit} objects must have class {.cls survfit} only.",
        i = "Multi-state models are not supported by this function."),
      call = get_cli_abort_call()
    )
  }

  # calculating event N --------------------------------------------------------
  x$table_body <-
    map2(
      x$inputs$x,
      names(x$inputs$x),
      ~ dplyr::tibble(
        nevent = broom::tidy(.x) |> dplyr::pull("n.event") |> sum(),
        variable = .y,
        row_type = "label"
      )
    ) |>
    dplyr::bind_rows() %>%
    {dplyr::left_join(x$table_body, ., by = c("variable", "row_type"))} %>% # styler: off
    select(any_of(c("variable", "row_type", "label", "N", "nevent")), everything())

  # adding N to table_styling and assigning header label -----------------------
  x <-
    modify_table_styling(
      x,
      columns = "nevent",
      label = "**Event N**",
      fmt_fun = style_number,
      hide = FALSE
    )

  # fill in the Ns in the header table modify_stat_* columns
  x <- .fill_table_header_modify_stats(x)
  # adding indicator to output that add_n was run on this data
  x$call_list <- updated_call_list
  x
}
