#' Add N
#'
#' For each `survfit()` object summarized with `tbl_survfit()` this function
#' will add the total number of observations in a new column.
#'
#' @param x object of class "`tbl_survfit`"
#' @param ... Not used
#' @export
#'
#' @examplesIf gtsummary:::is_pkg_installed("survival", reference_pkg = "gtsummary")
#' library(survival)
#' fit1 <- survfit(Surv(ttdeath, death) ~ 1, trial)
#' fit2 <- survfit(Surv(ttdeath, death) ~ trt, trial)
#'
#' # Example 1 ----------------------------------
#' list(fit1, fit2) |>
#'   tbl_survfit(times = c(12, 24)) |>
#'   add_n()
add_n.tbl_survfit <- function(x, ...) {
  set_cli_abort_call()
  check_dots_empty()
  updated_call_list <- c(x$call_list, list(add_n = match.call()))

  # adding N to the table_body -------------------------------------------------
  x$table_body <-
    map2(
      x$inputs$x,
      names(x$inputs$x),
      function(suvfit, variable) {
        # extracting survfit call
        survfit_call <- suvfit$call %>% as.list()
        # index of formula and data
        call_index <- names(survfit_call) %in% c("formula", "data") |> which()

        # converting call into a survdiff call
        model.frame_call <- call2(expr(stats::model.frame), !!!survfit_call[call_index], ...)

        # returning number of rows in data frame
        dplyr::tibble(
          variable = variable,
          row_type = "label",
          N = safe_survfit_eval(model.frame_call) |> nrow()
        )
      }
    ) %>%
    dplyr::bind_rows() %>%
    {dplyr::left_join(x$table_body, .,  by = c("variable", "row_type"))} %>%
    select(any_of(c("variable", "row_type", "label", "N")), everything())

  # adding styling data for N column -------------------------------------------
  x <-
    modify_table_styling(
      x,
      columns = "N",
      label = "**N**",
      fmt_fun = style_number,
      hide = FALSE
    )

  # fill in the Ns in the header table modify_stat_* columns
  x <- .fill_table_header_modify_stats(x)
  # adding indicator to output that add_n was run on this data
  x$call_list <- updated_call_list
  x
}

safe_survfit_eval <- function(x) {
  tryCatch(
    eval(x),
    error = function(e) {
      cli::cli_abort(
        c(
          "There was an error executing {.fun add_n} or {.fun add_p}.
           The error may be a due to the construction of the original {.fun survival::survfit} object.",
          i = "Please review {.help tbl_survfit_errors} for a possible solution."
        ),
        call = get_cli_abort_call()
      )
    }
  )
}
