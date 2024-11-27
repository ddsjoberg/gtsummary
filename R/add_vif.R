#' Add Variance Inflation Factor
#'
#' Add the variance inflation factor (VIF) or
#' generalized VIF (GVIF) to the regression table.
#' Function uses `car::vif()` to calculate the VIF.
#'
#' @param x `'tbl_regression'` object
#' @param statistic `"VIF"` (variance inflation factors, for models with no categorical terms) or one of/combination of `"GVIF"` (generalized variance inflation factors), `"aGVIF"` 'adjusted GVIF, i.e. `GVIF^[1/(2*df)]` and/or `"df"` (degrees of freedom).
#' See `car::vif()` for details.
#' @param estimate_fun Default is `label_style_sigfig(digits = 2)`.
#' @seealso Review [list, formula, and selector syntax][syntax] used throughout gtsummary
#' @export
#'
#' @examplesIf (identical(Sys.getenv("NOT_CRAN"), "true") || identical(Sys.getenv("IN_PKGDOWN"), "true")) && gtsummary:::is_pkg_installed(c("cardx", "car"))
#' # Example 1 ----------------------------------
#' lm(age ~ grade + marker, trial) |>
#'   tbl_regression() |>
#'   add_vif()
#'
#' # Example 2 ----------------------------------
#' lm(age ~ grade + marker, trial) |>
#'   tbl_regression() |>
#'   add_vif(c("aGVIF", "df"))
add_vif <- function(x, statistic = NULL, estimate_fun = label_style_sigfig(digits = 2)) {
  set_cli_abort_call()
  check_pkg_installed(c("car", "cardx"))

  # check inputs ---------------------------------------------------------------
  check_not_missing(x)
  check_class(x, "tbl_regression")
  estimate_fun <- as_function(estimate_fun)
  updated_call_list <- c(x$call_list, list(add_vif = match.call()))

  # calculating VIF ------------------------------------------------------------
  ard_vif <-
    cards::eval_capture_conditions(
      cardx::ard_car_vif(x$inputs$x)
    )
  if (!is_empty(ard_vif[["error"]])) {
    cli::cli_abort("{ard_vif[['error']]}", call = get_cli_abort_call())
  }
  x$cards$vif <- ard_vif[["result"]]

  # assigning statistic to print -----------------------------------------------
  vif_values <- ard_vif[["result"]]$stat_name |> unique()
  if (is_empty(statistic)) {
    statistic <- vif_values |> intersect(c("VIF", "GVIF", "aGVIF"))
  }
  statistic <- arg_match(statistic, values = vif_values, multiple = TRUE)

  # add the stats to the tbl ---------------------------------------------------
  ard_vif_wide <- ard_vif[["result"]] |>
    dplyr::filter(.data$stat_name %in% .env$statistic) |>
    tidyr::pivot_wider(
      id_cols = "variable",
      values_from = "stat",
      names_from = "stat_name",
      values_fn = unlist
    ) |>
    dplyr::mutate(row_type = "label") |>
    dplyr::relocate(any_of(statistic), .after = everything())

  x <- x |>
    modify_table_body(
      ~ dplyr::left_join(.x, ard_vif_wide, by = c("variable", "row_type"))
    )

  # add column header
  for (s in statistic) {
    x <- x %>%
      modify_table_styling(
        all_of(s),
        label = switch(s,
                       "VIF" = "**VIF**",
                       "GVIF" = "**GVIF**",
                       "aGVIF" = "**Adjusted GVIF**",
                       "df" = "**df**"
        ),
        footnote = switch(s, "aGVIF" = "GVIF^[1/(2*df)]"),
        footnote_abbrev = switch(s,
                                 "VIF" = "VIF = Variance Inflation Factor",
                                 "GVIF" = "GVIF = Generalized Variance Inflation Factor",
                                 "aGVIF" = "GVIF = Generalized Variance Inflation Factor",
                                 "df" = "df = degrees of freedom"
        ),
        fmt_fun = switch(s, "df" = style_number) %||% estimate_fun,
        hide = FALSE
      )
  }

  # fill in the Ns in the header table modify_stat_* columns
  x <- .fill_table_header_modify_stats(x)
  # add call list and return x
  x$call_list <- updated_call_list
  x
}
