#' Add Variance Inflation Factor
#'
#' \lifecycle{maturing}
#' Add the variance inflation factor (VIF) or
#' generalized VIF (GVIF) to the regression table.
#' Function uses `car::vif()` to calculate the VIF.
#'
#' @param x `'tbl_regression'` object
#' @param statistic `"VIF"` (variance inflation factors, for models with no categorical terms) or one of/combination of `"GVIF"` (generalized variance inflation factors), `"aGVIF"` 'adjusted GVIF, i.e. `GVIF^[1/(2*df)]` and/or `"df"` (degrees of freedom).
#' See `car::vif()` for details.
#' @param estimate_fun Default is [`style_sigfig()`].
#' @export
#'
#' @examples
#' # Example 1 ----------------------------------
#' if (requireNamespace("car")) {
#'   add_vif_ex1 <-
#'     lm(age ~ grade + marker, trial) %>%
#'     tbl_regression() %>%
#'     add_vif()
#' }
#'
#' # Example 2 ----------------------------------
#' if (requireNamespace("car")) {
#'   add_vif_ex2 <-
#'     lm(age ~ grade + marker, trial) %>%
#'     tbl_regression() %>%
#'     add_vif(c("aGVIF", "df"))
#' }
#' @section Example Output:
#' \if{html}{Example 1}
#'
#' \if{html}{\figure{add_vif_ex1.png}{options: width=45\%}}
#' \if{html}{Example 2}
#'
#' \if{html}{\figure{add_vif_ex2.png}{options: width=45\%}}
#'
add_vif <- function(x, statistic = NULL, estimate_fun = NULL) {
  updated_call_list <- c(x$call_list, list(add_vif = match.call()))
  # checking inputs ------------------------------------------------------------
  if (!inherits(x, "tbl_regression")) {
    stop("`x=` must be class 'tbl_regression'")
  }
  assert_package("car", "add_vif()")
  estimate_fun <- estimate_fun %||% style_sigfig %>% gts_mapper("add_vif(estimate_fun=)")

  # calculating VIF ------------------------------------------------------------
  df_vif <- .vif_to_tibble(x$model_obj)

  # assigning statistic to print -----------------------------------------------
  statistic <-
    statistic %||%
    switch("VIF" %in% names(df_vif),
      "VIF"
    ) %||%
    switch("GVIF" %in% names(df_vif),
      c("GVIF", "aGVIF")
    ) %>%
    match.arg(choices = c("VIF", "GVIF", "aGVIF", "df"), several.ok = TRUE)
  if (any(!statistic %in% names(df_vif))) {
    glue(
      "Statistic '{statistic}' not available for this model. ",
      "Select from {quoted_list(names(df_vif) %>% intersect(c('VIF', 'GVIF', 'aGVIF', 'df')))}."
    ) %>%
      stop(call. = FALSE)
  }

  # merging VIF with gtsummary table -------------------------------------------
  # merge in VIF stats
  x <- x %>%
    modify_table_body(
      dplyr::left_join,
      df_vif %>%
        select(any_of(c("variable", "row_type", statistic))),
      by = c("variable", "row_type")
    )

  # add column header
  for (s in statistic) {
    x <- x %>%
      modify_table_styling(
        s,
        label = switch(s,
          "VIF" = "**VIF**",
          "GVIF" = "**GVIF**",
          "aGVIF" = "**Adjusted GVIF**",
          "df" = "**df**"
        ),
        footnote = switch(s,
          "aGVIF" = "GVIF^[1/(2*df)]"
        ),
        footnote_abbrev = switch(s,
          "VIF" = "VIF = Variance Inflation Factor",
          "GVIF" = "GVIF = Generalized Variance Inflation Factor",
          "aGVIF" = "GVIF = Generalized Variance Inflation Factor",
          "df" = "df = degrees of freedom"
        ),
        fmt_fun = switch(s,
          "df" = style_number
        ) %||% estimate_fun,
        hide = FALSE
      )
  }

  # add call list and return x
  x$call_list <- updated_call_list
  x
}

# put VIF results in data frame
.vif_to_tibble <- function(x) {
  vif <- tryCatch(
    car::vif(x),
    error = function(e) {
      paste(
        "The {.code add_vif()} uses {.code car::vif()} to",
        "calculate the VIF, and the function returned an error (see below)."
      ) %>%
        stringr::str_wrap() %>%
        cli_alert_danger()
      stop(e)
    }
  )

  # if VIF is returned
  if (!is.matrix(vif)) {
    result <-
      vif %>%
      tibble::enframe("variable", "VIF")
  } # if Generalized VIF is returned
  else {
    result <-
      vif %>%
      as.data.frame() %>%
      tibble::rownames_to_column(var = "variable") %>%
      tibble::as_tibble() %>%
      dplyr::rename(
        aGVIF = .data$`GVIF^(1/(2*Df))`,
        df = .data$Df
      )
  }

  result <-
    result %>%
    mutate(
      variable = broom.helpers::.clean_backticks(.data$variable),
      row_type = "label"
    )
  return(result)
}
