#' Add Variance Inflation Factor
#'
#' \lifecycle{experimental}
#' Add the variance inflation factor (VIF) or
#' generalized VIF (GVIF) to the regression table.
#' Function uses `car::vif()` to calculate the VIF.
#'
#' @param x `'tbl_regression'` object
#' @param statistic One of `c("VIF", "GVIF", "GVIF^(1/(2*Df))")`.
#' See [`car::vif()`] for details.
#' @param estimate_fun Default is [`style_sigfig()`].
#' @export
#'
#' @examples
#' # Example 1 ----------------------------------
#' add_vif_ex1 <-
#'   lm(age ~ grade + marker, trial) %>%
#'   tbl_regression() %>%
#'   add_vif()
#' @section Example Output:
#' \if{html}{Example 1}
#'
#' \if{html}{\figure{add_vif_ex1.png}{options: width=45\%}}

add_vif <- function(x, statistic = NULL, estimate_fun = NULL) {
  # checking inputs ------------------------------------------------------------
  if (!inherits(x, "tbl_regression"))
    stop("`x=` must be class 'tbl_regression'")
  assert_package("car")
  estimate_fun <- estimate_fun %||% style_sigfig %>% gts_mapper()

  # calculating VIF ------------------------------------------------------------
  df_vif <- .vif_to_tibble(x$model_obj)

  # assigning statistic to print -----------------------------------------------
  statistic <-
    statistic %||%
    switch("VIF" %in% names(df_vif), "VIF") %||%
    switch("GVIF" %in% names(df_vif), "GVIF") %>%
    match.arg(choices = c("VIF", "GVIF", "GVIF^(1/(2*Df))"))
  if (!statistic %in% names(df_vif))
    glue("Statistic '{statistic}' not available for this model. ",
         "Select from {quoted_list(names(df_vif) %>% intersect(c('VIF', 'GVIF', 'GVIF^(1/(2*Df))')))}.")

  # merging VIF with gtsummary table -------------------------------------------
  # merge in VIF stats
  x %>%
    modify_table_body(
      dplyr::left_join,
      df_vif %>%
        select(any_of(c("variable", "row_type", statistic))) %>%
        dplyr::rename(vif = all_of(statistic)),
      by = c("variable", "row_type")
    ) %>%
    # add column header
    modify_table_header(
      "vif",
      label = "**VIF**",
      fmt_fun = estimate_fun,
      hide = FALSE
    )
}

# put VIF results in data frame
.vif_to_tibble <- function(x) {
  vif <- tryCatch(
    car::vif(x),
    error = function(e) {
      paste("The {ui_code('add_vif()')} uses {ui_code('car::vif()')} to",
            "calculate the VIF, and the function returned an error (see below).") %>%
        stringr::str_wrap() %>%  ui_oops()
      stop(e)
    }
  )

  # if VIF is returned
  if (!is.matrix(vif))
    result <-
      vif %>%
      tibble::enframe("variable", "VIF")
  # if Generalized VIF is returned
  else
    result <-
      vif %>%
      as.data.frame() %>%
      tibble::rownames_to_column(var = "variable") %>%
      tibble::as_tibble()
  return(result %>% mutate(row_type = "label"))
}
