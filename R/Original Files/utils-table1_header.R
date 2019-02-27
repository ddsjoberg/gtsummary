#' This function creates header rows for `fmt_table1` objects
#'
#' @param data Data frame
#' @param by Character variable name in `data` that Summary statistics for
#' `variable` are stratified
#' @param pvalue logical indicator whether p-values will be reported.
#' @keywords internal

fmt_table1_header <- function(data, by, pvalue) {

  # if there is no by variable, the header will simply be N
  if (is.null(by)) {
    fmt_table1_header <- dplyr::data_frame(
      .stat = paste0("N = ", nrow(data))
    )
  }
  # if there is by variable, the header will simply be N for each group
  if (!is.null(by)) {
    # getting N for each group in data_frame with correctly names cols
    fmt_table1_header <-
      table(data[[by]]) %>%
      as.matrix() %>%
      t() %>%
      dplyr::as_data_frame() %>%
      dplyr::mutate_all(dplyr::funs(paste0("N = ", .)))

    # adding label (defalut is var:var_value)
    fmt_table1_header <-
      dplyr::bind_rows(
        names(fmt_table1_header) %T>% {
          names(.) <- names(fmt_table1_header)
        } %>%
          t() %>%
          dplyr::as_data_frame(),
        fmt_table1_header
      )
  }

  # if there is a pvalue column, adding header
  if (pvalue == TRUE) {
    fmt_table1_header <- fmt_table1_header %>%
      dplyr::mutate_(
        .p = ~ ifelse(dplyr::row_number() == 1, "p-value", NA_character_)
      )
  }

  # adding row_type
  fmt_table1_header <- fmt_table1_header %>%
    dplyr::mutate_(
      .row_type = ~ paste0("header", dplyr::n():1)
    )

  return(fmt_table1_header)
}


# fmt_table1_header(mtcars, "am", TRUE)
