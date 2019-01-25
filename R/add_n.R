#' Adds a column with N (or N missing) for each variable
#'
#' The function assumes the DEFAULT header are in use. Only modify header rows
#' after all columns has been added.
#'
#' @param x object with class `fmt_table1` from the \code{\link{fmt_table1}} function
#' @param missing logical argument indicating whether to print N (`missing = FALSE`),
#' or N missing (`missing = TRUE`).  Default is `FALSE`
#' @param last logical indicator to include overall  column last. Default is `FALSE`
#' @export
#' @examples
#' trial %>% fmt_table1(by = "trt") %>% add_n()
add_n <- function(x, missing = FALSE, last = FALSE) {
  # checking that input is class fmt_table1
  if (class(x) != "fmt_table1") stop("x must be class 'fmt_table1'")

  # counting N
  if (missing == FALSE) {
    counts <-
      x$meta_data %>%
      dplyr::select(c(".variable")) %>%
      dplyr::mutate_(
        row_type = ~"label",
        N = ~ purrr::map_chr(
          .variable,
          ~ (nrow(x$inputs$data) - (x$inputs$data[[.x]] %>% is.na() %>% sum())) %>% as.character()
        )
      )
  }

  # counting missing N
  if (missing == TRUE) {
    counts <-
      x$meta_data %>%
      dplyr::select(c(".variable")) %>%
      dplyr::mutate_(
        row_type = ~"label",
        N_missing = ~ purrr::map_chr(
          .variable,
          ~ x$inputs$data[[.x]] %>% is.na() %>% sum() %>% as.character()
        )
      )
  }

  # adding header rows
  # when there is a by var, 2 deep, otherwise one
  if (x$inputs$by %>% is.null()) {
    by_space <- NULL
  } else {
    by_space <- ""
  }
  if (missing == FALSE) {
    header <-
      dplyr::data_frame(
        .variable = c(NA_character_, by_space),
        N = c("N", by_space)
      )
  }
  if (missing == TRUE) {
    header <-
      dplyr::data_frame(
        .variable = c(NA_character_, by_space),
        N_missing = c("N Missing", by_space)
      )
  }

  # stacking header onto counts (and adding header row_type)
  counts <- header %>%
    dplyr::mutate_(
      row_type = ~ paste0("header", dplyr::n():1)
    ) %>%
    dplyr::bind_rows(counts)

  # merging result with existing table1
  if (last == FALSE) {
    table1 <-
      x$table1 %>%
      dplyr::select(c(".variable", "row_type", "label")) %>%
      dplyr::left_join(counts, by = c(".variable", "row_type")) %>%
      dplyr::left_join(x$table1, by = c(".variable", "row_type", "label"))
  }

  if (last == TRUE) {
    table1 <-
      x$table1 %>%
      dplyr::left_join(counts, by = c(".variable", "row_type"))
  }

  # replacing old table1 with new
  x$table1 <- table1

  # adding indicator to output that add_overall was run on this data
  x$call_list <- c(x$call_list, list(add_overall = match.call()))

  # returning fmt_table1 object
  return(x)
}
