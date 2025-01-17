#' Bold or Italicize
#'
#' Bold or italicize labels or levels in gtsummary tables
#'
#' @name bold_italicize_labels_levels
#' @param x (`gtsummary`)
#'   An object of class 'gtsummary'
#' @author Daniel D. Sjoberg
#'
#' @return Functions return the same class of gtsummary object supplied
#' @examples
#' # Example 1 ----------------------------------
#' tbl_summary(trial, include = c("trt", "age", "response")) |>
#'   bold_labels() |>
#'   bold_levels() |>
#'   italicize_labels() |>
#'   italicize_levels()
NULL


#' @export
#' @rdname bold_italicize_labels_levels
bold_labels <- function(x) {
  UseMethod("bold_labels")
}

#' @export
#' @rdname bold_italicize_labels_levels
italicize_labels <- function(x) {
  UseMethod("italicize_labels")
}

#' @export
#' @rdname bold_italicize_labels_levels
bold_levels <- function(x) {
  UseMethod("bold_levels")
}


#' @export
#' @rdname bold_italicize_labels_levels
italicize_levels <- function(x) {
  UseMethod("italicize_levels")
}

#' @export
#' @rdname bold_italicize_labels_levels
bold_labels.gtsummary <- function(x) {
  updated_call_list <- c(x$call_list, list(bold_labels = match.call()))
  # input checks ---------------------------------------------------------------
  if (!"row_type" %in% x$table_styling$header$column) {
    cli::cli_inform("{.code bold_labels()} cannot be used in this context.")
    return(x)
  }

  # bold labels ----------------------------------------------------------------
  x <-
    .modify_text_format(
      x,
      columns = .first_unhidden_column(x),
      rows = .data$row_type == "label",
      text_format = "bold"
    )

  x$call_list <- updated_call_list

  x
}

#' @export
#' @rdname bold_italicize_labels_levels
bold_levels.gtsummary <- function(x) {
  updated_call_list <- c(x$call_list, list(bold_levels = match.call()))
  # input checks ---------------------------------------------------------------
  if (!"row_type" %in% x$table_styling$header$column) {
    cli::cli_inform("{.code bold_levels()} cannot be used in this context.")
    return(x)
  }

  # bold levels ----------------------------------------------------------------
  x <-
    .modify_text_format(
      x,
      columns = .first_unhidden_column(x),
      rows = .data$row_type != "label",
      text_format = "bold"
    )

  x$call_list <- updated_call_list

  x
}


#' @export
#' @rdname bold_italicize_labels_levels
italicize_labels.gtsummary <- function(x) {
  updated_call_list <- c(x$call_list, list(italicize_labels = match.call()))
  # input checks ---------------------------------------------------------------
  if (!"row_type" %in% x$table_styling$header$column) {
    cli::cli_inform("{.code italicize_labels()} cannot be used in this context.")
    return(x)
  }

  # italicize labels -----------------------------------------------------------
  x <-
    .modify_text_format(
      x,
      columns = .first_unhidden_column(x),
      rows = .data$row_type == "label",
      text_format = "italic"
    )

  x$call_list <- updated_call_list

  x
}

#' @export
#' @rdname bold_italicize_labels_levels
italicize_levels.gtsummary <- function(x) {
  updated_call_list <- c(x$call_list, list(italicize_levels = match.call()))
  # input checks ---------------------------------------------------------------
  if (!"row_type" %in% x$table_styling$header$column) {
    cli::cli_inform("{.code italicize_levels()} cannot be used in this context.")
    return(x)
  }

  # italicize levels -----------------------------------------------------------
  x <-
    .modify_text_format(
      x,
      columns = .first_unhidden_column(x),
      rows = .data$row_type != "label",
      text_format = "italic"
    )

  x$call_list <- updated_call_list

  x
}

#' @export
#' @rdname bold_italicize_labels_levels
bold_labels.tbl_cross <- function(x) {
  # bold labels ----------------------------------------------------------------
  x <- bold_labels.gtsummary(x)

  cols_to_style <-
    select(x$table_body, all_stat_cols(FALSE)) %>%
    names() |>
    # remove hidden columns
    setdiff(x$table_styling$header$column[x$table_styling$header$hide])

  x$table_styling$spanning_header <-
    x$table_styling$spanning_header |>
    dplyr::mutate(
      spanning_header =
        ifelse(
          .data$column %in% .env$cols_to_style & !is.na(.data$spanning_header),
          paste0("**", .data$spanning_header, "**"),
          .data$spanning_header
        )
    )

  x$table_styling$header <-
    x$table_styling$header |>
    dplyr::mutate(
      label =
        dplyr::case_when(
          .data$hide == FALSE & (.data$column %in% c("stat_0", "p.value")) ~
            paste0("**", label, "**"),
          TRUE ~ label
        )
    )

  x
}

#' @export
#' @rdname bold_italicize_labels_levels
bold_levels.tbl_cross <- function(x) {
  # bold levels ----------------------------------------------------------------
  x <- bold_levels.gtsummary(x)

  cols_to_style <- x$table_body |>
    dplyr::select(all_stat_cols(FALSE)) |>
    names()

  x$table_styling$header <-
    dplyr::mutate(x$table_styling$header,
           label =
             dplyr::case_when(
               .data$hide == FALSE & (.data$column %in% cols_to_style) ~
                 paste0("**", label, "**"),
               TRUE ~ label
             )
    )

  x
}

#' @export
#' @rdname bold_italicize_labels_levels
italicize_labels.tbl_cross <- function(x) {
  # italicize labels -----------------------------------------------------------
  x <- italicize_labels.gtsummary(x)

  cols_to_style <-
    dplyr::select(x$table_body, all_stat_cols(FALSE)) %>%
    names() |>
    # remove hidden columns
    setdiff(x$table_styling$header$column[x$table_styling$header$hide])

  x$table_styling$spanning_header <-
    x$table_styling$spanning_header |>
    dplyr::mutate(
      spanning_header =
        ifelse(
          .data$column %in% .env$cols_to_style & !is.na(.data$spanning_header),
          paste0("*", .data$spanning_header, "*"),
          .data$spanning_header
        )
    )

  x$table_styling$header <-
    x$table_styling$header |>
    dplyr::mutate(
      label = dplyr::case_when(
        .data$hide == FALSE & (.data$column %in% c("stat_0", "p.value")) ~
          paste0("*", label, "*"),
        TRUE ~ label
      )
    )

  x
}



#' @export
#' @rdname bold_italicize_labels_levels
italicize_levels.tbl_cross <- function(x) {
  x <- italicize_levels.gtsummary(x)

  # italicize levels ----------------------------------------------------------------
  cols_to_style <- x$table_body |>
    dplyr::select(all_stat_cols(FALSE)) |>
    names()

  x$table_styling$header <-
    dplyr::mutate(x$table_styling$header,
           label =
             dplyr::case_when(
               .data$hide == FALSE & (.data$column %in% cols_to_style) ~
                 paste0("*", label, "*"),
               TRUE ~ label
             )
    )
  x
}



