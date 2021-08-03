#' Continuous cross tabulation
#'
#' Summarize a continuous by categorical variables
#'
#' @param con Variable name of the continuous column to be summarized
#' @param rows Vector of categorical variable names
#' @param col Option name of single categorical variable. Default it `NULL`
#' @param statistic String specifying the continuous summary statistics to
#' display.  The default is `"{median} ({p25}, {p75})"`. See
#' `tbl_summary(statistic=)` argument for details.
#' @inheritParams tbl_summary
#'
#' @return a gtsummary table
#' @export
#'
#' @examples
#' # Example 1 ----------------------------------
#' tbl_cross_continuous_ex1 <-
#'   tbl_cross_continuous(
#'     data = trial,
#'     con = age,
#'     col = trt,
#'     rows = grade
#'   )
#'
#' # Example 2 ----------------------------------
#' tbl_cross_continuous_ex2 <-
#'   tbl_cross_continuous(
#'     data = trial,
#'     con = age,
#'     rows = grade
#'   )
#' @section Example Output:
#' \if{html}{Example 1}
#'
#' \if{html}{\figure{tbl_cross_continuous_ex1.png}{options: width=38\%}}
#'
#' \if{html}{Example 2}
#'
#' \if{html}{\figure{tbl_cross_continuous_ex2.png}{options: width=35\%}}

tbl_cross_continuous <- function(data,
                                 con,
                                 rows,
                                 col = NULL,
                                 statistic = "{median} ({p25}, {p75})",
                                 label = NULL) {
  # evaluate inputs ------------------------------------------------------------
  con <- .select_to_varnames(data = data, select = {{ con }},
                             select_single = TRUE, arg_name = "con")
  rows <- .select_to_varnames(data = data, select = {{ rows }},
                              select_single = FALSE, arg_name = "rows")
  col <- .select_to_varnames(data = data, select = {{ col }},
                             select_single = TRUE, arg_name = "col")
  label <- .formula_list_to_named_list(label, data = data, arg_name = "label")

  # saving function inputs -----------------------------------------------------
  tbl_cross_continuous_inputs <- as.list(environment())

  # construct data list --------------------------------------------------------
  if (is.null(col))
    data_list <- list(data) %>% set_names(paste0("**N = ", nrow(data), "**"))
  else
    data_list <-
    data %>%
    tidyr::nest(data = -all_of(col)) %>%
    arrange(.data[[col]]) %>%
    tibble::deframe()

  tbls <-
    data_list %>%
    imap(
      function(data, data_label) {
        rows %>%
          map(
            ~make_rows(data = data, con = con, row = .x,
                       row_label = label[[.x]] %||% attr(data[[.x]], "label") %||% .x,
                       statistic = statistic)
          ) %>%
          tbl_stack(quiet = TRUE) %>%
          modify_header(list("stat_0" = data_label))
      }
    )

  if (!is.null(col)) {
    col_label <-
      label[[col]] %||% attr(data[[col]], "label") %||% col %>%
      {paste0("**", ., "**")}
    tbl <-
      tbl_merge(tbls) %>%
      modify_spanning_header(all_stat_cols() ~ col_label) %>%
      # renaming var_type
      modify_table_body(
        ~.x %>%
          mutate(var_type = .data$var_type_1, .after = .data$variable)
      )
  }
  else
    tbl <- tbls[[1]]

  # cleanup internal objects ---------------------------------------------------
  class(tbl) <- c("tbl_cross_continuous", "gtsummary")
  tbl <- cleanup_internals(tbl)

  # add footnote ---------------------------------------------------------------
  con_label <- label[[con]] %||% attr(data[[con]], "label") %||% con
  tbl <-
    modify_footnote(tbl, all_stat_cols() ~ glue("{con_label}: {stat_label_match(statistic)}"))

  # return tbl -----------------------------------------------------------------
  tbl[["call_list"]] <- list(tbl_cross_continuous = match.call())
  tbl[["inputs"]] <- tbl_cross_continuous_inputs
  tbl
}

# construct rows
make_rows <- function(data, con, row, row_label, statistic) {
  data %>%
    nest(data = -all_of(.env$row)) %>%
    arrange(!!sym(row)) %>%
    mutate(
      tbl =
        map2(
          .data$data,
          .data[[row]],
          ~tbl_summary(
            data = .x,
            include = all_of(con),
            statistic = list(.env$statistic) %>% rlang::set_names(.env$con),
            type = list("continuous") %>% rlang::set_names(.env$con),
            label = list(as.character(.y)) %>% rlang::set_names(.env$con),
            missing = "no"
          )
        )
    ) %>%
    dplyr::pull(.data$tbl) %>%
    tbl_stack(quiet = TRUE) %>%
    modify_table_body(
      ~.x %>%
        mutate(
          row_type = "level"
        ) %>%
        {bind_rows(
          tibble::tibble(
            row_type = "label",
            label = row_label
          ), .
        )} %>%
        mutate(variable = .env$row,
               var_type = "categorical",
               var_label = .env$row_label)
    )
}

cleanup_internals <- function(x) {
  # replace in text_format styling ---------------------------------------------
  x$table_styling$text_format <-
    tibble(
      column = "label",
      rows = expr(.data$row_type %in% c("level", "missing")) %>% list(),
      format_type = "indent",
      undo_text_format = FALSE
    )

  # rename columns in table styling --------------------------------------------
  for (styling in names(x$table_styling)) {
    if (is.data.frame(x$table_styling[[styling]]) &&
        "column" %in% names(x$table_styling[[styling]])) {
      x$table_styling[[styling]] <-
        x$table_styling[[styling]] %>%
        mutate(
          column = stringr::str_replace(.data$column, "^stat_0_", "stat_")
        )
    }
  }

  # rename columns in table body -----------------------------------------------
  modify_table_body(
    x,
    ~ .x %>%
      dplyr::rename_with(~stringr::str_replace(., "^stat_0_", "stat_")) %>%
      select(-starts_with("tbl_id"), -starts_with("var_type_"))
  ) %>%
    purrr::list_modify(tbls = NULL)
}
