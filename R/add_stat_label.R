#' Add statistic labels
#'
#' Adds labels describing the summary statistics presented for
#' each variable in the [tbl_summary] / [tbl_svysummary] table.
#'
#' @section Tips:
#'
#' When using `add_stat_label(location='row')` with subsequent `tbl_merge()`,
#' it's important to have somewhat of an understanding of the underlying
#' structure of the gtsummary table.
#' `add_stat_label(location='row')` works by adding a new column called
#' `"stat_label"` to `x$table_body`. The `"label"` and `"stat_label"`
#' columns are merged when the gtsummary table is printed.
#' The `tbl_merge()` function merges on the `"label"` column (among others),
#' which is typically the first column you see in a gtsummary table.
#' Therefore, when you want to merge a table that has run `add_stat_label(location='row')`
#' you need to match the `"label"` column values before the `"stat_column"`
#' is merged with it.
#'
#' For example, the following two tables merge properly
#'
#' ```r
#' tbl1 <- trial %>% select(age, grade) %>% tbl_summary() %>% add_stat_label()
#' tbl2 <- lm(marker ~ age + grade, trial) %>% tbl_regression()
#'
#' tbl_merge(list(tbl1, tbl2))
#' ```
#'
#' @param x Object with class `tbl_summary` from the [tbl_summary] function
#' or with class `tbl_svysummary` from the [tbl_svysummary] function
#' @param location location where statistic label will be included.
#'  `"row"` (the default) to add the statistic label to the variable label row,
#'  and `"column"` adds a column with the statistic label.
#' @param label a list of formulas or a single formula updating the statistic
#' label, e.g. `label = all_categorical() ~ "No. (%)"`
#' @family tbl_summary tools
#' @family tbl_svysummary tools
#' @seealso Review [list, formula, and selector syntax][syntax] used throughout gtsummary
#' @author Daniel D. Sjoberg
#' @export
#' @return A `tbl_summary` or `tbl_svysummary` object
#' @examples
#' \donttest{
#' tbl <- trial %>%
#'   dplyr::select(trt, age, grade, response) %>%
#'   tbl_summary(by = trt)
#'
#' # Example 1 ----------------------------------
#' # Add statistic presented to the variable label row
#' add_stat_label_ex1 <-
#'   tbl %>%
#'   add_stat_label(
#'     # update default statistic label for continuous variables
#'     label = all_continuous() ~ "med. (iqr)"
#'   )
#'
#' # Example 2 ----------------------------------
#' add_stat_label_ex2 <-
#'   tbl %>%
#'   add_stat_label(
#'     # add a new column with statistic labels
#'     location = "column"
#'   )
#'
#' # Example 3 ----------------------------------
#' add_stat_label_ex3 <-
#'   trial %>%
#'   select(age, grade, trt) %>%
#'   tbl_summary(
#'     by = trt,
#'     type = all_continuous() ~ "continuous2",
#'     statistic = all_continuous() ~ c("{mean} ({sd})", "{min} - {max}"),
#'   ) %>%
#'   add_stat_label(label = age ~ c("Mean (SD)", "Min - Max"))
#' }
#' @section Example Output:
#' \if{html}{Example 1}
#'
#' \if{html}{\out{
#' `r man_create_image_tag(file = "add_stat_label_ex1.png", width = "60")`
#' }}
#'
#' \if{html}{Example 2}
#'
#' \if{html}{\out{
#' `r man_create_image_tag(file = "add_stat_label_ex2.png", width = "60")`
#' }}
#'
#' \if{html}{Example 3}
#'
#' \if{html}{\out{
#' `r man_create_image_tag(file = "add_stat_label_ex3.png", width = "45")`
#' }}

add_stat_label <- function(x, location = NULL, label = NULL) {
  updated_call_list <- c(x$call_list, list(add_stat_label = match.call()))
  # checking inputs ------------------------------------------------------------
  .assert_class(x, c("tbl_summary", "tbl_svysummary"))

  # if `add_stat_label()` already run, return unmodified -----------------------
  if ("add_stat_label" %in% names(x$call_list)) {
    cli_alert_info("{.code add_stat_label()} has previously been applied. Returning {.field gtsummary} table unaltered.")
    return(x)
  }

  # setting defaults -----------------------------------------------------------
  location <- location %||%
    get_theme_element("add_stat_label-arg:location") %>%
    match.arg(choices = c("row", "column"))

  # processing statistics label ------------------------------------------------
  # converting input to named list
  label <-
    .formula_list_to_named_list(
      x = label,
      data =
        switch(!is_survey(x$inputs$data),
          x$inputs$data[x$meta_data$variable]
        ) %||%
          x$inputs$data$variables[x$meta_data$variable],
      var_info = meta_data_to_var_info(x$meta_data),
      arg_name = "label",
      type_check = chuck(type_check, "is_character", "fn"),
      type_check_msg = chuck(type_check, "is_character", "msg")
    )

  # stat_label column
  df_stat_label <-
    x$meta_data %>%
    filter(!.data$summary_type %in% "continuous2") %>%
    select("variable", "stat_label") %>%
    tibble::deframe() %>%
    # updating the default values with values in label
    purrr::imap_chr(~ label[[.y]] %||% .x) %>%
    tibble::enframe("variable", "stat_label")

  # adding stat_label to `.$table_body`
  x <-
    x %>%
    modify_table_body(
      ~ .x %>%
        left_join(df_stat_label, by = "variable") %>%
        dplyr::relocate("stat_label", .after = "label") %>%
        mutate(
          # adding in "n" for missing rows, and header
          stat_label = case_when(
            .data$row_type == "missing" ~ "n",
            TRUE ~ .data$stat_label
          ),
          # setting some rows to NA depending on output type
          stat_label =
            switch(location,
              "row" = ifelse(.data$row_type %in% "label", .data$stat_label, NA),
              "column" =
                ifelse(
                  .data$row_type %in% "label" & .data$var_type %in% "categorical",
                  NA, .data$stat_label
                )
            )
        )
    ) %>%
    # removing stat label footnote
    modify_footnote(all_stat_cols() ~ NA_character_)

  # updating `continuous2` stat labels if they exist ---------------------------
  df_con2_update <-
    x$meta_data %>%
    filter(.data$summary_type %in% "continuous2") %>%
    select("variable", "summary_type", "stat_label") %>%
    mutate(
      stat_label = map2(.data$stat_label, .data$variable, ~ label[[.y]] %||% .x),
      row_type = "level"
    ) %>%
    tidyr::unnest("stat_label") %>%
    dplyr::rename(var_type = "summary_type", label = "stat_label")
  rows_to_update <-
    x$table_body$variable %in% unique(df_con2_update$variable) &
      x$table_body$var_type %in% "continuous2" &
      x$table_body$row_type %in% "level"
  if (nrow(df_con2_update) != sum(rows_to_update)) {
    abort("`label=` dimensions do not match for type `continuous2` variables.")
  }
  x$table_body$label[which(rows_to_update)] <- df_con2_update$label

  # if adding stat labels to row, then adding merge instructions ---------------
  if (location == "row") {
    x <-
      modify_table_styling(
        x,
        columns = "label",
        rows = !is.na(.data$stat_label),
        cols_merge_pattern = "{label}, {stat_label}"
      )
  }
  # unhiding column if requested -----------------------------------------------
  else if (location == "column") {
    x <- modify_header(x, stat_label ~ paste0("**", translate_text("Statistic"), "**"))
  }

  # keeping track of all functions previously run ------------------------------
  # fill in the Ns in the header table modify_stat_* columns
  x <- .fill_table_header_modify_stats(x)
  x$call_list <- updated_call_list

  x
}
