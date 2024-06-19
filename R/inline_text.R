#' Report statistics from gtsummary tables inline
#'
#' @param x (`gtsummary`)\cr
#'   Object created from a gtsummary function
#' @param ... Additional arguments passed to other methods.
#' @return A string reporting results from a gtsummary table
#' @keywords internal
#' @author Daniel D. Sjoberg
#' @seealso [inline_text.tbl_summary], [inline_text.tbl_svysummary],
#' [inline_text.tbl_regression], [inline_text.tbl_uvregression],
#' [inline_text.tbl_survfit], [inline_text.tbl_cross], [inline_text.gtsummary]
#'
#' @export
inline_text <- function(x, ...) {
  UseMethod("inline_text")
}

#' Report statistics from summary tables inline
#'
#' @param x (`gtsummary`)\cr
#'   gtsummary object
#' @param variable Variable name of statistic to present
#' @param level Level of the variable to display for categorical variables.
#' Default is `NULL`
#' @param column Column name to return from `x$table_body`.
#' @param pattern String indicating the statistics to return.
#' Uses [glue::glue] formatting. Default is `NULL`
#' @param ... Not used
#' @export
#' @name inline_text.gtsummary
#'
#' @section column + pattern:
#'
#' Some gtsummary tables report multiple statistics in a single cell,
#' e.g. `"{mean} ({sd})"` in `tbl_summary()` or `tbl_svysummary()`.
#' We often need to report just the mean or the SD, and that can be accomplished
#' by using both the `column=` and `pattern=` arguments. When both of these
#' arguments are specified, the column argument selects the column to report
#' statistics from, and the pattern argument specifies which statistics to report,
#' e.g. `inline_text(x, column = "stat_1", pattern = "{mean}")` reports just the
#' mean from a `tbl_summary()`.

inline_text.gtsummary <- function(x, variable,
                                  level = NULL, column = NULL, pattern = NULL, ...) {
  set_cli_abort_call()
  check_dots_empty()
  check_not_missing(x)
  check_not_missing(variable)

  column <- rlang::enquo(column)
  column_is_null <- tryCatch(suppressWarnings(is.null(eval_tidy(column))), error = function(e) FALSE)
  level <- rlang::enquo(level)
  level_is_null <- tryCatch(suppressWarnings(is.null(eval_tidy(level))), error = function(e) FALSE)

  # adding raw stats if user will use them -------------------------------------
  if (!is.null(pattern) && !column_is_null) {
    if (is.null(x$meta_data) || !"df_stats" %in% names(x$meta_data) ||
        !all(c("label", "col_name") %in% names(x$meta_data$df_stats[[1]]))) {
      paste(
        "When both `column=` and `pattern=` are specified, the gtsummary",
        "object must have a `x$meta_data` table with column 'df_stats',",
        "and the 'df_stats' data frame must have columns 'label' and 'col_name'."
      ) %>%
        abort()
    }

    x <- df_stats_to_table_body(x)
  }

  # convert gtsummary object to tibble -----------------------------------------
  # removing merging and other styling
  x$table_styling$cols_merge <- filter(x$table_styling$cols_merge, FALSE)
  x$table_styling$text_format <- filter(x$table_styling$text_format, FALSE)
  # keeping all columns
  x$table_styling$header$hide <- FALSE

  df_gtsummary <- as_tibble(x, col_labels = FALSE)

  # variable selection ---------------------------------------------------------
  if (!"variable" %in% names(df_gtsummary)) {
    paste("The gtsummary table does not have the required 'variable' column in `.$table_body`.") %>%
      stop(call. = FALSE)
  }

  variable <-
    .select_to_varnames({{ variable }},
                        var_info = x$table_body,
                        arg_name = "variable",
                        select_single = TRUE
    )

  df_gtsummary <- filter(df_gtsummary, .data$variable %in% .env$variable)

  # check if tbl contains duplicate variable names
  if ("row_type" %in% names(df_gtsummary) &&
      nrow(df_gtsummary %>% filter(.data$row_type %in% "label")) > 1) {
    glue("Variable '{variable}' likely not unique in gtsummary table, and",
         "the cell(s) you wish to display may not be accessible.",
         "This may occur when gtsummary tables with repeated variable",
         "names are combined using `tbl_stack()`.",
         .sep = " "
    ) %>%
      stringr::str_wrap() %>%
      inform()
  }

  # level selection ------------------------------------------------------------
  if (!level_is_null && "" %in% df_gtsummary$label) {
    paste(
      "There is a blank level, which may cause issues selecting",
      "the specified value. Blank levels cannot be selected."
    ) %>%
      inform()
  }

  # if level not provided, keep the first row
  if (level_is_null) {
    df_gtsummary <- filter(df_gtsummary, dplyr::row_number() == 1)
  } # if there is a level, drop first label row, keeping the levels only
  else {
    if (any(!c("row_type", "label") %in% names(df_gtsummary))) {
      paste("The gtsummary table does not have the required 'row_type' and 'label' columns in `.$table_body`.") %>%
        stop(call. = FALSE)
    }
    df_gtsummary <-
      filter(df_gtsummary, !(.data$row_type %in% "label" & dplyr::row_number() == 1))
    level <-
      .select_to_varnames(!!level,
                          var_info = df_gtsummary$label,
                          arg_name = "level",
                          select_single = TRUE
      )
    df_gtsummary <- filter(df_gtsummary, .data$label %in% .env$level)
  }

  # assert we've selected one line of table ------------------------------------
  if (nrow(df_gtsummary) != 1L) abort("Criteria must select exactly one row.")

  # cell/pattern selection -----------------------------------------------------
  if (!column_is_null) {
    column <-
      .select_to_varnames(
        !!column,
        data = df_gtsummary,
        arg_name = "column",
        select_single = TRUE
      )
  }

  # check pattern argument input
  if (!is.null(pattern) && !rlang::is_string(pattern)) {
    abort("`pattern=` argument must be a string.")
  }

  # if column selected and not pattern, return column
  if (!column_is_null && is.null(pattern)) {
    return(df_gtsummary[[column]])
  }

  # if using pattern and column name, rename the columns that comprise column name
  if (!is.null(pattern) && !column_is_null) {
    df_gtsummary <-
      df_gtsummary %>%
      bind_cols(
        select(., starts_with(vec_paste0("raw_", column, "_"))) %>%
          dplyr::rename_with(
            .fn = ~ stringr::str_remove(., fixed(vec_paste0("raw_", column, "_"))),
            .cols = starts_with(vec_paste0("raw_", column, "_"))
          )
      )
  }

  # if no column and pattern, return pattern
  if (!is.null(pattern)) {
    return(glue::glue_data(df_gtsummary, pattern) %>% as.character())
  }

  # must select column or pattern
  if (column_is_null && is.null(pattern)) {
    abort("Both `column=` and `pattern=` cannot be NULL")
  }
}

# function to apply the fmt_fun attr in df_stats
.apply_attr_fmt_fun <- function(x) {
  map(
    x,
    function(.x) {
      if (!is.null(attr(.x, "fmt_fun"))) {
        return(unname(do.call(attr(.x, "fmt_fun"), list(.x))))
      }
      return(.x)
    }
  )
}
