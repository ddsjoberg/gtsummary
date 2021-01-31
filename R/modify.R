#' Modify column headers, footnotes, spanning headers, and table captions
#'
#' These functions assist with updating or adding column headers
#' (`modify_header()`), footnotes (`modify_footnote()`), spanning
#' headers (`modify_spanning_header()`), and table captions
#' (`modify_caption()`). Use `show_header_names()` to learn
#' the column names.
#'
#' @name modify
#' @param x a gtsummary object
#' @param update list of formulas or a single formula specifying the updated
#' column header, footnote, or spanning header.
#' The LHS specifies the column(s) to be updated, and the RHS is the updated text.
#' Use the `show_header_names()` to see the column names that can be modified.
#' @param abbreviation Logical indicating if an abbreviation is being updated.
#' @param stat_by DEPRECATED, use `update = all_stat_cols() ~ "<label>"` instead.
#' @param ... Specify a column and updated column label,
#' e.g. `modify_header(p.value = "Model P-values")`. This is provided as an alternative to the
#' `update=` argument. They accomplish the same goal of updating column headers.
#' @param text_interpret String indicates whether text will be interpreted with
#' [gt::md()] or [gt::html()]. Must be `"md"` (default) or `"html"`.
#' @param caption a string of the table caption/title
#' @inheritParams add_global_p
#' @family tbl_summary tools
#' @family tbl_svysummary tools
#' @family tbl_regression tools
#' @family tbl_uvregression tools
#' @family tbl_survfit tools
#' @author Daniel D. Sjoberg
#'
#' @section tbl_summary(), tbl_svysummary(), and tbl_cross():
#' When assigning column headers, footnotes, spanning headers, and captions
#' for these gtsummary tables,
#' you may use `{N}` to insert the number of observations.
#' `tbl_svysummary` objects additionally have `{N_unweighted}` available.
#'
#' When there is a stratifying `by=` argument present, the following fields are
#' additionally available to stratifying columns: `{level}`, `{n}`, and `{p}`
#' (`{n_unweighted}` and `{p_unweighted}` for `tbl_svysummary` objects)
#'
#' Syntax follows [glue::glue()], e.g. `all_stat_cols() ~ "**{level}**, N = {n}"`.
#' @section tbl_regression():
#' When assigning column headers for `tbl_regression` tables,
#' you may use `{N}` to insert the number of observations.
#'
#' @section captions:
#' Captions are assigned based on output type.
#' - `gt::gt(caption=)`, available in gt version >0.2.2
#' - `flextable::set_caption(caption=)`
#' - `huxtable::set_caption(value=)`
#' - `knitr::kable(caption=)`
#'
#' @examples
#' # create summary table
#' tbl <- trial[c("age", "grade", "trt")] %>%
#'   tbl_summary(by = trt, missing = "no") %>%
#'   add_p()
#'
#' # print the column names that can be modified
#' show_header_names(tbl)
#'
#' # Example 1 ----------------------------------
#' # updating column headers, footnote, and table caption
#' modify_ex1 <- tbl %>%
#'   modify_header(
#'     update = list(label ~ "**Variable**",
#'                   p.value ~ "**P**")
#'   ) %>%
#'   modify_footnote(
#'     update = all_stat_cols() ~ "median (IQR) for Age; n (%) for Grade"
#'   ) %>%
#'   modify_caption("**Patient Characteristics** (N = {N})")
#'
#' # Example 2 ----------------------------------
#' # updating headers, remove all footnotes, add spanning header
#' modify_ex2 <- tbl %>%
#'   modify_header(update = all_stat_cols() ~ "**{level}**, N = {n} ({style_percent(p)}%)") %>%
#'   # use `modify_footnote(everything() ~ NA, abbreviation = TRUE)` to delete abbrev. footnotes
#'   modify_footnote(update = everything() ~ NA) %>%
#'   modify_spanning_header(all_stat_cols() ~ "**Treatment Received**")
#'
#' # Example 3 ----------------------------------
#' # updating an abbreviation in table footnote
#' modify_ex3 <-
#'   glm(response ~ age + grade, trial, family = binomial) %>%
#'   tbl_regression(exponentiate = TRUE) %>%
#'   modify_footnote(ci ~ "CI = Credible Interval", abbreviation = TRUE)
#'
#' @return Updated gtsummary object
#' @section Example Output:
#' \if{html}{Example 1}
#'
#' \if{html}{\figure{modify_ex1.png}{options: width=45\%}}
#'
#' \if{html}{Example 2}
#'
#' \if{html}{\figure{modify_ex2.png}{options: width=45\%}}
#'
#' \if{html}{Example 3}
#'
#' \if{html}{\figure{modify_ex3.png}{options: width=35\%}}
NULL

#' @name modify
#' @export
modify_header <- function(x, update = NULL, text_interpret = c("md", "html"),
                          quiet = NULL, ..., stat_by = NULL) {
  # setting defaults -----------------------------------------------------------
  quiet <- quiet %||% get_theme_element("pkgwide-lgl:quiet") %||% FALSE
  text_interpret <- match.arg(text_interpret)

  # converting update arg to a tidyselect list ---------------------------------
  update <-
    .formula_list_to_named_list(
      x = update,
      var_info = x$table_styling$header$column,
      arg_name = "update"
    ) %>%
    c(list(...)) # adding the ... to the update list
  if (!is.null(stat_by)) {
    # will mark this DEPRECATED, but won't print a deprecation note until later
    # lifecycle::deprecate_warn(
    #   "1.3.6",
    #   "gtsummary::modify_header(stat_by=)",
    #   details = glue("Use {ui_code(rlang::expr(modify_header(update =  all_stat_cols(FALSE) ~ !!stat_by)) %>% deparse(width.cutoff = 500L))} instead."))
    update <-
      c(update,
        .formula_list_to_named_list(x = rlang::inject(all_stat_cols(FALSE) ~ !!as.character(stat_by)),
                                    var_info = x$table_header$column,
                                    arg_name = "update"))
  }
  if (identical(list(), update)) update <- NULL

  # if no columns selected, print helpful message
  if (is.null(update) && is.null(stat_by) && identical(quiet, FALSE)) .modify_no_selected_vars(x)
  if (is.null(update) && is.null(stat_by)) return(x)
  if (purrr::map_lgl(update, ~!rlang::is_string(.)) %>% any())
    stop("All labels must be strings of length one.")

  # evaluating update with glue ------------------------------------------------
  update <-
    unlist(update) %>%
    tibble::enframe("column", "label") %>%
    inner_join(.info_tibble(x), by = "column") %>%
    dplyr::rowwise() %>%
    mutate(
      updated_value = switch(is.na(.data$label), NA_character_) %||%
        as.character(glue(.data$label)),
    ) %>%
    select(.data$column, .data$updated_value) %>%
    tibble::deframe() %>%
    as.list()

  # updating column headers ----------------------------------------------------
  x <-
    modify_table_styling(
      x,
      columns = names(update),
      label = unlist(update),
      text_interpret = glue("gt::{text_interpret}") %>% as.character(),
      hide = FALSE
    )

  # returning gtsummary object -------------------------------------------------
  x
}

#' @name modify
#' @export
modify_footnote <- function(x, update = NULL, abbreviation = FALSE,
                            text_interpret = c("md", "html"), quiet = NULL) {
  # checking inputs ------------------------------------------------------------
  if (!inherits(x, "gtsummary")) {
    stop("Argument `x=` must be an object with 'gtsummary' class", call. = FALSE)
  }
  # setting defaults -----------------------------------------------------------
  quiet <- quiet %||% get_theme_element("pkgwide-lgl:quiet") %||% FALSE

  # update table_style --------------------------------------------------------
  # x$table_header <- table_header_fill_missing(x$table_header, x$table_body)

  # converting update arg to a tidyselect list ---------------------------------
  update <-
    .formula_list_to_named_list(
      x = {{ update }},
      var_info = x$table_styling$header$column,
      arg_name = "update"
    )
  # if no columns selected, print helpful message
  if (identical(quiet, FALSE) && rlang::is_empty(update)) .modify_no_selected_vars(x)
  if (is.null(update)) return(x)

  # updating footnote ----------------------------------------------------------
  footnote_column_name <- ifelse(abbreviation == TRUE, "footnote_abbrev", "footnote")

  # updating footnote ----------------------------------------------------------
  update <-
    unlist(update) %>%
    tibble::enframe(name = "column", value = "footnote") %>%
    dplyr::inner_join(.info_tibble(x), by = "column") %>%
    dplyr::rowwise() %>%
    mutate(
      updated_value = switch(is.na(.data$footnote), NA_character_) %||%
        as.character(glue(.data$footnote)),
    ) %>%
    ungroup() %>%
    select(.data$column, .data$updated_value) %>%
    tibble::deframe() %>%
    as.list()

  # updating footnotes ---------------------------------------------------------
  if (abbreviation == FALSE) {
    x <-
      modify_table_styling(
        x,
        columns = names(update),
        footnote = unlist(update),
        text_interpret = glue("gt::{text_interpret}") %>% as.character(),
        hide = FALSE
      )
  }
  else if (abbreviation == TRUE) {
    x <-
      modify_table_styling(
        x,
        columns = names(update),
        footnote_abbrev = unlist(update),
        text_interpret = glue("gt::{text_interpret}") %>% as.character(),
        hide = FALSE
      )
  }

  # returning gtsummary object -------------------------------------------------
  x
}

#' @name modify
#' @export
modify_spanning_header <- function(x, update = NULL,
                                   text_interpret = c("md", "html"), quiet = NULL) {
  # checking inputs ------------------------------------------------------------
  if (!inherits(x, "gtsummary")) {
    stop("Argument `x=` must be an object with 'gtsummary' class", call. = FALSE)
  }
  # setting defaults -----------------------------------------------------------
  quiet <- quiet %||% get_theme_element("pkgwide-lgl:quiet") %||% FALSE

  # update table_header --------------------------------------------------------
  x$table_header <- table_header_fill_missing(x$table_header, x$table_body)

  # converting update arg to a tidyselect list ---------------------------------
  update <-
    .formula_list_to_named_list(
      x = {{ update }},
      var_info = x$table_styling$header$column,
      arg_name = "update"
    )

  # if no columns selected, print helpful message
  if (identical(quiet, FALSE) && rlang::is_empty(update)) .modify_no_selected_vars(x)
  if (is.null(update)) return(x)

  # updating spanning header ---------------------------------------------------
  df_update <-
    update %>%
    unlist() %>%
    tibble::enframe(name = "column", value = "spanning_header") %>%
    dplyr::inner_join(.info_tibble(x), by = "column") %>%
    # if users passes incorrect colname via `...` removing it
    dplyr::inner_join(select(x$table_styling$header, .data$column), by = "column") %>%
    dplyr::rowwise() %>%
    mutate(
      spanning_header = switch(is.na(.data$spanning_header), NA_character_) %||%
        as.character(glue(.data$spanning_header)),
    ) %>%
    ungroup() %>%
    select(.data$column, .data$spanning_header)

  x$table_styling$header <-
    x$table_styling$header %>%
    dplyr::rows_update(df_update, by = "column")

  # return updated gtsummary object --------------------------------------------
  x
}

#' @name modify
#' @export
modify_caption <- function(x, caption, text_interpret = c("md", "html")) {
  # checking inputs ------------------------------------------------------------
  if (!inherits(x, "gtsummary")) abort("`x=` must be class 'gtsummary'.")
  if (!rlang::is_string(caption)) abort("`caption=` must be a string.")
  text_interpret <- match.arg(text_interpret)

  # first interpreting caption in case user passed glue args -----------------
  caption <-
    .info_tibble(x) %>%
    filter(.data$column == "label") %>%
    with(glue(caption)) %>%
    as.character()

  # adding caption to gtsummary object ----------------------------------------
  x$list_output$caption <- caption
  attr(x$list_output$caption, "text_interpret") <- text_interpret

  # returning updated object ---------------------------------------------------
  x
}

#' @name modify
#' @export
show_header_names <- function(x = NULL, quiet = NULL) {
  # setting defaults -----------------------------------------------------------
  quiet <- quiet %||% get_theme_element("pkgwide-lgl:quiet") %||% FALSE

  # checking input -------------------------------------------------------------
  if (!inherits(x, "gtsummary"))
    stop("Pass a 'gtsummary' object in `x=` to print current column names and headers.")

  df_cols <- x$table_styling$header %>%
    filter(.data$hide == FALSE) %>%
    select(.data$column, .data$label)

  if (identical(quiet, FALSE)) {
    cat("\n")
    usethis::ui_info("As a usage guide, the code below re-creates the current column headers.")
    block <- mutate(df_cols, formula = glue("  {column} ~ {shQuote(label)}")) %>%
      pull(.data$formula) %>%
      paste0("", collapse = ",\n") %>%
      {glue("modify_header(update = list(\n{.}\n))")}

    ui_code_block(block)

    knitr::kable(df_cols, col.names = c("Column Name", "Column Header"), format = "pandoc") %>%
      print()
  }

  return(invisible(df_cols))
}

# prints a helpful message when no columns were selected in the modify functions
.modify_no_selected_vars <- function(x) {
  paste("No columns were selected.",
        "Use {ui_code('quiet = TRUE')} to supress these messages.") %>%
    stringr::str_wrap() %>%
    usethis::ui_info()

  show_header_names(x)
}

.info_tibble <- function(x) {
  # tbl_summary with no by variable
  if (inherits(x, c("tbl_summary", "tbl_svysummary")) && is.null(x$df_by)) {
    return(
      x$meta_data %>%
        dplyr::slice(1) %>%
        pluck("df_stats", 1) %>%
        select(any_of(c("N_obs", "N_unweighted"))) %>%
        dplyr::slice(1) %>%
        dplyr::rename(N = .data$N_obs) %>%
        full_join(
          select(x$table_styling$header, .data$column),
          by = character()
        )
    )
  }

  # tbl_summary with by variable
  if (inherits(x, c("tbl_summary", "tbl_svysummary")) && !is.null(x$df_by)) {
    return(
      x$table_styling$header %>%
        select(.data$column) %>%
        full_join(
          x$df_by %>%
            select(any_of(c("N",  "N_unweighted"))) %>%
            distinct(),
          by = character()
        ) %>%
        left_join(
          x$df_by %>%
            select(column = .data$by_col, level = .data$by_chr,
                   any_of(c("n", "p", "n_unweighted", "p_unweighted"))),
          by = "column"
        )
    )
  }

  # otherwise return tibble with N
  x$table_styling$header %>%
    select(.data$column) %>%
    mutate(N = x$N %||% x$n %||% NA_integer_,
           # in V1.3.6, all documentation about {n} being supported was removed. This can be removed eventually
           n = .data$N)
}

