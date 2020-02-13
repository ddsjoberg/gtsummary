# these functions are useful for workign with the `table_header` objects

# modifying table_header -------------------------------------------------------
# this function fills out table_header when there are missing cells
table_header_fill_missing <- function(table_header) {
  # table_header must be a tibble with the following columns with
  # at minimum a column named 'column'

  # label ----------------------------------------------------------------------
  if (!"label" %in% names(table_header)) {
    table_header$label <- NA_character_
  }

  # hide -----------------------------------------------------------------------
  if (!"hide" %in% names(table_header)) {
    table_header$hide <- TRUE
  }

  # text_interpret -------------------------------------------------------------
  if (!"text_interpret" %in% names(table_header)) {
    table_header$text_interpret <- "gt::md"
  }

  # fmt_fun --------------------------------------------------------------------
  if (!"fmt_fun" %in% names(table_header)) {
    table_header$fmt_fun <- list(NULL)
  }

  # bold -----------------------------------------------------------------------
  if (!"bold" %in% names(table_header)) {
    table_header$bold <- NA_real_
  }

  # footnote_abbrev ------------------------------------------------------------
  if (!"footnote_abbrev" %in% names(table_header)) {
    table_header$footnote_abbrev <- list(NULL)
  }

  # footnote -------------------------------------------------------------------
  if (!"footnote" %in% names(table_header)) {
    table_header$footnote <- list(NULL)
  }

  # filling in missing values with default -------------------------------------
  table_header <-
    table_header %>%
    mutate(
      label = coalesce(.data$label, .data$column),
      hide = coalesce(.data$hide, TRUE),
      text_interpret = coalesce(.data$text_interpret, "gt::md"),
      bold = coalesce(.data$bold, NA_real_)
    )

  table_header
}

table_header_fmt_fun <- function(table_header, ...) {
  # saving passed_dots arguments as a named list
  passed_dots <- list(...)

  # ordering the names to be the same as in table_header
  names_ordered <- table_header$column %>% intersect(names(passed_dots))
  passed_dots <- passed_dots[names_ordered]

  # browser()
  table_header_update <-
    tibble(
      column = table_header$column %>% intersect(names(passed_dots)),
      fmt_fun = passed_dots
    )

  # updating table_header
  table_header[
    table_header$column %in% table_header_update$column, # selecting rows
    c("column", "fmt_fun") # selecting columns
  ] <- table_header_update[c("column", "fmt_fun")]

  table_header
}

# creating gt calls from table_header ------------------------------------------
# gt table_header to gt fmt and bolding code
table_header_to_gt_fmt <- function(table_header) {

  # saving gt::fmt code in list
  fmt_code_vct <-
    table_header %>%
    filter(map_lgl(.data$fmt_fun, ~!is.null(.x))) %>%
    mutate(
      col_label_code =
        glue("gt::fmt(columns = gt::vars({column}), rows = !is.na({column}), fns = x$fmt_fun${column})")
    ) %>%
    pull("col_label_code")

  # saving code to bold pvalues
  bold_code_vct <-
    table_header %>%
    filter(!is.na(.data$bold)) %>%
    mutate(
      col_label_code =
        glue(
          "gt::tab_style(style = gt::cell_text(weight = 'bold'), ",
          "locations = gt::cells_body(columns = gt::vars({column}), ",
          "rows = {column} <= {bold}))"
        )
    ) %>%
    pull("col_label_code")

  # combining codes into single vector
  code_vct <- c(fmt_code_vct, bold_code_vct)

  if (length(code_vct) == 0) {
    return(NULL)
  }

  code_vct %>%
    glue_collapse(sep = " %>% ")
}

# gt table_header to gt cols_label code
table_header_to_gt_cols_label <- function(table_header) {
  table_header %>%
    filter(
      .data$hide == FALSE, # only label columns that are being printed
      .data$column != "level_label" # this column is a gt group variable
    ) %>%
    mutate(col_label_code = glue("{column} = gt::md(\"{label}\")")) %>%
    pull("col_label_code") %>%
    paste(collapse = ", ") %>%
    {
      glue("gt::cols_label({.})")
    }
}

# gt table_header to gt tab_footnote code
table_header_to_gt_tab_footnote <- function(table_header) {
  # initializing results
  tab_footnote_abbrev <- NULL
  tab_footnote <- NULL

  # convert abbreviations to a single footnote code
  footnote_abbrev <-
    table_header %>%
    unnest("footnote_abbrev")

  if (nrow(footnote_abbrev) > 0) {
    tab_footnote_abbrev <- glue(
      "gt::tab_footnote(",
      "footnote = '{paste(unique(footnote_abbrev$footnote_abbrev), collapse = \", \")}', ",
      "locations = gt::cells_column_labels(",
      "columns = gt::vars({paste(unique(footnote_abbrev$column), collapse = \", \")}))",
      ")"
    )
  }

  # convert footnotes into gt footnote code
  footnote <-
    table_header %>%
    unnest("footnote")

  if (nrow(footnote) > 0) {
    tab_footnote <-
      footnote %>%
      filter(!is.na(.data$footnote)) %>%
      mutate(
        tab_footnote = glue(
          "gt::tab_footnote(",
          "footnote = '{footnote}', ",
          "locations = gt::cells_column_labels(",
          "columns = gt::vars({column}))",
          ")"
        )
      ) %>%
      pull(.data$tab_footnote)
  }

  c(tab_footnote_abbrev, tab_footnote) %>%
    glue_collapse_null()
}

# gt table_header to gt cols_hide code
table_header_to_gt_cols_hide <- function(table_header) {
  table_header %>%
    filter(.data$hide == TRUE) %>%
    pull(.data$column) %>%
    glue_collapse(sep = ", ") %>%
    {
      glue("gt::cols_hide(columns = gt::vars({.}))")
    }
}


# creating kable calls from table_header ---------------------------------------
# kable table_header to gt cols_hide code
table_header_to_kable_cols_hide <- function(table_header) {
  table_header %>%
    filter(.data$hide == TRUE) %>%
    pull(.data$column) %>%
    {
      paste0("\"", ., "\"", collapse = ", ")
    } %>%
    {
      glue("dplyr::select(-c({.}))")
    }
}

# kable table_header to formatted columns and bolding code
table_header_to_kable_fmt <- function(table_header) {
  code_vct <-
    table_header %>%
    filter(map_lgl(.data$fmt_fun, ~!is.null(.x))) %>%
    mutate(
      col_code = case_when(
        is.na(bold) ~ glue("dplyr::mutate({column} = x$fmt_fun${column}({column}))"),
        TRUE ~ glue(
          "dplyr::mutate({column} = dplyr::case_when(",
          "{column} <= {bold} ~ paste0('__', x$fmt_fun${column}({column}), '__'), ",
          "TRUE ~ x$fmt_fun${column}({column})",
          "))"
        )
      )
    ) %>%
    pull("col_code")

  if (length(code_vct) == 0) {
    return(NULL)
  }

  code_vct %>%
    glue_collapse(sep = " %>% ")
}

# updating all codes from table_header -----------------------------------------
# function takes in a tbl object, and updates the
# gt function calls from the x$table_header
update_calls_from_table_header <- function(x) {
  # updating fmt_fun -----------------------------------------------------------
  x$fmt_fun <-
    x$table_header$fmt_fun %>%
    set_names(x$table_header$column) %>%
    compact()

  # gt calls -------------------------------------------------------------------
  x$gt_calls[["cols_hide"]] <- table_header_to_gt_cols_hide(x$table_header)

  x$gt_calls[["cols_label"]] <- table_header_to_gt_cols_label(x$table_header)

  x$gt_calls[["fmt"]] <- table_header_to_gt_fmt(x$table_header)

  x$gt_calls[["tab_footnote"]] <- table_header_to_gt_tab_footnote(x$table_header)

  # kable calls ----------------------------------------------------------------
  x$kable_calls[["cols_hide"]] <- table_header_to_kable_cols_hide(x$table_header)

  x$kable_calls[["fmt"]] <- table_header_to_kable_fmt(x$table_header)

  # moving the hide code to the last call
  cols_hide_loc <- which("cols_hide" == names(x$kable_calls))
  x$kable_calls <- c(x$kable_calls[-cols_hide_loc], x$kable_calls[cols_hide_loc])

  # returning gtsummary object
  x
}
