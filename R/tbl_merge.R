#' Merge two or more gtsummary regression objects
#'
#' The `tbl_merge` function merges two or more `tbl_regression` or
#' `tbl_uvregression` objects and adds appropriate spanning headers.
#' @param tbls list of gtsummary regression objects
#' @param header_span Spanning headers.  Vector with same length as `tbls`
#' @export

tbl_merge <- function(tbls, header_span = NULL) {

  # input checks ---------------------------------------------------------------
  # checking all inputs are class tbl_regression or tbl_uvregression
  if(!map_chr(tbls, class) %in% c("tbl_regression", "tbl_uvregression") %>% any()) {
    stop("All objects in 'tbls' must be class 'tbl_regression' or 'tbl_uvregression'")
  }

  # at least two objects must be passed
  if(length(tbls) <  1) stop("Supply 2 or more gtsummary regression objects to 'tbls ='")

  # merging tables -------------------------------------------------------------
  table_body <-
    tbls[[1]]$table_body %>%
    dplyr::rename_at(
      dplyr::vars(-c("variable", "var_type", "row_type", "label")),
      ~glue::glue("{.}_1")
    )

  for(i in 1:length(tbls[-1])) {
    table_body <-
      table_body %>%
      full_join(
        tbls[[i + 1]]$table_body %>%
          dplyr::rename_at(
            dplyr::vars(-c("variable", "var_type", "row_type", "label")),
            ~glue::glue("{.}_{i + 1}")
          ),
        by = c("variable", "var_type", "row_type", "label")
      )
  }

  table_body
}

# gt function calls ------------------------------------------------------------
# quoting returns an expression to be evaluated later
gt_tbl_merge <- quote(list(
  # first call to the gt function
  gt = "gt(data = x$table_body)" %>%
    glue(),

  # label column indented and left just
  gt_calls = glue(
    "cols_align(align = 'center') %>% ",
    "cols_align(align = 'left', columns = vars(label))"
  ),

  # do not print columns variable or row_type columns
  cols_hide = "cols_hide(columns = vars(variable, row_type, var_type, N))" %>%
    glue(),

  # NAs do not show in table
  fmt_missing = "fmt_missing(columns = everything(), missing_text = '')" %>%
    glue(),

  # Show "---" for reference groups
  fmt_missing_ref =
    "fmt_missing(columns = vars(coef, ll, ul), rows = row_type == 'level', missing_text = '---')" %>%
    glue(),

  # column headers
  cols_label = glue(
    "cols_label(",
    "label = md('**N = {n}**'), ",
    "coef = md('**{coef_header(x, exponentiate)}**'), ",
    "ll = md('**{style_percent(conf.level, symbol = TRUE)} CI**'), ",
    "pvalue = md('**p-value**')",
    ")"
  ),

  # column headers abbreviations footnote
  footnote_abbreviation = glue(
    "tab_footnote(",
    "footnote = '{footnote_abbr}',",
    "locations = cells_column_labels(",
    "columns = {footnote_location})",
    ")"
  ),

  # adding p-value formatting (evaluate the expression with eval() function)
  fmt_pvalue =
    "fmt(columns = vars(pvalue), rows = !is.na(pvalue), fns = x$inputs$pvalue_fun)" %>%
    glue(),

  # ceof and confidence interval formatting
  fmt_coef =
    "fmt(columns = vars(coef, ll, ul), rows = !is.na(coef), fns = x$inputs$coef_fun)" %>%
    glue(),

  # combining ll and ul to print confidence interval
  cols_merge_ci =
    "cols_merge(col_1 = vars(ll), col_2 = vars(ul), pattern = '{1}, {2}')" %>%
    glue::as_glue(),

  # indenting levels and missing rows
  tab_style_text_indent = glue(
    "tab_style(",
    "style = cells_styles(text_indent = px(10), text_align = 'left'),",
    "locations = cells_data(",
    "columns = vars(label),",
    "rows = row_type != 'label'",
    "))"
  )
))

