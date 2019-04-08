#' Merge two or more gtsummary regression objects
#'
#' The `tbl_merge` function merges two or more `tbl_regression` or
#' `tbl_uvregression` objects and adds appropriate spanning headers.
#' @param tbls list of gtsummary regression objects
#' @param tab_spanner Spanning headers.  Vector with same length as `tbls`
#' @export
#' @examples
#' library(survival)
#' t1 <-
#'   glm(response ~ trt + grade + age, trial, family = binomial) %>%
#'   tbl_regression(
#'     label = list(trt = "Treatment", grade = "Grade", age = "Age"),
#'     exponentiate = TRUE
#'   )
#' t2 <-
#'   coxph(Surv(ttdeath, death) ~ trt + grade + age, trial) %>%
#'   tbl_regression(
#'     label = list(trt = "Treatment", grade = "Grade", age = "Age"),
#'     exponentiate = TRUE
#'   )
#' t3 <-
#'   tbl_merge(
#'     tbls = list(t1, t2),
#'     tab_spanner = c("Tumor Response", "Time to Death")
#'   )

tbl_merge <- function(tbls,
                      tab_spanner = paste0(c("Model "), 1:length(tbls))) {
  # input checks ---------------------------------------------------------------
  # checking all inputs are class tbl_regression or tbl_uvregression
  if(!map_chr(tbls, class) %in% c("tbl_regression", "tbl_uvregression") %>% any()) {
    stop("All objects in 'tbls' must be class 'tbl_regression' or 'tbl_uvregression'")
  }

  # at least two objects must be passed
  tbls_length <- length(tbls)
  if(tbls_length <  1) stop("Supply 2 or more gtsummary regression objects to 'tbls ='")

  # length of cpannign header matches number of models passed
  if (tbls_length  != length(tab_spanner)) {
    stop("'tbls' and 'tab_spanner' must be the same length")
  }

  # merging tables -------------------------------------------------------------
  table_body <-
    tbls[[1]]$table_body %>%
    dplyr::rename_at(
      dplyr::vars(-c("variable", "var_type", "row_type", "label")),
      ~glue::glue("{.}_1")
    )

  for(i in 1:(tbls_length - 1)) {
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

  # creating column headers and footnotes --------------------------------------
  # creating column header for base variable (label, coef, ll, and pvalue)
  cols_label_base_vars <-
    imap_chr(
      tbl_inputs(tbls),
      ~glue(
        "coef_{.y} = md('**{coef_header(.x$x, .x$exponentiate)}**'), ",
        "ll_{.y} = md('**{.x$conf.level*100}% CI**'), ",
        "pvalue_{.y} = md('**p-value**')"
      )
    ) %>%
    glue::glue_collapse(sep = ", ") %>%
    glue::glue(", label = md('**Characteristic**')")

  # creating list of footnote information
  footnote_abbreviation <- list()
  footnote_abbreviation[["footnote"]] <-
    imap_chr(
      tbl_inputs(tbls),
      ~coef_header(.x$x, .x$exponentiate) %>% attr("footnote")
    ) %>%
    unique() %>%
    c("CI = Confidence Interval") %>%
    glue::glue_collapse(sep = ", ")

  footnote_abbreviation[["columns"]] <-
    purrr::imap_lgl(
      tbl_inputs(tbls),
      ~coef_header(.x$x, .x$exponentiate) %>% attr("footnote") %>% {!is.null(.)}
    ) %>%
    which() %>%
    {paste0("coef_", .)} %>%
    c(paste0("ll_", 1:tbls_length)) %>%
    glue::glue_collapse(sep = ", ")

  # returning results
  results <- list(
    table_body = table_body,
    coef_funs = map(tbl_inputs(tbls), pluck("coef_fun")),
    pvalue_funs = map(tbl_inputs(tbls), pluck("pvalue_fun")),
    gt_calls = eval(gt_tbl_merge)
  )

  class(results) <- "tbl_merge"
  results
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
  cols_hide = glue(
    "cols_hide(columns = vars(variable, row_type, var_type, ",
    # making list of N_* columns, e.g.g N_1, N_2...not sure why starts_with('N_') doesn't work?
    "{paste0('N_', 1:tbls_length) %>% paste(collapse = ', ')}",
    "))"
  ),

  # NAs do not show in table
  fmt_missing = "fmt_missing(columns = everything(), missing_text = '')" %>%
    glue(),

  # Show "---" for reference groups
  # fmt_missing_ref =
  #   "fmt_missing(columns = vars(coef, ll, ul), rows = row_type == 'level', missing_text = '---')" %>%
  #   glue(),

  # column headers
  cols_label = glue("cols_label({cols_label_base_vars})"),

  # column headers abbreviations footnote
  footnote_abbreviation = glue(
    "tab_footnote(",
    "footnote = '{footnote_abbreviation$footnote}',",
    "locations = cells_column_labels(",
    "columns = vars({footnote_abbreviation$columns}))",
    ")"
  ),

  # adding p-value formatting (evaluate the expression with eval() function)
  fmt_pvalue =
    map(
      1:tbls_length,
      ~glue("fmt(columns = vars(pvalue_{.x}), rows = !is.na(pvalue_{.x}), fns = x$pvalue_funs[[{.x}]])")
    ) %>%
    glue::glue_collapse(" %>% "),

  # ceof and confidence interval formatting
  fmt_coef =
    map(
      1:tbls_length,
      ~paste0(c("coef_", "ll_", "ul_"), .x, collapse = ", ") %>%
      {glue("fmt(columns = vars({.}), rows = !is.na(coef_{.x}), fns = x$coef_funs[[{.x}]])")}
    ) %>%
    glue::glue_collapse(" %>% "),

  # combining ll and ul to print confidence interval
  # example result:
  # cols_merge(col_1 = vars(ll_1), col_2 = vars(ul_1), pattern = '{1}, {2}') %>%
  #   cols_merge(col_1 = vars(ll_2), col_2 = vars(ul_2), pattern = '{1}, {2}')
  cols_merge_ci =
    purrr::map(
      1:tbls_length,
      ~paste0(
        "cols_merge(",
        glue::glue("col_1 = vars(ll_{.x}), col_2 = vars(ul_{.x}), "),
        "pattern = '{1}, {2}')")
    ) %>%
    glue::glue_collapse(" %>% "),

  # indenting levels and missing rows
  tab_style_text_indent = glue(
    "tab_style(",
    "style = cells_styles(text_indent = px(10), text_align = 'left'),",
    "locations = cells_data(",
    "columns = vars(label),",
    "rows = row_type != 'label'",
    "))"
  ),

  # table spanner
  tab_spanner =
    glue::glue(
      "tab_spanner(label = '{tab_spanner}', columns = ends_with('_{1:tbls_length}'))"
    ) %>%
    glue::glue_collapse(sep = " %>% ")
))



# this function grabs the inputs from each of the input models
# for univariate tbls, taking the inputs from the first model
tbl_inputs <- function(tbl) {
  map(
    tbl,
    function(tbl) {
      if (class(tbl) == "tbl_regression")
        return(pluck(tbl, "inputs"))
      if (class(tbl) == "tbl_uvregression")
        return(pluck(tbl, "tbl_regression_list", 1, "inputs"))
    }
  )
}
