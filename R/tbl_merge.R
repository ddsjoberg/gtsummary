#' Merge two or more gtsummary regression objects
#'
#' The `tbl_merge` function merges two or more `tbl_regression` or
#' `tbl_uvregression` objects and adds appropriate spanning headers.
#'
#' @param tbls list of gtsummary regression objects
#' @param tab_spanner Spanning headers.  Vector with same length as `tbls`
#' @author Daniel D. Sjoberg
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
#' tbl_merge_ex <-
#'   tbl_merge(
#'     tbls = list(t1, t2),
#'     tab_spanner = c("Tumor Response", "Time to Death")
#'   )
#' @section Figures:
#' \if{html}{\figure{tbl_merge_ex.png}{options: width=50\%}}
#'
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
    rename_at(
      dplyr::vars(-c("variable", "var_type", "row_type", "label")),
      ~glue("{.}_1")
    )

  for(i in 1:(tbls_length - 1)) {
    table_body <-
      table_body %>%
      full_join(
        tbls[[i + 1]]$table_body %>%
          rename_at(
            dplyr::vars(-c("variable", "var_type", "row_type", "label")),
            ~glue("{.}_{i + 1}")
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
   glue_collapse(sep = ", ") %>%
   glue(", label = md('**Characteristic**')")

  # creating list of footnote information
  footnote_abbreviation <- list()
  footnote_abbreviation[["footnote"]] <-
    imap(
      tbl_inputs(tbls),
      ~coef_header(.x$x, .x$exponentiate) %>% attr("footnote")
    ) %>%
    unique() %>%
    compact() %>%
    unlist() %>%
    c("CI = Confidence Interval") %>%
   glue_collapse(sep = ", ")

  footnote_abbreviation[["columns"]] <-
    imap_lgl(
      tbl_inputs(tbls),
      ~coef_header(.x$x, .x$exponentiate) %>%
        attr("footnote") %>%
        {!is.null(.)}
    ) %>%
    which() %>%
    # if any abbreviations, include coef_{i} in abbreviation list, otherwise return NULL
    {switch(length(.) > 0 +  1, paste0("coef_", .), NULL)} %>%
    c(paste0("ll_", 1:tbls_length)) %>%
   glue_collapse(sep = ", ")

  # returning results
  results <- list(
    table_body = table_body,
    coef_funs = map(tbl_inputs(tbls), pluck("coef_fun")),
    pvalue_funs = map(tbl_inputs(tbls), pluck("pvalue_fun")),
    qvalue_funs = map(tbls, pluck("qvalue_fun")),
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
  cols_hide = c(
      "cols_hide(columns = vars(variable, row_type, var_type))",
      "cols_hide(columns = starts_with('N_'))",
      "cols_hide(columns = starts_with('nevent_'))"
    ) %>%
    glue_collapse(sep = " %>% "),

  # NAs do not show in table
  fmt_missing = "fmt_missing(columns = everything(), missing_text = '')" %>%
    glue(),

  # Show "---" for reference groups
  # e.g.fmt_missing(columns = vars(coef_2, ll_2, ul_2), rows = (variable == 'trt' & row_type == 'level' & label == \'Drug\'), missing_text = '---') %>%
  #     fmt_missing(columns = vars(coef_2, ll_2, ul_2), rows = (variable == 'grade' & row_type == 'level' & label == \'I\'), missing_text = '---')
  fmt_missing_ref =
    purrr::imap(
      tbls,
      function(x, y) {
        cat_var <-
          purrr::pluck(x, "table_body") %>%
          dplyr::filter(.data$var_type == "categorical",
                        is.na(.data$coef),
                        .data$row_type == "level") %>%
          dplyr::select(c("variable", "label"))

        if(nrow(cat_var) == 0) return(NULL)
        purrr::map2(
          cat_var$variable, cat_var$label,
          ~glue(
            "fmt_missing(",
            "columns = vars({paste(c('coef', 'll', 'ul'), y, sep = '_', collapse = ', ')}), ",
            "rows = (variable == '{.x}' & row_type == 'level' & label == '{.y}'), ",
            "missing_text = '---')"
          )
        )
      }
    ) %>%
    unlist() %>%
    purrr::compact() %>%
    glue_collapse_null(),

   # glue("coef_{1:tbls_length}, ll_{1:tbls_length}, ul_{1:tbls_length}") %>%
   # glue_collapse(sep = ", ") %>%
   #  {glue("fmt_missing(columns = vars({.}), rows = row_type == 'level', missing_text = '---')")},

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
   glue_collapse(" %>% "),

  # ceof and confidence interval formatting
  fmt_coef =
    map(
      1:tbls_length,
      ~paste0(c("coef_", "ll_", "ul_"), .x, collapse = ", ") %>%
      {glue("fmt(columns = vars({.}), rows = !is.na(coef_{.x}), fns = x$coef_funs[[{.x}]])")}
    ) %>%
   glue_collapse(" %>% "),

  # combining ll and ul to print confidence interval
  # example result:
  # cols_merge(col_1 = vars(ll_1), col_2 = vars(ul_1), pattern = '{1}, {2}') %>%
  #   cols_merge(col_1 = vars(ll_2), col_2 = vars(ul_2), pattern = '{1}, {2}')
  cols_merge_ci =
    map(
      1:tbls_length,
      ~paste0(
        "cols_merge(",
       glue("col_1 = vars(ll_{.x}), col_2 = vars(ul_{.x}), "),
        "pattern = '{1}, {2}')")
    ) %>%
   glue_collapse(" %>% "),

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
   glue(
      "tab_spanner(label = '{tab_spanner}', columns = ends_with('_{1:tbls_length}'))"
    ) %>%
   glue_collapse(sep = " %>% "),

  # qvalue format
  fmt_qvalue =
    tbls %>%
    map(pluck("qvalue_fun")) %>%
    imap(
      function(x, y) {
        if(is.null(x)) return(NULL)
       glue("fmt(columns = vars(qvalue_{y}), rows = !is.na(qvalue_{y}), fns = x$qvalue_funs[[{y}]])")
      }
    ) %>%
    compact() %>%
    glue_collapse_null(" %>% "),

  # qvalue column header
  cols_label_qvalue =
    tbls %>%
    map(pluck("qvalue_fun")) %>%
    imap(
      function(x, y) {
        if(is.null(x)) return(NULL)
       glue("cols_label(qvalue_{y} = md('**q-value**'))")
      }
    ) %>%
    compact() %>%
    glue_collapse_null(" %>% "),

  # qvalue method footnote
  footnote_q_method =
    tbls %>%
    map(pluck("qvalue_method")) %>%
    imap_dfr(
      function(x, y) {
        if(is.null(x)) return(tibble(i = y, method = NA_character_, var = paste0("qvalue_", i)))
        return(tibble(i = y, method = x, var = paste0("qvalue_", i)))
      }
    ) %>%
    stats::na.omit() %>%
    left_join(add_q_method_lookup, by = "method") %>%
    group_by(method_label) %>%
    nest() %>%
    mutate(
      var_list = map_chr(
        data,
        ~.x$var %>% paste(collapse = ", ")
      ),
      gt_call =glue(
        "tab_footnote(",
        "footnote = '{method_label}', ",
        "locations = cells_column_labels(",
        "columns = vars({var_list}))",
        ")"
      )
    ) %>%
    pull("gt_call") %>%
    glue_collapse_null(sep = " %>% ")
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

# this function is glue_collapse, but returns NULL is passed nothing
glue_collapse_null <- function(x, sep = " %>% ") {
  if (length(x) == 0) return(NULL)
 glue_collapse(x, sep)
}
