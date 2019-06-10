#' Merge two or more gtsummary regression objects
#'
#' Merges two or more `tbl_regression` or `tbl_uvregression` objects and adds appropriate spanning headers.
#'
#' @param tbls list of gtsummary regression objects
#' @param tab_spanner Spanning headers. Character vector with same length as `tbls`
#' @family tbl_regression tools
#' @family tbl_uvregression tools
#' @seealso [tbl_stack]
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
#' @section Example Output:
#' \if{html}{\figure{tbl_merge_ex.png}{options: width=50\%}}
#'
tbl_merge <- function(tbls,
                      tab_spanner = paste0(c("Model "), 1:length(tbls))) {
  # input checks ---------------------------------------------------------------
  # checking all inputs are class tbl_regression or tbl_uvregression
  if (!map_chr(tbls, class) %in% c("tbl_regression", "tbl_uvregression") %>% any()) {
    stop("All objects in 'tbls' must be class 'tbl_regression' or 'tbl_uvregression'")
  }

  # at least two objects must be passed
  tbls_length <- length(tbls)
  if (tbls_length < 1) stop("Supply 2 or more gtsummary regression objects to 'tbls ='")

  # length of cpannign header matches number of models passed
  if (tbls_length != length(tab_spanner)) {
    stop("'tbls' and 'tab_spanner' must be the same length")
  }

  # merging tables -------------------------------------------------------------
  # nesting data by variable (one line per variable), and renaming columns with number suffix
  nested_table <-
    tbls %>%
    imap(
      function(x, y) {
        pluck(x, "table_body") %>%
          rename_at(
            vars(-c("variable", "var_type", "row_type", "label")),
            ~ glue("{.}_{y}")
          ) %>%
          nest(-c("variable", "var_type"))
      }
    )

  # merging formatted objects together
  merged_table <-
    nested_table[[1]] %>%
    rename(
      table = .data$data
    )

  # cycling through all tbls, merging results into a column tibble called table
  for (i in 2:tbls_length) {
    merged_table <-
      merged_table %>%
      full_join(nested_table[[i]], by = c("variable", "var_type")) %>%
      mutate(
        table = map2(
          .data$table, .data$data,
          function(table, data) {
            if (is.null(table)) return(data)
            if (is.null(data)) return(table)
            full_join(table, data, by = c("row_type", "label"))
          }
        )
      ) %>%
      select(-c("data"))
  }

  # unnesting results from within variable column tibbles
  table_body <-
    merged_table %>%
    unnest()


  # creating column headers and footnotes --------------------------------------
  # creating column header for base variable (label, estimate, conf.low, and p.value)
  cols_label_base_vars <-
    imap_chr(
      tbl_inputs(tbls),
      ~ glue(
        "estimate_{.y} = md('**{estimate_header(.x$x, .x$exponentiate)}**'), ",
        "conf.low_{.y} = md('**{.x$conf.level*100}% CI**'), ",
        "p.value_{.y} = md('**p-value**')"
      )
    ) %>%
    glue_collapse(sep = ", ") %>%
    glue(", label = md('**Characteristic**')")

  # creating list of footnote information
  footnote_abbreviation <- list()
  footnote_abbreviation[["footnote"]] <-
    imap(
      tbl_inputs(tbls),
      ~ estimate_header(.x$x, .x$exponentiate) %>% attr("footnote")
    ) %>%
    unique() %>%
    compact() %>%
    unlist() %>%
    c("CI = Confidence Interval") %>%
    glue_collapse(sep = ", ")

  footnote_abbreviation[["columns"]] <-
    imap_lgl(
      tbl_inputs(tbls),
      ~ estimate_header(.x$x, .x$exponentiate) %>%
        attr("footnote") %>%
        {
          !is.null(.)
        }
    ) %>%
    which() %>%
    # if any abbreviations, include estimate_{i} in abbreviation list, otherwise return NULL
    {
      switch(length(.) > 0 + 1, paste0("estimate_", .), NULL)
    } %>%
    c(paste0("conf.low_", 1:tbls_length)) %>%
    glue_collapse(sep = ", ")

  # returning results
  results <- list(
    table_body = table_body,
    estimate_funs = map(tbl_inputs(tbls), pluck("estimate_fun")),
    pvalue_funs = map(tbl_inputs(tbls), pluck("pvalue_fun")),
    qvalue_funs = map(tbls, pluck("qvalue_fun")),
    call_list = list(tbl_merge = match.call()),
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
    "cols_hide(columns = starts_with('row_ref_'))",
    "cols_hide(columns = starts_with('N_'))",
    "cols_hide(columns = starts_with('nevent_'))"
  ) %>%
    glue_collapse(sep = " %>% "),

  # NAs do not show in table
  fmt_missing = "fmt_missing(columns = everything(), missing_text = '')" %>%
    glue(),

  # Show "---" for reference groups
  # e.g.fmt_missing(columns = vars(estimate_2, conf.low_2, conf.high_2), rows = (variable == 'trt' & row_type == 'level' & label == \'Drug\'), missing_text = '---') %>%
  #     fmt_missing(columns = vars(estimate_2, conf.low_2, conf.high_2), rows = (variable == 'grade' & row_type == 'level' & label == \'I\'), missing_text = '---')
  fmt_missing_ref =
    imap(
      tbls,
      function(x, y) {
        cat_var <-
          pluck(x, "table_body") %>%
          filter(
            .data$var_type == "categorical",
            .data$row_ref == TRUE,
            .data$row_type == "level"
          ) %>%
          select(c("variable", "row_ref"))

        if (nrow(cat_var) == 0) return(NULL)
        map(
          cat_var$variable,
          ~ glue(
            "fmt_missing(",
            "columns = vars({paste(c('estimate', 'conf.low', 'conf.high'), y, sep = '_', collapse = ', ')}), ",
            "rows = (variable == '{.x}' & row_type == 'level' & row_ref_{y} == TRUE), ",
            "missing_text = '---')"
          )
        )
      }
    ) %>%
      unlist() %>%
      compact() %>%
      glue_collapse_null(),

  # glue("estimate_{1:tbls_length}, conf.low_{1:tbls_length}, conf.high_{1:tbls_length}") %>%
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
      ~ glue("fmt(columns = vars(p.value_{.x}), rows = !is.na(p.value_{.x}), fns = x$pvalue_funs[[{.x}]])")
    ) %>%
      glue_collapse(" %>% "),

  # ceof and confidence interval formatting
  fmt_estimate =
    map(
      1:tbls_length,
      ~ paste0(c("estimate_", "conf.low_", "conf.high_"), .x, collapse = ", ") %>% {
        glue("fmt(columns = vars({.}), rows = !is.na(estimate_{.x}), fns = x$estimate_funs[[{.x}]])")
      }
    ) %>%
      glue_collapse(" %>% "),

  # combining conf.low and conf.high to print confidence interval
  # example result:
  # cols_merge(col_1 = vars(conf.low_1), col_2 = vars(conf.high_1), pattern = '{1}, {2}') %>%
  #   cols_merge(col_1 = vars(conf.low_2), col_2 = vars(conf.high_2), pattern = '{1}, {2}')
  cols_merge_ci =
    map(
      1:tbls_length,
      ~ paste0(
        "cols_merge(",
        glue("col_1 = vars(conf.low_{.x}), col_2 = vars(conf.high_{.x}), "),
        "pattern = '{1}, {2}')"
      )
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
          if (is.null(x)) return(NULL)
          glue("fmt(columns = vars(q.value_{y}), rows = !is.na(q.value_{y}), fns = x$qvalue_funs[[{y}]])")
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
          if (is.null(x)) return(NULL)
          glue("cols_label(q.value_{y} = md('**q-value**'))")
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
          if (is.null(x)) return(tibble(i = y, method = NA_character_, var = paste0("q.value_", i)))
          return(tibble(i = y, method = x, var = paste0("q.value_", i)))
        }
      ) %>%
      stats::na.omit() %>%
      left_join(add_q_method_lookup, by = "method") %>%
      group_by(method_label) %>%
      nest() %>%
      mutate(
        var_list = map_chr(
          data,
          ~ .x$var %>% paste(collapse = ", ")
        ),
        gt_call = glue(
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
      if (class(tbl) == "tbl_regression") {
        return(pluck(tbl, "inputs"))
      }
      if (class(tbl) == "tbl_uvregression") {
        return(pluck(tbl, "tbl_regression_list", 1, "inputs"))
      }
    }
  )
}

# this function is glue_collapse, but returns NULL is passed nothing
glue_collapse_null <- function(x, sep = " %>% ") {
  if (length(x) == 0) return(NULL)
  glue_collapse(x, sep)
}
