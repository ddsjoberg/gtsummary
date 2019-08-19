#' Merge two or more gtsummary regression objects
#'
#' Merges two or more `tbl_regression`, `tbl_uvregression`, or `tbl_stack`
#' objects and adds appropriate spanning headers.
#'
#' @param tbls List of gtsummary regression objects
#' @param tab_spanner Character vector specifying the spanning headers.
#' Must be same length as `tbls`argument
#' @family tbl_regression tools
#' @family tbl_uvregression tools
#' @seealso [tbl_stack]
#' @author Daniel D. Sjoberg
#' @export
#' @return A `tbl_merge` object
#' @examples
#' library(survival)
#' t1 <-
#'   glm(response ~ trt + grade + age, trial, family = binomial) %>%
#'   tbl_regression(exponentiate = TRUE)
#' t2 <-
#'   coxph(Surv(ttdeath, death) ~ trt + grade + age, trial) %>%
#'   tbl_regression(exponentiate = TRUE)
#' tbl_merge_ex <-
#'   tbl_merge(
#'     tbls = list(t1, t2),
#'     tab_spanner = c("Tumor Response", "Time to Death")
#'   )
#' @section Example Output:
#' \if{html}{\figure{tbl_merge_ex.png}{options: width=70\%}}
#'
tbl_merge <- function(tbls,
                      tab_spanner = paste0(c("Model "), seq_len(length(tbls)))) {
  # input checks ---------------------------------------------------------------
  # class of tbls
  if (!"list" %in% class(tbls)) {
    stop("Expecting 'tbls' to be a list, e.g. 'tbls = list(tbl1, tbl2)'")
  }

  # checking all inputs are class tbl_regression, tbl_uvregression, or tbl_stack
  if (!map_chr(tbls, class) %in% c("tbl_regression", "tbl_uvregression", "tbl_stack") %>% any()) {
    stop("All objects in 'tbls' must be class 'tbl_regression', 'tbl_uvregression', or 'tbl_stack'")
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
  nested_table <- tbls %>%
    map("table_body") %>%
    imap(function(x, y) {
      rename_at(
        x,
        vars(-c("variable", "var_type", "row_type", "label")),
        ~ glue("{.}_{y}")
      )
    })

  # THIS IS FROM PR #195, the if-else won't be required when tidyr >=1.0.0 is deps
  if (tidyr_has_legacy_nest()) {
    nested_table <- map(
      nested_table,
      ~ nest(.x, data = -one_of(c("variable", "var_type")))
    )
  } else {
    nested_table <- map(
      nested_table,
      ~ nest(.x, -c("variable", "var_type"))
    )
  }

  # merging formatted objects together
  merged_table <-
    nested_table[[1]] %>%
    rename(
      table = .data$data
    )

  # cycling through all tbls, merging results into a column tibble
  for (i in 2:tbls_length) {
    merged_table <-
      merged_table %>%
      full_join(nested_table[[i]], by = c("variable", "var_type")) %>%
      mutate(
        table = map2(
          .data$table, .data$data,
          function(table, data) {
            if (is.null(table)) {
              return(data)
            }
            if (is.null(data)) {
              return(table)
            }
            full_join(table, data, by = c("row_type", "label"))
          }
        )
      ) %>%
      select(-c("data"))
  }

  # unnesting results from within variable column tibbles
  # THIS IS FROM PR #195, the if-else won't be required when tidyr >=1.0.0 is deps
  if (tidyr_has_legacy_nest()) {
    table_body <-
      unnest(merged_table, "table") %>%
      select(.data$label, everything())
  } else {
    table_body <-
      unnest(merged_table) %>%
      select(.data$label, everything())
  }

  # stacking all table_header dfs together and renaming
  table_header <-
    imap_dfr(
      tbls,
      ~ pluck(.x, "table_header") %>%
        mutate(
          column = paste0(column, "_", .y),
          fmt = stringr::str_replace(fmt, stringr::fixed("x$"), paste0("x$tbls[[", .y, "]]$"))
        )
    )

  table_header <-
    tibble(column = names(table_body)) %>%
    left_join(table_header, by = "column") %>%
    table_header_fill_missing()

  # creating column footnotes --------------------------------------
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
    c(paste0("conf.low_", seq_len(tbls_length))) %>%
    glue_collapse(sep = ", ")

  # returning results
  results <- list(
    table_body = table_body,
    table_header = table_header,
    tbls = tbls,
    estimate_funs = map(tbl_inputs(tbls), pluck("estimate_fun")),
    pvalue_funs = map(tbl_inputs(tbls), pluck("pvalue_fun")),
    qvalue_funs = map(tbls, pluck("qvalue_fun")),
    call_list = list(tbl_merge = match.call()),
    gt_calls = eval(gt_tbl_merge),
    kable_calls = eval(kable_tbl_merge)
  )

  # setting column headers
  results <- modify_header_internal(
    results,
    label = "**Characteristic**",
  )

  # writing additional gt and kable calls with data from table_header
  results <- update_calls_from_table_header(results)

  class(results) <- "tbl_merge"
  results
}

# gt function calls ------------------------------------------------------------
# quoting returns an expression to be evaluated later
gt_tbl_merge <- quote(list(
  # first call to the gt function
  gt = "gt::gt(data = x$table_body)" %>%
    glue(),

  # label column indented and left just
  cols_align = glue(
    "gt::cols_align(align = 'center') %>% ",
    "gt::cols_align(align = 'left', columns = vars(label))"
  ),

  # NAs do not show in table
  fmt_missing = "gt::fmt_missing(columns = gt::everything(), missing_text = '')" %>%
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

        if (nrow(cat_var) == 0) {
          return(NULL)
        }
        map(
          cat_var$variable,
          ~ glue(
            "gt::fmt_missing(",
            "columns = gt::vars({paste(c('estimate', 'conf.low', 'conf.high'), y, sep = '_', collapse = ', ')}), ",
            "rows = (variable == '{.x}' & row_type == 'level' & row_ref_{y} == TRUE), ",
            "missing_text = '---')"
          )
        )
      }
    ) %>%
      unlist() %>%
      compact() %>%
      glue_collapse_null(),

  # column headers abbreviations footnote
  footnote_abbreviation = glue(
    "gt::tab_footnote(",
    "footnote = '{footnote_abbreviation$footnote}',",
    "locations = gt::cells_column_labels(",
    "columns = gt::vars({footnote_abbreviation$columns}))",
    ")"
  ),

  # combining conf.low and conf.high to print confidence interval
  # example result:
  # cols_merge(col_1 = vars(conf.low_1), col_2 = vars(conf.high_1), pattern = '{1}, {2}') %>%
  #   cols_merge(col_1 = vars(conf.low_2), col_2 = vars(conf.high_2), pattern = '{1}, {2}')
  cols_merge_ci =
    map(
      seq_len(tbls_length),
      ~ paste0(
        "gt::cols_merge(",
        glue("col_1 = gt::vars(conf.low_{.x}), col_2 = gt::vars(conf.high_{.x}), "),
        "pattern = '{1}, {2}')"
      )
    ) %>%
      glue_collapse(" %>% "),

  # indenting levels and missing rows
  tab_style_text_indent = glue(
    "gt::tab_style(",
    "style = gt::cell_text(indent = gt::px(10), align = 'left'),",
    "locations = gt::cells_data(",
    "columns = gt::vars(label),",
    "rows = row_type != 'label'",
    "))"
  ),

  # table spanner
  tab_spanner =
    glue(
      "gt::tab_spanner(label = '{tab_spanner}', columns = gt::ends_with('_{seq_len(tbls_length)}'))"
    ) %>%
      glue_collapse(sep = " %>% "),

  # qvalue method footnote
  footnote_q_method =
    tbls %>%
      map(pluck("qvalue_method")) %>%
      imap_dfr(
        function(x, y) {
          if (is.null(x)) {
            return(tibble(i = y, method = NA_character_, var = paste0("q.value_", i)))
          }
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
          "gt::tab_footnote(",
          "footnote = '{method_label}', ",
          "locations = gt::cells_column_labels(",
          "columns = gt::vars({var_list}))",
          ")"
        )
      ) %>%
      pull("gt_call") %>%
      glue_collapse_null(sep = " %>% ")
))

# kable function calls ------------------------------------------------------------
# quoting returns an expression to be evaluated later
kable_tbl_merge <- quote(list(
  # first call to the gt function
  kable = glue("x$table_body"),

  #  placeholder, so the formatting calls are performed other calls below
  fmt = NULL,

  # combining conf.low and conf.high to print confidence interval
  cols_merge_ci =
    map(
      seq_len(tbls_length),
      ~ "dplyr::mutate(conf.low_{.x} = ifelse(is.na(estimate_{.x}), NA, paste0(conf.low_{.x}, \", \", conf.high_{.x})) %>% as.character())" %>%
        glue()
    ) %>%
      glue_collapse_null(sep = " %>% "),

  # Show "---" for reference groups
  fmt_missing_ref =
    map(
      seq_len(tbls_length),
      ~ glue(
        "dplyr::mutate_at(dplyr::vars(estimate_{.x}, conf.low_{.x}), ",
        "~ dplyr::case_when(row_ref_{.x} == TRUE ~ '---', TRUE ~ .))"
      )
    ) %>%
      glue_collapse_null(sep = " %>% ")
))



# this function grabs the inputs from each of the input models
# for univariate tbls, taking the inputs from the first model
tbl_inputs <- function(tbl) {
  map(
    tbl,
    function(tbl) {
      if (class(tbl) %in% c("tbl_regression", "tbl_stack")) {
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
  if (length(x) == 0) {
    return(NULL)
  }
  glue_collapse(x, sep)
}
