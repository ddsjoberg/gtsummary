#' Merge two or more gtsummary objects
#'
#' Merges two or more `tbl_regression`, `tbl_uvregression`, `tbl_stack`,
#' or `tbl_summary` objects and adds appropriate spanning headers.
#'
#' @param tbls List of gtsummary objects to merge
#' @param tab_spanner Character vector specifying the spanning headers.
#' Must be the same length as `tbls`. The
#' strings are interpreted with `gt::md`.
#' Must be same length as `tbls` argument
#' @family tbl_regression tools
#' @family tbl_uvregression tools
#' @family tbl_summary tools
#' @seealso [tbl_stack]
#' @author Daniel D. Sjoberg
#' @export
#' @return A `tbl_merge` object
#' @examples
#' # Side-by-side Regression Models
#' library(survival)
#' t1 <-
#'   glm(response ~ trt + grade + age, trial, family = binomial) %>%
#'   tbl_regression(exponentiate = TRUE)
#' t2 <-
#'   coxph(Surv(ttdeath, death) ~ trt + grade + age, trial) %>%
#'   tbl_regression(exponentiate = TRUE)
#' tbl_merge_ex1 <-
#'   tbl_merge(
#'     tbls = list(t1, t2),
#'     tab_spanner = c("**Tumor Response**", "**Time to Death**")
#'   )
#' \donttest{
#' # Descriptive statistics alongside univariate regression, with no spanning header
#' t3 <-
#'   trial %>%
#'   dplyr::select(age, grade, response) %>%
#'   tbl_summary(missing = "no") %>%
#'   add_n()
#' t4 <-
#'   tbl_uvregression(
#'     trial %>% dplyr::select(ttdeath, death, age, grade, response),
#'     method = coxph,
#'     y = Surv(ttdeath, death),
#'     exponentiate = TRUE,
#'     hide_n = TRUE
#'   )
#' tbl_merge_ex2 <-
#'   tbl_merge(tbls = list(t3, t4)) %>%
#'   as_gt(exclude = "tab_spanner") %>%
#'   gt::cols_label(stat_0_1 = gt::md("**Summary Statistics**"))
#' }
#' @section Example Output:
#' \if{html}{Example 1}
#'
#' \if{html}{\figure{tbl_merge_ex1.png}{options: width=70\%}}
#'
#' \if{html}{Example 2}
#'
#' \if{html}{\figure{tbl_merge_ex2.png}{options: width=65\%}}
#'
tbl_merge <- function(tbls, tab_spanner = NULL) {
  # input checks ---------------------------------------------------------------
  # if tab spanner is null, default is Table 1, Table 2, etc....
  if (is.null(tab_spanner)) {
    tab_spanner <-  paste0(c("**Table "), seq_len(length(tbls)), "**")
  }

  # class of tbls
  if (!"list" %in% class(tbls)) {
    stop("Expecting 'tbls' to be a list, e.g. 'tbls = list(tbl1, tbl2)'")
  }

  # checking all inputs are class tbl_regression, tbl_uvregression,
  # tbl_regression, tbl_summary, or tbl_stack
  if (!map_chr(tbls, class) %in%
      c("tbl_regression", "tbl_uvregression", "tbl_summary", "tbl_stack") %>% all()) {
    stop(paste("All objects in 'tbls' must be class 'tbl_regression',",
               "'tbl_uvregression', 'tbl_summary', or 'tbl_stack'"))
  }

  # at least two objects must be passed
  tbls_length <- length(tbls)
  if (tbls_length < 2) stop("Supply 2 or more gtsummary regression objects to 'tbls ='")

  # length of spanning header matches number of models passed
  if (tbls_length != length(tab_spanner)) {
    stop("'tbls' and 'tab_spanner' must be the same length")
  }

  # if previously called bold/italicize_labels/levels, print note to do it again
  style_funs <- c("bold_labels", "bold_levels", "italicize_labels", "italicize_levels")
  if (purrr::some(tbls, ~names(pluck(.x, "call_list")) %in% style_funs %>% any())) {
    message(glue::glue(
      'Styling functions ',
      '{glue::glue("`{style_funs}()`") %>% glue::glue_collapse(sep = ", ", last = ", and ")}',
      ' need to be re-applied after `tbl_merge()`.'
    ))
  }

  # for tbl_summary, moving footnote above label column to the first stat_* column
  tbls <- map_if(
    tbls,
    ~ class(.x) == "tbl_summary",
    function(x){
      x$table_header$footnote[startsWith(x$table_header$column, "stat_")] =
        x$table_header$footnote[x$table_header$column == "label"][1]

      x$table_header$footnote[x$table_header$column == "label"][1] = list(NULL)
      return(x)
    }
  )

  # merging tables -------------------------------------------------------------
  # nesting data by variable (one line per variable), and renaming columns with number suffix
  nested_table <- tbls %>%
    map("table_body") %>%
    imap(function(x, y) {
      # creating a column that is the variable label
      group_by(x, .data$variable) %>%
      mutate(
        var_label = ifelse(.data$row_type == "label", .data$label, NA)
      ) %>%
      tidyr::fill(.data$var_label, .direction = "downup") %>%
      ungroup() %>%
      rename_at(
        vars(-c("variable", "row_type", "var_label", "label")),
        ~ glue("{.}_{y}")
      )
    })

  # nesting results within variable
  nested_table <- map(
    nested_table,
    ~ nest(.x, data = -one_of(c("variable", "var_label")))
  )

  # merging formatted objects together
  merged_table <-
    nested_table[[1]] %>%
    rename(table = .data$data)

  # cycling through all tbls, merging results into a column tibble
  for (i in 2:tbls_length) {
    merged_table <-
      merged_table %>%
      full_join(
        nested_table[[i]],
        by = c("variable", "var_label")
      ) %>%
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
      select(-c("data", "table"), "table")
  }

  # unnesting results from within variable column tibbles
  table_body <-
    merged_table %>%
    select(-.data$var_label) %>%
    unnest("table") %>%
    select(.data$label, everything())

  # stacking all table_header dfs together and renaming
  table_header <-
    imap_dfr(
      tbls,
      ~ pluck(.x, "table_header") %>%
        mutate(
          column = paste0(.data$column, "_", .y),
          fmt = stringr::str_replace(fmt, stringr::fixed("x$"), paste0("x$tbls[[", .y, "]]$"))
        )
    ) %>%
    # using the identifying columns from first passed object
    mutate(
      column = case_when(
        .data$column == "label_1" ~ "label",
        .data$column == "variable_1" ~ "variable",
        .data$column == "row_type_1" ~ "row_type",
        TRUE ~ .data$column
      )
    ) %>%
    # deleting labels from subsequent merged tables (i >= 2)
    filter(!startsWith(.data$column, "label_"))

  table_header <-
    tibble(column = names(table_body)) %>%
    left_join(table_header, by = "column") %>%
    table_header_fill_missing()

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
  fmt_missing_ref =
    imap(
      tbls,
      function(x, y) {
        # returning NULL for non-regression objects
        if (!class(x) %in% c("tbl_regression", "tbl_uvregression", "tbl_stack"))
          return(NULL)
        if (class(x) == "tbl_stack" &&
            !class(x$tbl_regression_list[[1]]) %in% c("tbl_regression", "tbl_uvregression"))
          return(NULL)
        # making gt missing code for references
        glue(
          "gt::fmt_missing(",
          "columns = gt::vars({paste(c('estimate', 'ci'), y, sep = '_', collapse = ', ')}), ",
          "rows = (variable %in% c('{paste(unique(x$table_body$variable), collapse = \"', '\")}') & ",
          "row_type == 'level' & row_ref_{y} == TRUE), missing_text = '---')"
        )
      }
    ) %>%
    compact() %>%
    unlist() %>%
    glue_collapse_null(),

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
      "gt::tab_spanner(label = gt::md('{tab_spanner}'), columns = gt::ends_with('_{seq_len(tbls_length)}'))"
    ) %>%
    glue_collapse(sep = " %>% ")
))

# kable function calls ------------------------------------------------------------
# quoting returns an expression to be evaluated later
kable_tbl_merge <- quote(list(
  # first call to the gt function
  kable = glue("x$table_body"),

  #  placeholder, so the formatting calls are performed other calls below
  fmt = NULL,

  # Show "---" for reference groups
  fmt_missing_ref =
    imap(
      tbls,
      function(x, y) {
        # returning NULL for non-regression objects
        if (!class(x) %in% c("tbl_regression", "tbl_uvregression", "tbl_stack"))
          return(NULL)
        if (class(x) == "tbl_stack" &&
            !class(x$tbl_regression_list[[1]]) %in% c("tbl_regression", "tbl_uvregression"))
          return(NULL)
        # making mutate missing code for references
        glue(
          "dplyr::mutate_at(dplyr::vars(estimate_{y}, ci_{y}), ",
          "~ dplyr::case_when(row_ref_{y} == TRUE ~ '---', TRUE ~ .))"
        )
      }
    ) %>%
    compact() %>%
    unlist() %>%
    glue_collapse_null()
))



# this function grabs the inputs from each of the input models
# for univariate tbls, taking the inputs from the first model
tbl_inputs <- function(tbl) {
  map(
    tbl,
    function(tbl) {
      if (class(tbl) %in% c("tbl_regression", "tbl_stack", "tbl_summary")) {
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
