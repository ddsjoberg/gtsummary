#' Add stratified columns
#'
#' Stratifies existing `tbl_summary()` object by a third variable
#'
#' @param x `tbl_summary` object
#' @param strata variable that tables will be stratified by
#' @param include_unstratafied logical indicated whether to include the
#' original table in the result. Default is `FALSE`
#' @param additional_fn an additional function to run on the resulting individual
#' tbls before they are merged. Accepts formula shortcut notation, e.g.
#' `~add_p(.x) %>% modify_header(all_stat_cols() ~ "**{level}**")`
#'
#' @export
#' @examples
#' add_strata_ex1 <-
#'   trial %>%
#'   select(age, grade, trt, stage) %>%
#'   dplyr::mutate(grade = paste("Grade", grade)) %>%
#'   tbl_summary(by = trt, include = -grade, missing = "no") %>%
#'   add_strata(
#'     strata = grade,
#'     additional_fn = ~modify_header(.x, all_stat_cols() ~ "**{level}**")
#'   )
#'
#' @section Example Output:
#' \if{html}{Example 1}
#'
#' \if{html}{\figure{add_strata_ex1.png}{options: width=65\%}}

add_strata <- function(x, strata, include_unstratafied = FALSE, additional_fn = identity) {
  additional_fn <- gts_mapper(additional_fn)
  strata <- broom.helpers::.select_to_varnames(select = {{ strata }},
                                               data = x$inputs$data,
                                               var_info = x$table_body,
                                               arg_name = "strata",
                                               select_single = TRUE)
  if (rlang::is_empty(strata)) abort("`strata=` cannot be missing.")

  # set args that will be passed to `tbl_summary()` ----------------------------
  args <- x$inputs
  args$data <- NULL
  args$label <- .extract_variable_label(x)
  args$include <- args$include %>% setdiff(strata)
  args$digits <- .extract_fmt_funs(x)
  args$type <- .extract_summary_type(x)

  # build tbl_summary within each strata ---------------------------------------
  df_strata <-
    x$inputs$data %>%
    mutate_at(
      all_of(tibble::enframe(args$type) %>% filter(.data$value %in% "categorical") %>% pull(.data$name)),
      factor
    ) %>%
    arrange(!!sym(strata)) %>%
    group_by(!!sym(strata)) %>%
    tidyr::nest() %>%
    mutate(
      tbl = map(data, ~rlang::inject(tbl_summary(data = .x, !!!args))) %>%
        map(additional_fn)
    )

  tab_spanner <- df_strata[[strata]] %>% as.character() %>% {paste0("**", ., "**")}
  tbl_list <- df_strata$tbl

  # include original table if requested ----------------------------------------
  if (include_unstratafied == TRUE) {
    tab_spanner <- c("**Unstratified**", tab_spanner)
    tbl_list <- c(list(x), tbl_list)
  }

  # return merged tbls ---------------------------------------------------------
  tbl_merge(tbl_list, tab_spanner = tab_spanner)
}

.extract_fmt_funs <- function(x) {
  x$meta_data %>%
    select(variable, stat_display, df_stats) %>%
    mutate(
      stat_names = map2(stat_display, variable, extracting_function_calls_from_stat_display),
      stat_funs = map2(
        stat_names, df_stats,
        function(stat_names, df_stats) {
          map(stat_names, ~attr(df_stats[[.x]], "fmt_fun")) %>%
            rlang::set_names(stat_names)
        }
      )
    ) %>%
    select(variable, stat_funs) %>%
    tibble::deframe()
}

.extract_summary_type <- function(x) {
  x$meta_data %>%
    select(variable, summary_type) %>%
    tibble::deframe()
}

.extract_variable_label <- function(x) {
  x$meta_data %>%
    select(variable, var_label) %>%
    tibble::deframe()
}
