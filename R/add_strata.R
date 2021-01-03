#' Add stratified columns
#'
#' Stratifies existing `tbl_summary()`/`tbl_cross()` object by a categorical variable
#'
#' @param x `tbl_summary` object
#' @param strata variable that tables will be stratified by
#' @param include_unstratafied logical indicated whether to include the
#' original table in the result. Default is `FALSE`
#' @param method method used to combine stratified tables. Must be one of
#' `"merge"` or `"stack"`
#' @param additional_fn an additional function to run on the resulting individual
#' tbls before they are combined. Accepts formula shortcut notation, e.g.
#' `~add_p(.x) %>% modify_header(all_stat_cols() ~ "**{level}**")`
#' @inheritParams combine_terms
#'
#' @export
#' @examples
#' add_strata_ex1 <-
#'   trial %>%
#'   select(age, grade, trt, stage) %>%
#'   mutate(grade = paste("Grade", grade)) %>%
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

add_strata <- function(x, strata, method = c("merge", "stack"),
                       additional_fn = NULL, include_unstratafied = FALSE,
                       quiet = NULL) {
  # setting defaults -----------------------------------------------------------
  quiet <- quiet %||% get_theme_element("pkgwide-lgl:quiet") %||% FALSE
  additional_fn <- additional_fn %||% identity %>% gts_mapper(additional_fn)

  # checking inputs ------------------------------------------------------------
  if (!inherits(x, c("tbl_summary", "tbl_cross")))
    abort("`x=` must be class 'tbl_summary' or 'tbl_cross'")
  if (identical(quiet, FALSE)) .additional_fn_msg(x, additional_fn)

  strata <- broom.helpers::.select_to_varnames(select = {{ strata }},
                                               data = x$inputs$data,
                                               var_info = x$table_body,
                                               arg_name = "strata",
                                               select_single = TRUE)
  if (rlang::is_empty(strata)) abort("`strata=` cannot be missing.")


  tbl_fun <-
    match.arg(method) %>%
    switch("merge" = tbl_merge,
           "stack" = tbl_stack)

  # set args that will be passed to `tbl_summary()` ----------------------------
  args <- x$inputs
  args$data <- NULL
  args$label <- .extract_variable_label(x, strata)

  if (class(x)[1] == "tbl_summary") {
    args$include <- args$include %>% setdiff(strata)
    args$digits <- .extract_fmt_funs(x, strata)
    args$type <- .extract_summary_type(x, strata)

    x_fun <- tbl_summary
    vars_to_factor <-
      tibble::enframe(args$type) %>%
      filter(.data$value %in% "categorical") %>%
      pull(.data$name) %>%
      intersect(args$include)
  }
  else if (class(x)[1] == "tbl_cross") {
    x_fun <- tbl_cross
    vars_to_factor <- c(x$inputs$row, x$inputs$col)
  }

  # build tbl_summary within each strata ---------------------------------------
  df_strata <-
    x$inputs$data %>%
    mutate_at(
      all_of(vars_to_factor),
      ~switch(inherits(., "factor"), .) %||% factor(.)
    ) %>%
    arrange(!!sym(strata)) %>%
    group_by(!!sym(strata)) %>%
    tidyr::nest() %>%
    mutate(
      tbl = map(.data$data, ~rlang::inject(x_fun(data = .x, !!!args))) %>%
        map(additional_fn)
    )

  tbl_header <- df_strata[[strata]] %>% as.character()
  tbl_list <- df_strata$tbl

  # include original table if requested ----------------------------------------
  if (include_unstratafied == TRUE) {
    tbl_header <- c("Unstratified", tbl_header)
    tbl_list <- c(list(x), tbl_list)
  }
  if (identical(tbl_fun, tbl_merge)) tbl_header <- paste0("**", tbl_header, "**")

  # creating tbl_fn args list --------------------------------------------------
  if (identical(tbl_fun, tbl_merge)) tbl_fun_args <- list(tbls = tbl_list, tab_spanner = tbl_header)
  if (identical(tbl_fun, tbl_stack)) tbl_fun_args <- list(tbls = tbl_list, group_header = tbl_header)

  # return merged tbls ---------------------------------------------------------
  result <- inject(tbl_fun(!!!tbl_fun_args))
  result[["call_list"]] <- c(x[["call_list"]], add_strata = match.call())
  result
}

.additional_fn_msg <- function(x, additional_fn) {
  if (!identical(additional_fn, identity)) return(invisible())

  call_names <- x$call_list[-1] %>% names() %>% paste0("()")

  if (is.null(call_names)) return(invisible())

  code_msg <-
    c(".x", call_names) %>%
    reduce(~ paste(.x, "%>%", .y)) %>%
    {paste0("additional_fn = ~", .)}

  glue("Include the additional modifications to the stratified tables with ",
       "{ui_code(code_msg)}") %>%
    str_wrap() %>%
    ui_info()
}

.extract_fmt_funs <- function(x, strata) {
  x$meta_data %>%
    select(.data$variable, .data$stat_display, .data$df_stats) %>%
    mutate(
      stat_names = map2(.data$stat_display, .data$variable,
                        extracting_function_calls_from_stat_display),
      stat_funs = map2(
        .data$stat_names, .data$df_stats,
        function(stat_names, df_stats) {
          map(stat_names, ~attr(df_stats[[.x]], "fmt_fun")) %>%
            rlang::set_names(stat_names)
        }
      )
    ) %>%
    select(.data$variable, .data$stat_funs) %>%
    filter(!.data$variable %in% .env$strata) %>%
    tibble::deframe() %>%
    as.list()
}

.extract_summary_type <- function(x, strata) {
  x$meta_data %>%
    select(.data$variable, .data$summary_type) %>%
    filter(!.data$variable %in% .env$strata) %>%
    tibble::deframe() %>%
    as.list()
}

.extract_variable_label <- function(x, strata) {
  x$meta_data %>%
    select(.data$variable, .data$var_label) %>%
    filter(!.data$variable %in% .env$strata) %>%
    tibble::deframe() %>%
    as.list()
}
