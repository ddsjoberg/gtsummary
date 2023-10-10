#' Stratified gtsummary tables
#'
#' \lifecycle{maturing}
#' Build a stratified gtsummary table. Any gtsummary table that accepts
#' a data frame as its first argument can be stratified.
#' - In `tbl_strata()`, the stratified or subset data frame is passed to the
#'   function in `.tbl_fun=`, e.g. `purrr::map(data, .tbl_fun)`.
#' - In `tbl_strata2()`, both the stratified data frame and the strata level
#'   are passed to `.tbl_fun=`, e.g. `purrr::map2(data, strata, .tbl_fun)`
#'
#' @param data a data frame or survey object
#' @param .tbl_fun A function or formula. If a _function_, it is used as is.
#' If a formula, e.g. `~ .x %>% tbl_summary() %>% add_p()`, it is converted to a function.
#' The stratified data frame is passed to this function.
#' @param ... Additional arguments passed on to the `.tbl_fun` function.
#' @param strata character vector or tidy-selector of columns in data to stratify results by
#' @param .sep when more than one stratifying variable is passed, this string is
#' used to separate the levels in the spanning header. Default is `", "`
#' @param .combine_with One of `c("tbl_merge", "tbl_stack")`. Names the function
#' used to combine the stratified tables.
#' @param .combine_args named list of arguments that are passed to function
#' specified in `.combine_with=`
#' @param .header String indicating the headers that will be placed.
#' Default is `"**{strata}**"` when `.combine_with = "tbl_merge"` and
#' `"{strata}"` when `.combine_with = "tbl_stack"`. Items placed in
#' curly brackets will be evaluated according to `glue::glue()` syntax.
#'   - `strata` stratum levels
#'   - `n` N within stratum
#'   - `N` Overall N
#'
#' The evaluated value of `.header=` is also available within `tbl_strata2(.tbl_fun=)`
#' @param .stack_group_header DEPRECATED.
#' @param .quiet Logical indicating whether to print messages in console.
#' Default is `FALSE`
#'
#' @section Tips:
#'
#' * `tbl_summary()`
#'
#'     * The number of digits continuous variables are rounded to is determined
#'     separately within each stratum of the data frame. Set the `digits=`
#'     argument to ensure continuous variables are rounded to the same number
#'     of decimal places.
#'
#'     * If some levels of a categorical variable are unobserved within a
#'     stratum, convert the variable to a factor to ensure all levels appear in
#'     each stratum's summary table.
#'
#' @author Daniel D. Sjoberg
#' @name tbl_strata
#' @family tbl_regression tools
#' @family tbl_uvregression tools
#' @family tbl_summary tools
#' @family tbl_survfit tools
#' @family tbl_svysummary tools
#'
#' @examples
#' \donttest{
#' # Example 1 ----------------------------------
#' tbl_strata_ex1 <-
#'   trial %>%
#'   select(age, grade, stage, trt) %>%
#'   mutate(grade = paste("Grade", grade)) %>%
#'   tbl_strata(
#'     strata = grade,
#'     .tbl_fun =
#'       ~ .x %>%
#'         tbl_summary(by = trt, missing = "no") %>%
#'         add_n(),
#'     .header = "**{strata}**, N = {n}"
#'   )
#'
#' # Example 2 ----------------------------------
#' tbl_strata_ex2 <-
#'   trial %>%
#'   select(grade, response) %>%
#'   mutate(grade = paste("Grade", grade)) %>%
#'   tbl_strata2(
#'     strata = grade,
#'     .tbl_fun =
#'       ~ .x %>%
#'         tbl_summary(
#'           label = list(response = .y),
#'           missing = "no",
#'           statistic = response ~ "{p}%"
#'         ) %>%
#'         add_ci(pattern = "{stat} ({ci})") %>%
#'         modify_header(stat_0 = "**Rate (95% CI)**") %>%
#'         modify_footnote(stat_0 = NA),
#'     .combine_with = "tbl_stack",
#'     .combine_args = list(group_header = NULL),
#'     .quiet = TRUE
#'   ) %>%
#'   modify_caption("**Response Rate by Grade**")
#' }
#' @section Example Output:
#' \if{html}{Example 1}
#'
#' \if{html}{\out{
#' `r man_create_image_tag(file = "tbl_strata_ex1.png", width = "84")`
#' }}
#'
#' @section Example Output:
#' \if{html}{Example 2}
#'
#' \if{html}{\out{
#' `r man_create_image_tag(file = "tbl_strata_ex2.png", width = "30")`
#' }}
NULL

#' @export
#' @rdname tbl_strata
tbl_strata <- function(data,
                       strata,
                       .tbl_fun,
                       ...,
                       .sep = ", ",
                       .combine_with = c("tbl_merge", "tbl_stack"),
                       .combine_args = NULL,
                       .header =
                         ifelse(.combine_with == "tbl_merge", "**{strata}**", "{strata}"),
                       .stack_group_header = NULL,
                       .quiet = NULL) {
  .combine_with <- match.arg(.combine_with)
  # run `tbl_strata()``
  tbl_strata_internal(
    data = data,
    strata = {{ strata }},
    .tbl_fun = .tbl_fun,
    ...,
    .sep = .sep,
    .combine_with = .combine_with,
    .combine_args = .combine_args,
    .header = .header,
    .stack_group_header = .stack_group_header,
    .quiet = .quiet,
    .parent_fun = "tbl_strata"
  )
}

#' @export
#' @rdname tbl_strata
tbl_strata2 <- function(data,
                        strata,
                        .tbl_fun,
                        ...,
                        .sep = ", ",
                        .combine_with = c("tbl_merge", "tbl_stack"),
                        .combine_args = NULL,
                        .header =
                          ifelse(.combine_with == "tbl_merge", "**{strata}**", "{strata}"),
                        .stack_group_header = NULL,
                        .quiet = NULL) {
  .combine_with <- match.arg(.combine_with)
  # run `tbl_strata()``
  tbl_strata_internal(
    data = data,
    strata = {{ strata }},
    .tbl_fun = .tbl_fun,
    ...,
    .sep = .sep,
    .combine_with = .combine_with,
    .combine_args = .combine_args,
    .header = .header,
    .stack_group_header = .stack_group_header,
    .quiet = .quiet,
    .parent_fun = "tbl_strata2"
  )
}

tbl_strata_internal <- function(data,
                                strata,
                                .tbl_fun,
                                ...,
                                .sep = ", ",
                                .combine_with = c("tbl_merge", "tbl_stack"),
                                .combine_args = NULL,
                                .header = NULL,
                                .stack_group_header = NULL,
                                .quiet = NULL,
                                .parent_fun) {
  # setting defaults -----------------------------------------------------------
  .quiet <- .quiet %||% get_theme_element("pkgwide-lgl:quiet") %||% FALSE

  # checking inputs ------------------------------------------------------------
  if (!is.data.frame(data) && !is_survey(data)) {
    abort("`data=` must be a data frame or survey object.")
  }
  if (!rlang::is_string(.header)) {
    stop("Argument `.header=` must be a string.", call. = FALSE)
  }

  # selecting stratum ----------------------------------------------------------
  strata <-
    select(
      switch(is_survey(data),
        data$variables
      ) %||% data, # select from data frame
      {{ strata }}
    ) %>%
    names()
  new_strata_names <-
    as.list(strata) %>%
    set_names(paste0("strata_", seq_len(length(strata))))

  # calculating df_by ----------------------------------------------------------
  data_for_strata <- data
  if (!is_survey(data_for_strata)) {
    df_by <-
      data_for_strata %>%
      mutate(strata = paste(!!!syms(strata), sep = .sep)) %>%
      df_by(by = "strata")
  } else {
    data_for_strata$variables <-
      data_for_strata$variables %>%
      mutate(strata = paste(!!!syms(strata), sep = .sep))
    df_by <-
      data_for_strata %>%
      df_by(by = "strata")
  }
  df_by <-
    df_by %>%
    select(
      strata = "by",
      any_of(c(
        "n", "N", "p",
        "n_unweighted", "N_unweighted", "p_unweighted"
      ))
    ) %>%
    mutate(header = glue::glue(.header))

  # nesting data and building tbl objects --------------------------------------
  df_tbls <-
    nest_df_and_svy(data, strata) %>%
    arrange(!!!syms(strata)) %>%
    rename(!!!syms(new_strata_names)) %>%
    rowwise() %>%
    mutate(strata = paste(!!!syms(names(new_strata_names)), sep = .sep)) %>%
    ungroup() %>%
    left_join(
      df_by %>% select("strata", "header"),
      by = "strata"
    ) %>%
    mutate(
      tbl =
        switch(.parent_fun,
          "tbl_strata" = map(.data$data, .tbl_fun, ...),
          "tbl_strata2" = map2(.data$data, .data$header, .tbl_fun, ...)
        )
    )

  # deprecated argument --------------------------------------------------------
  if (!is.null(.stack_group_header) && isTRUE(.combine_with == "tbl_stack")) {
    lifecycle::deprecate_stop(
      when = "1.5.1",
      what = "gtsummary::tbl_strata(.stack_group_header)",
      details =
        switch(isFALSE(.stack_group_header),
          glue(
            "Use the following instead:\n",
            "gtsummary::tbl_strata(.combine_args = list(group_header = NULL))"
          )
        )
    )
  }

  # combining tbls -------------------------------------------------------------
  .combine_args <-
    # default arguments
    switch(.combine_with,
      "tbl_merge" = list(tab_spanner = df_tbls$header),
      "tbl_stack" = list(group_header = df_tbls$header, quiet = .quiet)
    ) %>%
    # update with user-passed arguments
    purrr::list_modify(!!!.combine_args)

  if (.combine_with == "tbl_merge") {
    tbl <- rlang::inject(tbl_merge(tbls = df_tbls$tbl, !!!.combine_args))
  } else if (.combine_with == "tbl_stack") {
    tbl <- rlang::inject(tbl_stack(tbls = df_tbls$tbl, !!!.combine_args))
  }

  # return tbl -----------------------------------------------------------------
  tbl$df_strata <- df_tbls %>% select(starts_with("strata_"), "header")
  class(tbl) <- c("tbl_strata", .combine_with, "gtsummary")
  tbl
}

nest_df_and_svy <- function(data, strata) {
  # if data frame, return nested tibble
  if (is.data.frame(data)) {
    return(nest(data, data = -all_of(.env$strata)))
  }

  if (length(strata) > 1) {
    abort("survey objects allow for a single stratifying variable.")
  }

  # if survey object, construct a nested tibble
  tibble(strata_var = pluck(data, "variables", strata) %>% unique()) %>%
    rowwise() %>%
    mutate(
      data = data[data$variables[[.env$strata]] %in% .data$strata_var, ] %>% list()
    ) %>%
    ungroup() %>%
    set_names(c(strata, "data"))
}
