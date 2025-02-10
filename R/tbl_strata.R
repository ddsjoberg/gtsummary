#' Stratified gtsummary tables
#'
#' Build a stratified gtsummary table. Any gtsummary table that accepts
#' a data frame as its first argument can be stratified.
#' - In `tbl_strata()`, the stratified or subset data frame is passed to the
#'   function in `.tbl_fun=`, e.g. `purrr::map(data, .tbl_fun)`.
#' - In `tbl_strata2()`, both the stratified data frame and the strata level
#'   are passed to `.tbl_fun=`, e.g. `purrr::map2(data, strata, .tbl_fun)`
#'
#' @param data (`data.frame`, `survey.design`)\cr
#'   a data frame or survey object
#' @param .tbl_fun (`function`)
#'   A function or formula. If a _function_, it is used as is.
#'   If a formula, e.g. `~ .x %>% tbl_summary() %>% add_p()`, it is converted to a function.
#'   The stratified data frame is passed to this function.
#' @param ... Additional arguments passed on to the `.tbl_fun` function.
#' @param strata ([`tidy-select`][dplyr::dplyr_tidy_select])\cr
#'   character vector or tidy-selector of columns in data to stratify results by.
#'   Only _observed_ combinations are shown in results.
#' @param .sep (`string`)\cr
#'   when more than one stratifying variable is passed, this string is
#'   used to separate the levels in the spanning header. Default is `", "`
#' @param .combine_with (`string`)\cr
#'   One of `c("tbl_merge", "tbl_stack")`. Names the function
#'   used to combine the stratified tables.
#' @param .combine_args (named `list`)\cr
#'   named list of arguments that are passed to function specified in `.combine_with`
#' @param .header (`string`)\cr
#'   String indicating the headers that will be placed.
#'   Default is `"**{strata}**"` when `.combine_with = "tbl_merge"` and
#'   `"{strata}"` when `.combine_with = "tbl_stack"`. Items placed in
#'   curly brackets will be evaluated according to `glue::glue()` syntax.
#'     - `strata` stratum levels
#'     - `n` N within stratum
#'     - `N` Overall N
#'
#'   The evaluated value of `.header` is also available within `tbl_strata2(.tbl_fun)`
#' @param .stack_group_header `r lifecycle::badge("deprecated")`
#' @param .quiet `r lifecycle::badge("deprecated")`
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
#'
#' @examplesIf (identical(Sys.getenv("NOT_CRAN"), "true") || identical(Sys.getenv("IN_PKGDOWN"), "true")) && gtsummary:::is_pkg_installed(c("cardx", "broom"))
#' # Example 1 ----------------------------------
#' trial |>
#'   select(age, grade, stage, trt) |>
#'   mutate(grade = paste("Grade", grade)) |>
#'   tbl_strata(
#'     strata = grade,
#'     .tbl_fun =
#'       ~ .x |>
#'         tbl_summary(by = trt, missing = "no") |>
#'         add_n(),
#'     .header = "**{strata}**, N = {n}"
#'   )
#'
#' # Example 2 ----------------------------------
#' trial |>
#'   select(grade, response) |>
#'   mutate(grade = paste("Grade", grade)) |>
#'   tbl_strata2(
#'     strata = grade,
#'     .tbl_fun =
#'       ~ .x %>%
#'         tbl_summary(
#'           label = list(response = .y),
#'           missing = "no",
#'           statistic = response ~ "{p}%"
#'         ) |>
#'         add_ci(pattern = "{stat} ({ci})") |>
#'         modify_header(stat_0 = "**Rate (95% CI)**") |>
#'         remove_footnote_header(stat_0),
#'     .combine_with = "tbl_stack",
#'     .combine_args = list(group_header = NULL)
#'   ) |>
#'   modify_caption("**Response Rate by Grade**")
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
  set_cli_abort_call()

  # deprecations ---------------------------------------------------------------
  if (!missing(.quiet)) {
    lifecycle::deprecate_warn(
      when = "2.0.0",
      what = "gtsummary::tbl_strata(.quiet)",
      details = "Argument has been ignored."
    )
  }

  # process inputs -------------------------------------------------------------
  check_class(data, c("data.frame", "survey.design"))
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
                        .quiet = TRUE) {
  set_cli_abort_call()

  # deprecations ---------------------------------------------------------------
  if (!missing(.quiet)) {
    lifecycle::deprecate_warn(
      when = "2.0.0",
      what = "gtsummary::tbl_strata(.quiet)",
      details = "Argument has been ignored."
    )
  }

  # process inputs -------------------------------------------------------------
  check_class(data, c("data.frame", "survey.design"))
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
                                .parent_fun) {
  check_string(.header)


  # selecting stratum ----------------------------------------------------------
  cards::process_selectors(
    data = as.data.frame(data),
    strata = {{ strata }}
  )

  new_strata_names <-
    as.list(strata) %>%
    set_names(paste0("strata_", seq_len(length(strata))))

  # calculating df_by ----------------------------------------------------------
  data_for_strata <- data
  if (!is_survey(data_for_strata)) {
    df_by <-
      data_for_strata %>%
      dplyr::mutate(strata = paste(!!!syms(strata), sep = .sep)) %>%
      df_by(by = "strata")
  } else {
    data_for_strata$variables <-
      data_for_strata$variables %>%
      dplyr::mutate(strata = paste(!!!syms(strata), sep = .sep))
    df_by <-
      data_for_strata %>%
      df_by(by = "strata")
  }
  df_by <-
    df_by %>%
    dplyr::select(
      strata = "by",
      any_of(c(
        "n", "N", "p",
        "n_unweighted", "N_unweighted", "p_unweighted"
      ))
    ) %>%
    dplyr::mutate(header = glue::glue(.header))

  # nesting data and building tbl objects --------------------------------------
  df_tbls <-
    nest_df_and_svy(data, strata) %>%
    dplyr::arrange(!!!syms(strata)) %>%
    dplyr::rename(!!!syms(new_strata_names)) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(strata = paste(!!!syms(names(new_strata_names)), sep = .sep)) %>%
    dplyr::ungroup() %>%
    dplyr::left_join(
      df_by %>% select("strata", "header"),
      by = "strata"
    ) %>%
    dplyr::mutate(
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
           "tbl_stack" = list(group_header = df_tbls$header)
    ) %>%
    # update with user-passed arguments
    utils::modifyList(val = .combine_args %||% list())

  if (.combine_with == "tbl_merge") {
    tbl <- inject(tbl_merge(tbls = df_tbls$tbl, !!!.combine_args))
  } else if (.combine_with == "tbl_stack") {
    tbl <- inject(tbl_stack(tbls = df_tbls$tbl, !!!.combine_args))
  }

  # return tbl -----------------------------------------------------------------
  tbl$df_strata <- df_tbls %>% dplyr::select(starts_with("strata_"), "header")
  class(tbl) <- c("tbl_strata", .combine_with, "gtsummary")
  tbl
}

nest_df_and_svy <- function(data, strata) {
  # if data frame, return nested tibble
  if (is.data.frame(data)) {
    return(tidyr::nest(data, data = -all_of(.env$strata)))
  }

  if (length(strata) > 1) {
    cli::cli_abort(
      "Survey objects can only be stratified by a single variable.",
      call = get_cli_abort_call()
    )
  }

  # if survey object, construct a nested tibble
  dplyr::tibble(strata_var = getElement(data, "variables") |> getElement(strata) |> unique()) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      data = data[data$variables[[.env$strata]] %in% .data$strata_var, ] %>% list()
    ) %>%
    dplyr::ungroup() %>%
    set_names(c(strata, "data"))
}

#' Simple utility function to get extract and calculate additional information
#' about the 'by' variable in \code{\link{tbl_summary}}
#'
#' Given a dataset and the name of the 'by' variable, this function returns a
#' data frame with unique levels of the by variable, the by variable ID, a character
#' version of the levels, and the column name for each level in the \code{\link{tbl_summary}}
#' output data frame.
#'
#' @param data data frame
#' @param by character name of the `by` variable found in data
#' @noRd
#' @keywords internal
#' @author Daniel D. Sjoberg

df_by <- function(data, by) {
  if (is.null(by)) {
    return(NULL)
  }

  if (!is_survey(data)) {
    # classic data.frame
    result <-
      data %>%
      dplyr::select(by = all_of(by)) %>%
      dplyr::count(!!sym("by"), .drop = FALSE) %>%
      dplyr::arrange(!!sym("by")) %>%
      dplyr::mutate(
        N = sum(.data$n),
        p = .data$n / .data$N,
        by_id = 1:dplyr::n(), # 'by' variable ID
        by_chr = as.character(.data$by), # Character version of 'by' variable
        by_fct = # factor version of 'by' variable
          switch(inherits(.data$by, "factor"),
                 factor(.data$by, levels = attr(.data$by, "levels"), ordered = FALSE)
          ) %||%
          factor(.data$by),
        by_col = paste0("stat_", .data$by_id) # Column name of in fmt_table1 output
      ) %>%
      dplyr::select(starts_with("by"), everything())
  } else {
    # survey object
    svy_table <- survey::svytable(c_form(right = by), data, round = TRUE) %>%
      dplyr::as_tibble() %>%
      set_names("by", "n") %>%
      dplyr::mutate(
        N = sum(.data$n),
        p = .data$n / .data$N
      )

    result <- df_by(data$variables, by) %>%
      dplyr::rename(n_unweighted = "n", N_unweighted = "N", p_unweighted = "p") %>%
      dplyr::left_join(svy_table, by = "by")

    result
  }

  attr(result$by, "label") <- NULL
  result
}

c_form <- function(left = NULL, right = 1) {
  # quoting to take into account complex names
  if (!is.null(left)) left <- paste0("`", left, "`")
  right <- paste0("`", right, "`")
  left <- paste(left, collapse = "+")
  right <- paste(right, collapse = "+")
  stats::as.formula(paste(left, "~", right))
}
