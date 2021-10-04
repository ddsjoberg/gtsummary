#' Stratified gtsummary tables
#'
#' \lifecycle{maturing}
#' Build a stratified gtsummary table. Any gtsummary table that accepts
#' a data frame as its first argument can be stratified.
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
#' @param .stack_group_header When `TRUE` and `.combine_with = 'tbl_stack'`,
#' the stratum are passed in `tbl_stack(group_header=)`. Default is `TRUE`
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
#' @export
#' @family tbl_regression tools
#' @family tbl_uvregression tools
#' @family tbl_summary tools
#' @family tbl_survfit tools
#' @family tbl_svysummary tools
#'
#' @examples
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
#'         add_n()
#'   )
#' @section Example Output:
#' \if{html}{Example 1}
#'
#' \if{html}{\figure{tbl_strata_ex1.png}{options: width=64\%}}

tbl_strata <- function(data, strata,
                       .tbl_fun,
                       ...,
                       .sep = ", ",
                       .combine_with = c("tbl_merge", "tbl_stack"),
                       .stack_group_header = TRUE,
                       .quiet = NULL) {
  # setting defaults -----------------------------------------------------------
  .quiet <- .quiet %||% get_theme_element("pkgwide-lgl:quiet") %||% FALSE

  # checking inputs ------------------------------------------------------------
  if (!is.data.frame(data) && !is_survey(data)) {
    abort("`data=` must be a data frame or survey object.")
  }
  .combine_with <- match.arg(.combine_with)

  # selecting stratum ----------------------------------------------------------
  strata <-
    select(
      switch(is_survey(data),
             data$variables
      ) %||% data, # select from data frame
      {{ strata }}
    ) %>%
    names()
  new_strata_names <- as.list(strata) %>% set_names(paste0("strata_", seq_len(length(strata))))

  # nesting data and building tbl objects --------------------------------------
  df_tbls <-
    nest_df_and_svy(data, strata) %>%
    arrange(!!!syms(strata)) %>%
    rename(!!!syms(new_strata_names)) %>%
    rowwise() %>%
    mutate(
      strata = paste(!!!syms(names(new_strata_names)), sep = .sep),
      header =
        ifelse(.env$.combine_with == "tbl_merge",
               paste0("**", .data$strata, "**"),
               .data$strata)
    ) %>%
    ungroup() %>%
    mutate(
      tbl = map(.data$data, .tbl_fun, ...)
    )

  # combining tbls -------------------------------------------------------------
  if (.combine_with == "tbl_merge") {
    tbl <- tbl_merge(tbls = df_tbls$tbl, tab_spanner = df_tbls$header)
  }
  else if (.combine_with == "tbl_stack") {
    tbl <-
      tbl_stack(tbls = df_tbls$tbl,
                group_header = switch(isTRUE(.stack_group_header), df_tbls$header),
                quiet = .quiet)
  }

  # return tbl -----------------------------------------------------------------
  tbl$df_strata <- df_tbls %>% select(starts_with("strata_"), .data$header)
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
