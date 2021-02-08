#' Stratified gtsummary table
#'
#' Build a stratified gtsummary table. Any gtsummary table that accepts
#' a data frame as its first argument is accepted
#'
#' @param data a date frame
#' @param strata character vector or tidyselector of columns in data to
#' stratify tables by
#' @param .tbl_fun A function or formula. If a _function_, it is used as is.
#' If a formula, e.g. `~ .x %>% tbl_summary() %>% add_p()`, it is converted to a function.
#' The stratified data frame is passed to this function.
#' @param ... Additional arguments passed on to the `.tbl_fun` function.
#' @param .sep when more than one stratifying variable is passed, this string is
#' used to separate the levels. Default is `", "`
#' @param combine_with One of `c("tbl_merge", "tbl_stack")`. Names the function
#' used to combine the stratified tables.
#'
#' @author Daniel D. Sjoberg
#' @export
#'
#' @examples
#' tbl_strata_ex1 <-
#'   trial %>%
#'   select(age, grade, stage, trt) %>%
#'   mutate(grade = paste("Grade", grade)) %>%
#'   tbl_strata(
#'     strata = grade,
#'     ~ .x %>%
#'       tbl_summary(by = trt) %>%
#'       add_p()
#'   )

tbl_strata <- function(data, strata, .f, ..., .sep = ", ", combine_with = c("tbl_merge", "tbl_stack")) {
  # checking inputs ------------------------------------------------------------
  if (!is.data.frame(data)) abort("`data=` must be a data frame.")
  combine_with <- match.arg(combine_with)

  # selecting stratum ----------------------------------------------------------
  strata <- select(data, {{ strata }}) %>% names()
  new_strata_names <- as.list(strata) %>% set_names(paste0("strata_", seq_len(length(strata))))

  # nesting data and building tbl objects --------------------------------------
  df_tbls <-
    data %>%
    nest(data = -all_of(strata)) %>%
    rename(!!!syms(new_strata_names)) %>%
    mutate(
      tbl = map(.data$data, .f, ...)
    ) %>%
    rowwise() %>%
    mutate(
      header =
        paste(!!!syms(names(new_strata_names)), sep = .sep) %>%
        {ifelse(.env$combine_with == "tbl_merge", paste0("**", ., "**"), .)}
    )

  # combining tbls -------------------------------------------------------------
  if (combine_with == "tbl_merge")
    tbl <- tbl_merge(tbls = df_tbls$tbl, tab_spanner = df_tbls$header)
  else if (combine_with == "tbl_stack")
    tbl <- tbl_stack(tbls = df_tbls$tbl, group_header = df_tbls$header)

  # return tbl -----------------------------------------------------------------
  tbl$df_strata <- df_tbls %>% select(starts_with("strata_"), .data$header)
  class(tbl) <- c("tbl_strata", combine_with, "gtsummary")
  tbl
}
