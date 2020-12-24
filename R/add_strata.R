#' Add stratified columns
#'
#' Stratifies existing `tbl_summary()` object by a third variable
#'
#' @param x `tbl_summary` object
#' @param strata variable that tables will be stratified by
#' @param include_unstratafied logical indicated whether to include the
#' original table in the result. Default is `FALSE`
#'
#' @export
#' @examples
#' add_strata_ex1 <-
#'   trial %>%
#'   select(age, grade, trt, stage) %>%
#'   mutate(grade = paste("Grade", grade)) %>%
#'   tbl_summary(by = trt, include = -grade, missing = "no") %>%
#'   add_strata(grade)
#'
#' @section Example Output:
#' \if{html}{Example 1}
#'
#' \if{html}{\figure{add_strata_ex1.png}{options: width=65\%}}

add_strata <- function(x, strata, include_unstratafied = FALSE) {
  addtl_fns <- rlang::enquo(addtl_fns)
  strata <- broom.helpers::.select_to_varnames(select = {{ strata }},
                                               data = x$inputs$data,
                                               var_info = x$table_body,
                                               arg_name = "strata",
                                               select_single = TRUE)
  if (rlang::is_empty(strata)) abort("`strata=` cannot be missing.")

  # set args that will be passed to `tbl_summary()` ----------------------------
  args <- x$inputs
  args$data <- NULL
  args$include <- args$include %>% setdiff(strata)

  # build tbl_summary within each strata ---------------------------------------
  df_strata <-
    x$inputs$data %>%
    arrange(!!sym(strata)) %>%
    group_by(!!sym(strata)) %>%
    tidyr::nest() %>%
    mutate(
      tbl = map(data, ~rlang::inject(tbl_summary(data = .x, !!!args)))
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




