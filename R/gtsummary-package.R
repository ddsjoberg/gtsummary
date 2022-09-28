#' @importFrom dplyr mutate select n group_by ungroup filter pull case_when
#' if_else full_join left_join distinct bind_rows count coalesce arrange rename
#' rename_at bind_cols mutate_all mutate_at slice desc rowwise inner_join
#' row_number
#' @importFrom purrr map imap map2 pmap map_chr map_dfr map_lgl map_dbl map_if
#' imap_dfr imap_lgl map2_chr pmap_lgl pmap_chr pmap_dbl compact keep discard
#' every some pluck flatten negate partial cross_df reduce chuck
#' @importFrom tidyr nest unnest complete spread
#' @importFrom tibble tibble tribble as_tibble enframe deframe
#' @importFrom rlang .data .env %||% set_names sym syms parse_expr expr exprs
#' call2 := inform abort is_function is_string enexpr inject is_empty
#' is_function is_list is_named is_character check_dots_empty
#' quo_is_null enquo eval_tidy quo_text
#' @importFrom glue glue as_glue glue_collapse
#' @importFrom stringr fixed word str_extract_all str_remove_all str_starts
#' str_split str_detect str_remove str_replace_all str_wrap str_sub str_locate
#' str_sub
#' @importFrom broom.helpers .formula_list_to_named_list .select_to_varnames
#' .generic_selector
#' @importFrom cli cli_alert_info cli_alert_danger cli_code cli_ul
#' @importFrom gt md html
#' @keywords internal
"_PACKAGE"

# allowing for the use of the dot when piping
utils::globalVariables(".")

## usethis namespace: start
#' @importFrom lifecycle deprecate_soft
## usethis namespace: end
NULL

release_bullets <- function() {
  c(
    "Check the output from `devtools::check()` and look for warnings or messages",
    "Review deprecation schedule",
    "Run the code styler",
    "Updated the gt help file images",
    "Check build size is less than 5MB"
  )
}
