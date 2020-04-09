#' @importFrom dplyr mutate select n group_by ungroup filter pull case_when
#' if_else full_join left_join distinct bind_rows count coalesce arrange rename
#' rename_at bind_cols mutate_all mutate_at slice desc
#' @importFrom purrr map imap map2 pmap map_chr map_dfr map_lgl map_dbl map_if
#' imap_dfr imap_lgl map2_chr pmap_lgl pmap_chr pmap_dbl compact keep discard
#' every some pluck flatten negate partial cross_df reduce
#' @importFrom tidyr nest unnest complete spread
#' @importFrom tibble tibble as_tibble
#' @importFrom rlang .data .env %||% set_names sym syms parse_expr expr exprs call2 :=
#' @importFrom glue glue as_glue glue_collapse
#' @importFrom stringr fixed word str_extract_all str_remove_all str_starts
#' str_split str_detect str_remove str_replace_all
#' @keywords internal
"_PACKAGE"

# allowing for the use of the dot when piping
utils::globalVariables(".")

## usethis namespace: start
#' @importFrom lifecycle deprecate_soft
## usethis namespace: end
NULL

release_questions <- function() {
  c(
    "Have you updated all the gt help file images?",
    "Is the size of the build less than 5MB?"
  )
}
