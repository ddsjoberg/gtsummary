#' @import dplyr
#' @import purrr
#' @import gt
#' @importFrom tidyr nest unnest complete spread
#' @importFrom tibble tibble as_tibble
#' @importFrom rlang .data %||% set_names sym expr enexpr quo enquo parse_expr
#' @importFrom glue glue
#' @keywords internal
"_PACKAGE"

# allowing for the use of the dot when piping
utils::globalVariables(".")
