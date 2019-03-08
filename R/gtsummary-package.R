#' @import dplyr
#' @import purrr
#' @import gt
#' @importFrom tidyr nest nest_ unnest unnest_ complete complete_ spread_ spread
#' @importFrom tibble tibble as_tibble
#' @importFrom rlang %||% set_names sym expr enexpr quo enquo eval_tidy := parse_expr
#' @importFrom glue glue glue_data
#' @importFrom stats sd var quantile
#' @keywords internal
"_PACKAGE"

# allowing for the use of the dot when piping
utils::globalVariables(".")
