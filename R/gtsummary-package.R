#' @keywords internal
#' @import dplyr
#' @import purrr
#' @importFrom tidyr nest nest_ unnest unnest_ complete complete_ spread_ spread
#' @importFrom tibble tibble as_tibble
#' @importFrom rlang %||% set_names sym expr enexpr quo enquo eval_tidy :=
#' @importFrom glue glue glue_data
#' @importFrom stats sd var quantile
"_PACKAGE"

# allowing for the use of the dot when piping
utils::globalVariables(".")

