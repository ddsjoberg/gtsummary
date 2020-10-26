#' Select helper functions
#'
#' Set of functions to supplement the {tidyselect} set of functions for selecting
#' columns of data frames. `all_continuous()`, `all_continuous2()`, `all_categorical()`, and
#' `all_dichotomous()` may only be used with `tbl_summary()`, where each variable
#' has been classified into one of these three groups. All other helpers
#' are available throughout the package.
#' @name select_helpers
#' @rdname select_helpers
#' @param dichotomous Logical indicating whether to include dichotomous variables.
#' Default is `TRUE`
#' @param continuous2 Logical indicating whether to include continuous2 variables.
#' Default is `TRUE`
#' @export
#' @return A character vector of column names selected
#' @examples
#' select_ex1 <-
#'   trial %>%
#'   select(age, response, grade) %>%
#'   tbl_summary(
#'     statistic = all_continuous() ~ "{mean} ({sd})",
#'     type = all_dichotomous() ~ "categorical"
#'   )

all_continuous <- function(continuous2 = TRUE) {
  if (continuous2) con_types <- c("continuous", "continuous2")
  else con_types <- "continuous"

  .generic_selector("variable", "var_type",
                    .data$var_type %in% con_types,
                    fun_name = "all_continuous")
}

#' @rdname select_helpers
#' @export
all_continuous2 <- function() {
  .generic_selector("variable", "var_type",
                    .data$var_type %in% "continuous2",
                    fun_name = "all_continuous")
}

# broom.helpers ----------------------------------------------------------------
#' @rdname select_helpers
#' @export
#' @importFrom broom.helpers all_dichotomous
broom.helpers::all_dichotomous

#' @rdname select_helpers
#' @export
#' @importFrom broom.helpers all_categorical
broom.helpers::all_categorical
