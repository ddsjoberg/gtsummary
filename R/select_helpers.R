#' Select helper functions
#'
#' Set of functions to supplement the {tidyselect} set of functions for selecting
#' columns of data frames. `all_continuous()`, `all_categorical()`, and
#' `all_dichotomous()` may only be used with `tbl_summary()`, where each variable
#' has been classified into one of these three groups. All other helpers
#' are available throughout the package.
#' @name select_helpers
#' @rdname select_helpers
#' @param dichotomous Logical indicating whether to include dichotomous variables.
#' Default is `TRUE`
#' @export
#' @return A character vector of column names selected
#' @examples
#' select_ex1 <-
#'   trial %>%
#'   dplyr::select(age, response, grade) %>%
#'   tbl_summary(
#'     statistic = all_continuous() ~ "{mean} ({sd})",
#'     type = all_dichotomous() ~ "categorical"
#'   )

# THE ENVIRONMENTS ARE CREATED IN `utils-gtsummary_core.R`
all_continuous <- function() {
  meta_data_env$summary_type %>%
    keep(meta_data_env$summary_type == "continuous") %>%
    names()
}

#' @rdname select_helpers
#' @export
all_categorical <- function(dichotomous = TRUE) {
  # return variable names if dochotomous included
  if (dichotomous) {
    x <-
      keep(meta_data_env$summary_type, ~ . %in% c("categorical", "dichotomous")) %>%
      names()
    return(x)
  }

  # return variable names if dochotomous NOT included
  meta_data_env$summary_type %>%
    keep(meta_data_env$summary_type == "categorical") %>%
    names()
}

#' @rdname select_helpers
#' @export
all_dichotomous <- function() {
  meta_data_env$summary_type %>%
    keep(meta_data_env$summary_type == "dichotomous") %>%
    names()
}

#' @rdname select_helpers
#' @export
all_numeric <- function() {
  which(data_env$numeric)
}

#' @rdname select_helpers
#' @export
all_character <- function() {
  which(data_env$character)
}

#' @rdname select_helpers
#' @export
all_integer <- function() {
  which(data_env$integer)
}

#' @rdname select_helpers
#' @export
all_double <- function() {
  which(data_env$double)
}

#' @rdname select_helpers
#' @export
all_logical <- function() {
  which(data_env$logical)
}

#' @rdname select_helpers
#' @export
all_factor <- function() {
  which(data_env$factor)
}


