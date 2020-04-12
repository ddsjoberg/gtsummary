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

# setting environments
data_env <- rlang::new_environment()
meta_data_env <- rlang::new_environment()

# registering data information
scoped_data <- function(.data) {
  data_env$numeric <- map_lgl(.data, is.numeric)
  data_env$character <- map_lgl(.data, is.character)
  data_env$integer <- map_lgl(.data, is.integer)
  data_env$double <- map_lgl(.data, is.double)
  data_env$logical <- map_lgl(.data, is.logical)
  data_env$factor <- map_lgl(.data, is.factor)
}

# registering meta data information
scoped_meta_data <- function(.meta_data) {
  meta_data_env$summary_type <-
    .meta_data$summary_type %>%
    set_names(.meta_data$variable)
}
