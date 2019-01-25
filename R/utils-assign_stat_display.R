#' Assign type of summary statistic
#'
#' Function that assigns default statistics to display, or if specified,
#' assigns the user-defined statistics for display.
#'
#' @param summary_type A list that includes specified summary types
#' @param stat_display List with up to two named elements.  Names must be
#' continuous or categorical. Can be \code{NULL}.
#' @return vector of stat_display selections for each variable
#' @keywords internal

assign_stat_display <- function(summary_type, stat_display) {
  # dichotomous and categorical are treated in the same fashion here
  summary_type <- ifelse(summary_type == "dichotomous", "categorical", summary_type)

  # otherwise, return defaults
  return(
    purrr::map_chr(
      summary_type,
      ~ dplyr::case_when(
        .x == "continuous" ~
        purrr::`%||%`(stat_display[[.x]], "{median} ({q1}, {q3})"),
        .x %in% c("categorical", "dichotomous") ~
        purrr::`%||%`(stat_display[[.x]], "{n} ({p}%)")
      )
    )
  )
}

# assign_stat_display("continuous", NULL)
# assign_stat_display(c("continuous", "dichotomous"), NULL)
# assign_stat_display(c("continuous", "dichotomous"), stat_display = list(continuous = "{median}"))

# assign_stat_display("continuous", NULL)
# assign_stat_display("continuous", list(dichotomous = "{n}/{N} ({p}%)"))
# assign_stat_display("dichotomous", list(dichotomous = "{n}/{N} ({p}%)"))
