#' Assigns summary type (e.g. continuous, categorical, or dichotomous).
#'
#' For variables where the summary type was not specified in the function
#' call of `tbl_summary`, `assign_summary_type` assigns a type based on class and
#' number of unique levels.
#'
#' @param data Data frame.
#' @param variable Vector of column name.
#' @param class Vector of classes (e.g. numeric, character, etc.)
#' corresponding one-to-one with the names in `variable`.
#' @param summary_type list that includes specified summary types,
#' e.g. \code{summary_type = list(age = "continuous")}
#' @return Vector summary types `c("continuous", "categorical", "dichotomous")`.
#' @keywords internal
#' @author Daniel Sjoberg
#' @examples
#' # assign_summary_type(data = mtcars,
#' #                     variable =  names(mtcars),
#' #                     class = apply(mtcars, 2, class),
#' #                     summary_type = NULL)
assign_summary_type <- function(data, variable, class, summary_type) {
  map2_chr(
    variable, class,
    ~ summary_type[[.x]] %||%
      case_when(
        # logical variables will be dichotmous
        .y == "logical" ~
        "dichotomous",

        # numeric variables that are 0 and 1 only, will be dichotomous
        .y %in% c("integer", "numeric") & length(setdiff(na.omit(data[[.x]]), c(0, 1))) == 0
        ~ "dichotomous",

        # factor variables that are "No" and "Yes" only, will be dichotomous
        .y %in% c("factor") & length(setdiff(na.omit(data[[.x]]), c("No", "Yes"))) == 0
        ~ "dichotomous",
        .y %in% c("factor") & length(setdiff(na.omit(data[[.x]]), c("no", "yes"))) == 0
        ~ "dichotomous",
        .y %in% c("factor") & length(setdiff(na.omit(data[[.x]]), c("NO", "YES"))) == 0
        ~ "dichotomous",

        # factors and characters are categorical
        .y %in% c("factor", "character") ~
        "categorical",

        # numeric variables with fewer than 10 levels will be categorical
        .y %in% c("integer", "numeric") & length(unique(na.omit(data[[.x]]))) < 10
        ~ "categorical",

        # everything else is assigned to continuous
        TRUE ~ "continuous"
      )
  )
}


# n = 50
# dta = dplyr::data_frame(
#   age = rnorm(n) + 35,
#   female = sample(c(T, F), size = n, replace = T),
#   male = as.numeric(female),
#   sex = ifelse(female == T, "Female", "Male"),
#   country = sample(c("USA", "Canada"), size = n, replace = T),
#   shoe_size = sample(1:25, size = n, replace = T),
#   family_size = sample(1:5, size = n, replace = T)
# )
# #adding missing values
# dta = dplyr::mutate_all(dta, dplyr::funs( ifelse(runif(n) < 0.25, NA, .)) ) %>%
#   dplyr::mutate(
#     sex = as.factor(sex),
#     male_fct = ifelse(female == TRUE, "No", "Yes") %>% factor()
#   )
# dta
#
# # creating base meta data dataframe
# meta_data =
#   dplyr::data_frame(
#     variable = names(dta),
#     class = purrr::map_chr(variable, ~ class(dta[[.x]]))
#   )
# meta_data
#
# # tesing function's guessing ability
# meta_data %>%
#   dplyr::mutate(
#     assign_summary_type = assign_summary_type(dta, variable, class, NULL)
#   )
#
#
# # tesing function's ability when type assigned
# meta_data %>%
#   dplyr::mutate(
#     assign_summary_type = assign_summary_type(dta, variable, class, list(shoe_size = "categorical"))
#   )
