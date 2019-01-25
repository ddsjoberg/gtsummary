#' Assigns variable label to display.
#'
#' Preference is given to labels specified in `fmt_table1(..., var_label = list())`
#' argument, then to a label attribute attached to the data frame
#' (i.e. attr(data$var, "label)), then to the variable name.
#'
#' @param data Data frame.
#' @param variable Vector of column name.
#' @param var_label list that includes specified variable labels,
#' e.g. `var_label = list(age = "Age, yrs")`
#' @return Vector variable labels.
#' @keywords internal
#' @examples
#' # assign_var_label(mtcars, names(mtcars), list(hp = "Horsepower"))
assign_var_label <- function(data, variable, var_label) {
  purrr::map_chr(
    variable,
    ~ var_label[[.x]] %||%
      attr(data[[.x]], "label") %||%
      .x
  )
}

# n = 10
# dta = dplyr::data_frame(
#   age = rnorm(n),
#   sex = 1
# )
# attr(dta[["age"]], "label") = "Patient Age, yrs"
#
# var_label_one(dta, "age", NULL)
#
# meta =
#   dplyr::data_frame(
#     variable = names(dta)
#   )
#
# dplyr::mutate(meta, var_label = var_label(dta, variable, NULL))
# dplyr::mutate(meta, var_label = var_label(dta, variable, list(sex = "Gender")))
