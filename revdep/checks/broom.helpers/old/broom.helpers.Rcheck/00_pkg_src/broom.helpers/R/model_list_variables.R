#' List all the variables used in a model
#'
#' Including variables used only in an interaction.
#'
#' @param model a model object
#' @param labels an optional named list or named vector of
#' custom variable labels
#' @param only_variable if `TRUE`, will return only "variable"
#' column
#' @return
#' A tibble with three columns:
#' * `variable`: the corresponding variable
#' * `var_class`: class of the variable (cf. [stats::.MFclass()])
#' * `label_attr`: variable label defined in the original data frame
#'    with the label attribute (cf. [labelled::var_label()])
#' * `var_label`: a variable label (by priority, `labels` if defined,
#'   `label_attr` if available, otherwise `variable`)
#'
#' @export
#' @family model_helpers
#' @examples
#' Titanic %>%
#'   dplyr::as_tibble() %>%
#'   dplyr::mutate(Survived = factor(Survived, c("No", "Yes"))) %>%
#'   glm(
#'     Survived ~ Class + Age : Sex,
#'     data = ., weights = .$n,
#'     family = binomial
#'   ) %>%
#'   model_list_variables()
#'
#' iris %>%
#'   lm(
#'     Sepal.Length ~ poly(Sepal.Width, 2) + Species,
#'     data = .,
#'     contrasts = list(Species = contr.sum)
#'   ) %>%
#'   model_list_variables()
#'
#' if (requireNamespace("gtsummary")) {
#'   glm(
#'     response ~ poly(age, 3) + stage + grade * trt,
#'     na.omit(gtsummary::trial),
#'     family = binomial,
#'   ) %>%
#'     model_list_variables()
#' }
model_list_variables <- function(model, labels = NULL, only_variable = FALSE) {
  UseMethod("model_list_variables")
}

#' @rdname model_list_variables
#' @export
model_list_variables.default <- function(model, labels = NULL, only_variable = FALSE) {
  model_terms <- stats::terms(model)

  variable_names <- attr(model_terms, "term.labels")
  model_frame <- model_get_model_frame(model)
  dataClasses <- purrr::map(model_frame, .MFclass2) %>% unlist()

  if (is.null(dataClasses)) {
    dataClasses <- attr(model_terms, "dataClasses")
  }

  # update the list with all elements of dataClasses
  variable_names <- names(dataClasses) %>%
    c(variable_names) %>%
    .clean_backticks() %>%
    unique()

  res <- tibble::tibble(
    variable = variable_names
  ) %>%
    .add_var_class(dataClasses) %>%
    .add_label_attr(model) %>%
    # specific case of polynomial terms defined with poly()
    dplyr::mutate(
      variable = stringr::str_replace(.data$variable, "^poly\\((.*),(.*)\\)$", "\\1")
    ) %>%
    .compute_var_label(labels)

  if (only_variable) return(res$variable)

  res
}


#' @rdname model_list_variables
#' @export
model_list_variables.lavaan <- function(model, labels = NULL, only_variable = FALSE) {
  res <- tibble::tibble(
    variable = .clean_backticks(unique(model@ParTable$lhs))
  ) %>%
    dplyr::left_join(
      tibble::tibble(
        variable = .clean_backticks(model@Data@ov$name),
        var_class = model@Data@ov$type
      ),
      by = "variable"
    ) %>%
    dplyr::mutate(
      var_class = dplyr::if_else(
        .data$var_class == "ordered",
        "factor",
        .data$var_class
      )
    ) %>%
    .add_label_attr(model) %>%
    .compute_var_label(labels)

  if (only_variable) return(res$variable)

  res
}

## model_list_variables() helpers --------------------------

.add_var_class <- function(x, dataClasses) {
  x %>%
    dplyr::left_join(
      tibble::tibble(
        variable = names(dataClasses),
        var_class = dataClasses
      ),
      by = "variable"
    )
}

.add_label_attr <- function(x, model) {
  labels <- unlist(labelled::var_label(model_get_model_frame(model)))
  if (length(labels) > 0)
    x %>%
      dplyr::left_join(
        dplyr::tibble(
          variable = names(labels),
          label_attr = labels
        ),
        by = "variable"
      )
  else
    x %>%
      dplyr::mutate(label_attr = NA)
}

# stats::.MFclass do not distinct integer and numeric
.MFclass2 <- function (x)
{
  if (is.logical(x))
    return("logical")
  if (is.ordered(x))
    return("ordered")
  if (is.factor(x))
    return("factor")
  if (is.character(x))
    return("character")
  if (is.matrix(x) && is.numeric(x))
    return(paste0("nmatrix.", ncol(x)))
  if (is.integer(x))
    return("integer")
  if (is.numeric(x))
    return("numeric")
  return("other")
}

.compute_var_label <- function(x, labels = NULL) {
  if (is.list(labels)) {
    labels <- unlist(labels)
  }
  if (is.null(labels)) {
    x$var_custom_label <- NA_character_
  } else {
    x <- x %>%
      dplyr::left_join(
        dplyr::tibble(
          variable = names(labels),
          var_custom_label = labels
        ),
        by = "variable"
      )
  }
  x %>% dplyr::mutate(
    label_attr = as.character(.data$label_attr),
    var_label = dplyr::case_when(
      !is.na(.data$var_custom_label) ~ .data$var_custom_label,
      !is.na(.data$label_attr) ~ .data$label_attr,
      TRUE ~ .data$variable
    )
  ) %>%
    dplyr::select(-.data$var_custom_label)
}
