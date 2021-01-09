#' Add variable labels
#'
#' Will add variable labels in a `var_label` column, based on:
#' 1. labels provided in `labels` argument if provided;
#' 2. variable labels defined in the original data frame with
#'    the `label` attribute (cf. [labelled::var_label()]);
#' 3. variable name otherwise.
#'
#' @details
#' If the `variable` column is not yet available in `x`,
#' [tidy_identify_variables()] will be automatically applied.
#'
#' It is possible to pass a custom label for an interaction
#' term in `labels` (see examples).
#' @param x a tidy tibble
#' @param labels an optional named list or named vector of
#' custom variable labels
#' @param interaction_sep separator for interaction terms
#' @param model the corresponding model, if not attached to `x`
#' @inheritParams tidy_plus_plus
#' @export
#' @family tidy_helpers
#' @examples
#' df <- Titanic %>%
#'   dplyr::as_tibble() %>%
#'   dplyr::mutate(Survived = factor(Survived, c("No", "Yes"))) %>%
#'   labelled::set_variable_labels(
#'     Class = "Passenger's class",
#'     Sex = "Sex"
#'   )
#'
#' df %>%
#'   glm(Survived ~ Class * Age * Sex, data = ., weights = .$n, family = binomial) %>%
#'   tidy_and_attach() %>%
#'   tidy_add_variable_labels(
#'     labels = list(
#'       "(Intercept)" = "Custom intercept",
#'       Sex = "Gender",
#'       "Class:Age" = "Custom label"
#'     )
#'   )
tidy_add_variable_labels <- function(x,
                                     labels = NULL,
                                     interaction_sep = " * ",
                                     model = tidy_get_model(x),
                                     quiet = FALSE,
                                     strict = FALSE) {
  if (is.null(model)) {
    stop("'model' is not provided. You need to pass it or to use 'tidy_and_attach()'.")
  }

  if ("header_row" %in% names(x)) {
    stop("`tidy_add_variable_labels()` cannot be applied after `tidy_add_header_rows().`")
  }

  .attributes <- .save_attributes(x)

  if ("var_label" %in% names(x)) {
    x <- x %>% dplyr::select(-.data$var_label)
  }

  if (!"variable" %in% names(x)) {
    x <- x %>% tidy_identify_variables(model = model)
  }

  labels <- .formula_list_to_named_list(labels, var_info = x, arg_name = "labels")
  if (is.list(labels)) {
    labels <- unlist(labels)
  }

  # start with the list of terms
  var_labels <- unique(x$term)
  names(var_labels) <- var_labels

  # add the list of variables
  variable_list <- model_list_variables(model, labels = labels)
  additional_labels <- variable_list$var_label
  names(additional_labels) <- variable_list$variable
  var_labels <- var_labels %>%
    .update_vector(additional_labels)

  # check if all elements of labels are in x
  # show a message otherwise
  not_found <- setdiff(names(labels), names(var_labels))
  if (length(not_found) > 0 && !quiet) {
    usethis::ui_oops(paste0(
      usethis::ui_code(not_found),
      " terms have not been found in ",
      usethis::ui_code("x"),
      "."
    ))
  }
  if (length(not_found) > 0 && strict) {
    stop("Incorrect call with `labels=`. Quitting execution.", call. = FALSE)
  }

  var_labels <- var_labels %>%
    .update_vector(labels)

  # save custom labels
  .attributes$variable_labels <- labels

  # management of interaction terms
  interaction_terms <- x$variable[!is.na(x$var_type) & x$var_type == "interaction"]
  # do not treat those specified in labels
  interaction_terms <- setdiff(interaction_terms, names(labels))
  names(interaction_terms) <- interaction_terms
  # compute labels for interaction terms
  interaction_terms <- interaction_terms %>%
    strsplit(":") %>%
    lapply(function(x) {
      paste(var_labels[x], collapse = interaction_sep)
    }) %>%
    unlist()
  var_labels <- var_labels %>% .update_vector(interaction_terms)

  x %>%
    dplyr::left_join(
      tibble::tibble(
        variable = names(var_labels),
        var_label = var_labels
      ),
      by = "variable"
    ) %>%
    tidy_attach_model(model = model, .attributes = .attributes)
}
