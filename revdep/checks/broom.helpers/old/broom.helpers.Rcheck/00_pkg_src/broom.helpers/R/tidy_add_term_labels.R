#' Add term labels
#'
#' Will add term labels in a `label` column, based on:
#' 1. labels provided in `labels` argument if provided;
#' 2. factor levels for categorical variables coded with
#'    treatment, SAS or sum contrasts (the label could be
#'    customized with `categorical_terms_pattern` argument);
#' 3. variable labels when there is only one term per variable;
#' 4. term name otherwise.
#'
#' @details
#' If the `variable_label` column is not yet available in `x`,
#' [tidy_add_variable_labels()] will be automatically applied.
#' If the `contrasts` column is not yet available in `x`,
#' [tidy_add_contrasts()] will be automatically applied.
#'
#' It is possible to pass a custom label for any term in `labels`,
#' including interaction terms.
#' @param x a tidy tibble
#' @param labels an optional named list or named vector of
#' custom term labels
#' @param interaction_sep separator for interaction terms
#' @param categorical_terms_pattern a [glue pattern][glue::glue()] for
#' labels of categorical terms with treatment or sum contrasts
#' (see examples and [model_list_terms_levels()])
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
#' mod <- df %>%
#'   glm(Survived ~ Class * Age * Sex, data = ., weights = .$n, family = binomial)
#' mod %>%
#'   tidy_and_attach() %>%
#'   tidy_add_term_labels()
#' mod %>%
#'   tidy_and_attach() %>%
#'   tidy_add_term_labels(
#'     interaction_sep = " x ",
#'     categorical_terms_pattern = "{level} / {reference_level}"
#'   )
tidy_add_term_labels <- function(x,
                                 labels = NULL,
                                 interaction_sep = " * ",
                                 categorical_terms_pattern = "{level}",
                                 model = tidy_get_model(x),
                                 quiet = FALSE,
                                 strict = FALSE) {
  if (is.null(model)) {
    stop("'model' is not provided. You need to pass it or to use 'tidy_and_attach()'.")
  }

  if ("header_row" %in% names(x)) {
    stop("`tidy_add_term_labels()` cannot be applied after `tidy_add_header_rows().`")
  }

  .attributes <- .save_attributes(x)

  if ("label" %in% names(x)) {
    x <- x %>% dplyr::select(-.data$label)
  }

  if (is.list(labels)) {
    labels <- unlist(labels)
  }

  if (!"var_label" %in% names(x)) {
    x <- x %>% tidy_add_variable_labels(model = model, quiet = quiet)
  }
  if (!"contrasts" %in% names(x)) {
    x <- x %>% tidy_add_contrasts(model = model)
  }

  # specific case for nnet::multinom
  # keeping only one level for computing term_labels
  if ("y.level" %in% names(x) & inherits(model, "multinom")) {
    xx <- x %>%
      dplyr::filter(.data$y.level == x$y.level[1])
  } else {
    xx <- x
  }

  # start with term names
  term_labels <- unique(stats::na.omit(xx$term))
  names(term_labels) <- term_labels

  # add categorical terms levels
  terms_levels <- model_list_terms_levels(
    model,
    label_pattern = categorical_terms_pattern,
    variable_labels = .attributes$variable_labels
  )
  additional_term_labels <- terms_levels$label
  names(additional_term_labels) <- terms_levels$term
  term_labels <- term_labels %>%
    .update_vector(additional_term_labels)

  # add variable labels
  # first variable list (for interaction only terms)
  # then current variable labels in x
  variables_list <- model_list_variables(model) %>%
    dplyr::mutate(
      label = dplyr::if_else(
        is.na(.data$label_attr),
        .data$variable,
        as.character(.data$label_attr)
      )
    )
  additional_term_labels <- variables_list$label
  names(additional_term_labels) <- variables_list$variable
  term_labels <- term_labels %>%
    .update_vector(additional_term_labels)

  x_var_labels <- xx %>%
    dplyr::mutate(
      variable = dplyr::if_else(
        is.na(.data$variable), # for intercept
        .data$term,
        .data$variable
      )
    ) %>%
    dplyr::group_by(.data$variable) %>%
    dplyr::summarise(
      var_label = dplyr::first(.data$var_label),
      .groups = "drop_last"
    )
  additional_term_labels <- x_var_labels$var_label
  names(additional_term_labels) <- x_var_labels$variable
  term_labels <- term_labels %>%
    .update_vector(additional_term_labels)


  # check if all elements of labels are in x
  # show a message otherwise
  not_found <- setdiff(names(labels), names(term_labels))
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

  # labels for polynomial terms
  poly_terms <- xx %>%
    dplyr::filter(.data$term %>% stringr::str_starts("poly\\(")) %>%
    dplyr::mutate(
      degree = .data$term %>% stringr::str_replace("poly\\(.+\\)([0-9]+)", "\\1"),
      label = paste0(.data$var_label, .superscript_numbers(.data$degree))
    )
  poly_labels <- poly_terms$label
  names(poly_labels) <- poly_terms$term
  term_labels <- term_labels %>%
    .update_vector(poly_labels)

  # labels argument
  term_labels <- term_labels %>%
    .update_vector(labels)
  # save custom labels
  .attributes$term_labels <- labels

  # management of interaction terms
  interaction_terms <- xx$term[!is.na(xx$var_type) & xx$var_type == "interaction"]
  # do not treat those specified in labels
  interaction_terms <- setdiff(interaction_terms, names(labels))
  names(interaction_terms) <- interaction_terms
  interaction_terms <-
    interaction_terms %>%
    strsplit(":") %>%
    lapply(function(x) {
      paste(term_labels[x], collapse = interaction_sep)
    }) %>%
    unlist()
  term_labels <- term_labels %>%
    .update_vector(interaction_terms)

  x %>%
    dplyr::left_join(
      tibble::tibble(
        term = names(term_labels),
        label = term_labels
      ),
      by = "term"
    ) %>%
    tidy_attach_model(model = model, .attributes = .attributes)
}
