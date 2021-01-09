#' Add references rows for categorical variables
#'
#' For categorical variables with a treatment contrast
#' ([stats::contr.treatment()]), a SAS contrast ([stats::contr.SAS()])
#' or a sum contrast ([stats::contr.sum()]), add a reference row.
#'
#' The added `reference_row` column will be equal to:
#'
#' * `TRUE` for a reference row;
#' * `FALSE` for a normal row of a variable with a reference row;
#' * `NA` for variables without a reference row.
#'
#' If the `contrasts` column is not yet available in `x`,
#' [tidy_add_contrasts()] will be automatically applied.
#'
#' `tidy_add_reference_rows()` will not populate the label
#' of the reference term. It is therefore better to apply
#' [tidy_add_term_labels()] after `tidy_add_reference_rows()`
#' rather than before.
#' @param x a tidy tibble
#' @param no_reference_row a vector indicating the name of variables
#' for those no reference row should be added.
#' Accepts [tidyselect][dplyr::select] syntax. Default is `NULL`.
#' See also [all_categorical()] and [all_dichotomous()]
#' @param model the corresponding model, if not attached to `x`
#' @inheritParams tidy_plus_plus
#' @export
#' @family tidy_helpers
#' @examples
#' df <- Titanic %>%
#'   dplyr::as_tibble() %>%
#'   dplyr::mutate(Survived = factor(Survived, c("No", "Yes")))
#'
#' res <- df %>%
#'   glm(
#'     Survived ~ Class + Age + Sex,
#'     data = ., weights = .$n, family = binomial,
#'     contrasts = list(Age = contr.sum, Class = "contr.SAS")
#'   ) %>%
#'   tidy_and_attach()
#' res %>% tidy_add_reference_rows()
#' res %>% tidy_add_reference_rows(no_reference_row = all_dichotomous())
#' res %>% tidy_add_reference_rows(no_reference_row = "Class")
#'
#' if (requireNamespace("gtsummary")) {
#'   glm(
#'     response ~ stage + grade * trt,
#'     gtsummary::trial,
#'     family = binomial,
#'     contrasts = list(
#'       stage = contr.treatment(4, base = 3),
#'       grade = contr.treatment(3, base = 2),
#'       trt = contr.treatment(2, base = 2)
#'     )
#'   ) %>%
#'     tidy_and_attach() %>%
#'     tidy_add_reference_rows()
#' }
tidy_add_reference_rows <- function(
  x, no_reference_row = NULL,
  model = tidy_get_model(x),
  quiet = FALSE
) {
  if (is.null(model)) {
    stop("'model' is not provided. You need to pass it or to use 'tidy_and_attach()'.")
  }

  # adding reference rows is not meaningful for stats::aov
  if (inherits(model, "aov")) {
    return(x %>% dplyr::mutate(reference_row = NA))
  }

  if ("header_row" %in% names(x)) {
    stop("`tidy_add_reference_rows()` cannot be applied after `tidy_add_header_rows().`")
  }

  if ("reference_row" %in% names(x)) {
    if (!quiet)
      usethis::ui_oops("tidy_add_reference_rows() has already been applied. x has been returned unchanged.")
    return(x)
  }

  .attributes <- .save_attributes(x)

  if ("label" %in% names(x)) {
    if (!quiet)
      usethis::ui_info(paste0(
        "tidy_add_reference_rows() has been applied after tidy_add_term_labels().\n",
        "You should consider applying tidy_add_reference_rows() first."
      ))
  }

  if (!"contrasts" %in% names(x)) {
    x <- x %>% tidy_add_contrasts(model = model)
  }

  # obtain character vector of selected variables
  no_reference_row <- .select_to_varnames({{ no_reference_row }}, var_info = x, arg_name = "no_reference_row")

  terms_levels <- model_list_terms_levels(model)

  if (!is.null(terms_levels))
    terms_levels <- terms_levels %>%
      # keep only terms corresponding to variable in x
      # (e.g. to exclude interaction only variables)
      dplyr::filter(
        .data$variable %in% unique(stats::na.omit(x$variable)) &
          # and exclude variables in no_reference_row
          !.data$variable %in% no_reference_row
      )

  if (is.null(terms_levels) || nrow(terms_levels) == 0)
    return(
      x %>%
        dplyr::mutate(reference_row = NA) %>%
        tidy_attach_model(model)
      )

  terms_levels <- terms_levels %>%
    dplyr::group_by(.data$variable) %>%
    dplyr::mutate(rank = 1:dplyr::n())

  has_var_label <- "var_label" %in% names(x)
  if (!has_var_label) {
    x$var_label <- NA_character_
  } # temporary populate it

  x <- x %>%
    dplyr::mutate(
      reference_row = dplyr::if_else(
        .data$variable %in% unique(terms_levels$variable),
        FALSE,
        NA
      ),
      rank = 1:dplyr::n() # for sorting table at the end
    )

  if ("y.level" %in% names(x) & inherits(model, "multinom")) { # specific case for nnet::multinom
    ref_rows <- terms_levels %>%
      dplyr::filter(.data$reference) %>%
      dplyr::mutate(reference_row = TRUE) %>%
      dplyr::select(.data$term, .data$variable, .data$label, .data$reference_row, .data$rank)

    if (!"label" %in% names(x))
      ref_rows <- ref_rows %>% dplyr::select(-.data$label)

    var_summary <- x %>%
      dplyr::group_by(.data$y.level, .data$variable) %>%
      dplyr::summarise(
        var_class = dplyr::first(.data$var_class),
        var_type = dplyr::first(.data$var_type),
        var_label = dplyr::first(.data$var_label),
        var_nlevels = dplyr::first(.data$var_nlevels),
        contrasts = dplyr::first(.data$contrasts),
        contrasts_type = dplyr::first(.data$contrasts_type),
        var_min_rank = min(.data$rank),
        var_max_rank = min(.data$rank),
        .groups = "drop_last"
      )

    ref_rows <- ref_rows %>%
      dplyr::left_join(
        var_summary,
        by = "variable"
      ) %>%
      dplyr::mutate(
        rank = .data$var_min_rank -1.25 + .data$rank,
        # if last, reduce by .5 to avoid overlap with next variable
        rank = dplyr::if_else(
          .data$rank > .data$var_max_rank,
          .data$rank - .5,
          .data$rank
        )
      ) %>%
      dplyr::select(-.data$var_min_rank, -.data$var_max_rank)

    x <- x %>%
      dplyr::bind_rows(ref_rows)
  } else {
    # normal case
    ref_rows <- terms_levels %>%
      dplyr::filter(.data$reference) %>%
      dplyr::mutate(reference_row = TRUE) %>%
      dplyr::select(.data$term, .data$variable, .data$label, .data$reference_row, .data$rank)

    if (!"label" %in% names(x))
      ref_rows <- ref_rows %>% dplyr::select(-.data$label)

    var_summary <- x %>%
      dplyr::group_by(.data$variable) %>%
      dplyr::summarise(
        var_class = dplyr::first(.data$var_class),
        var_type = dplyr::first(.data$var_type),
        var_label = dplyr::first(.data$var_label),
        var_nlevels = dplyr::first(.data$var_nlevels),
        contrasts = dplyr::first(.data$contrasts),
        contrasts_type = dplyr::first(.data$contrasts_type),
        var_min_rank = min(.data$rank),
        var_max_rank = min(.data$rank),
        .groups = "drop_last"
      )

    ref_rows <- ref_rows %>%
      dplyr::left_join(
        var_summary,
        by = "variable"
      ) %>%
      dplyr::mutate(
        rank = .data$var_min_rank -1.25 + .data$rank,
        # if last, reduce by .5 to avoid overlap with next variable
        rank = dplyr::if_else(
          .data$rank > .data$var_max_rank,
          .data$rank - .5,
          .data$rank
        )
      ) %>%
      dplyr::select(-.data$var_min_rank, -.data$var_max_rank)

    x <- x %>%
      dplyr::bind_rows(ref_rows)
  }

  if (!has_var_label) {
    x <- x %>% dplyr::select(-.data$var_label)
  }

  x %>%
    dplyr::arrange(.data$rank) %>%
    dplyr::select(-.data$rank) %>%
    tidy_attach_model(model = model, .attributes = .attributes)
}
