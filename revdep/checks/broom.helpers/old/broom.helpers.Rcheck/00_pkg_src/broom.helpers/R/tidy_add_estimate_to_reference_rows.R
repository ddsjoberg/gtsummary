#' Add an estimate value to references rows for categorical variables
#'
#' For categorical variables with a treatment contrast
#' ([stats::contr.treatment()]) or a SAS contrast ([stats::contr.SAS()])
#' will add an estimate equal to `0` (or `1` if `exponentiate = TRUE`)
#' to the reference row.
#'
#' For categorical variables with a sum contrast ([stats::contr.sum()]),
#' the estimate value of the reference row will be equal to the sum of
#' all other coefficients multiplied by `-1` (eventually exponentiated if
#' `exponentiate = TRUE`), and obtained with [stats::dummy.coef()].
#' For sum contrasts, the model coefficient corresponds
#' to the difference of each level with the grand mean.
#'
#' For other variables, no change will be made.
#'
#' @details
#' If the `reference_row` column is not yet available in `x`,
#' [tidy_add_reference_rows()] will be automatically applied.
#'
#' @param x a tidy tibble
#' @param exponentiate logical indicating whether or not to exponentiate the
#' coefficient estimates. It should be consistent with the original call to
#' [broom::tidy()]
#' @param model the corresponding model, if not attached to `x`
#' @inheritParams tidy_plus_plus
#' @export
#' @family tidy_helpers
#' @examples
#' df <- Titanic %>%
#'   dplyr::as_tibble() %>%
#'   dplyr::mutate(dplyr::across(where(is.character), factor))
#'
#' df %>%
#'   glm(
#'     Survived ~ Class + Age + Sex,
#'     data = ., weights = .$n, family = binomial,
#'     contrasts = list(Age = contr.sum, Class = "contr.SAS")
#'   ) %>%
#'   tidy_and_attach(exponentiate = TRUE) %>%
#'   tidy_add_reference_rows() %>%
#'   tidy_add_estimate_to_reference_rows()
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
#'     tidy_add_reference_rows() %>%
#'     tidy_add_estimate_to_reference_rows()
#' }
tidy_add_estimate_to_reference_rows <- function(
  x, exponentiate = attr(x, "exponentiate"),
  model = tidy_get_model(x),
  quiet = FALSE
) {
  if (is.null(exponentiate) | !is.logical(exponentiate))
    stop("'exponentiate' is not provided. You need to pass it explicitely.")

  if (is.null(model)) {
    stop("'model' is not provided. You need to pass it or to use 'tidy_and_attach()'.")
  }

  .attributes <- .save_attributes(x)
  .attributes$exponentiate <- exponentiate

  if (!"reference_row" %in% names(x)) {
    x <- x %>% tidy_add_reference_rows(model = model)
  }

  if (!"estimate" %in% names(x)) { # to avoid a problem with certain types of model (e.g. gam)
    return(x %>% tidy_attach_model(model))
  }

  # treatment contrasts
  x <- x %>%
    dplyr::mutate(
      estimate = dplyr::if_else(
        !is.na(.data$reference_row) & .data$reference_row & stringr::str_starts(.data$contrasts, "contr.treatment|contr.SAS"),
        dplyr::if_else(exponentiate, 1, 0),
        .data$estimate
      )
    )

  # sum contrasts
  ref_rows_sum <- which(x$reference_row & x$contrasts == "contr.sum")
  if (length(ref_rows_sum) > 0) {
    for (i in ref_rows_sum) {
      x$estimate[i] <-
        .get_ref_row_estimate_contr_sum(x$variable[i], model = model,
                                        exponentiate = exponentiate, quiet = quiet)
    }
  }

  x %>%
    tidy_attach_model(model = model, .attributes = .attributes)
}


.get_ref_row_estimate_contr_sum <- function(variable, model, exponentiate = FALSE,
                                            quiet = FALSE) {

  if (inherits(model, "multinom")) {
    dc <- NULL
    if (!quiet)
      usethis::ui_info(paste0(
        "Sum contrasts are not supported for 'multinom' models.\n",
        "Reference row of variable '", variable, "' remained unchanged."
      ))
  } else {
    dc <- tryCatch(
      suppressMessages(
        emmeans::emmeans(model, specs = variable, contr = "eff")
      ),
      error = function(e) {
        if (!quiet)
          usethis::ui_info(paste0(
            "No emmeans() method for this type of model.\n",
            "Reference row of variable '", variable, "' remained unchanged."
          ))
        NULL
      }
    )
  }

  if (is.null(dc)) {
    dc <- NA_real_
  } else {
    dc <- dc$contrasts %>%
      as.data.frame() %>%
      purrr::pluck("estimate") %>%
      dplyr::last()
  }

  if (exponentiate) {
    dc <- exp(dc)
  }
  dc
}

# Origiginal code using stats::dummy.coef
# .get_ref_row_estimate_contr_sum <- function(variable, model, exponentiate = FALSE,
#                                             quiet) {
#   # bug fix for character variables
#   if ("model" %in% names(model)) {
#     model$model <- model$model %>%
#       dplyr::mutate(dplyr::across(where(is.character), factor))
#   }
#
#   dc <- tryCatch(
#     dplyr::last(stats::dummy.coef(model)[[variable]]),
#     error = function(e) {
#       if (!quiet)
#         usethis::ui_info(paste0(
#           "No dummy.coef() method for this type of model.\n",
#           "Reference row of variable '", variable, "' remained unchanged."
#         ))
#       NA
#     }
#   )
#
#   if (exponentiate) {
#     dc <- exp(dc)
#   }
#   dc
# }
