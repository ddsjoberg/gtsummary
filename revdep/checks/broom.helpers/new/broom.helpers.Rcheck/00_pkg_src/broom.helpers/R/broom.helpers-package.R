## usethis namespace: start
#' @importFrom lifecycle deprecate_soft
#' @importFrom usethis ui_code
#' @importFrom rlang .data .env
#' @importFrom purrr %||%
## usethis namespace: end
NULL

# because `where` is not exported by tidyselect
# cf. https://github.com/r-lib/tidyselect/issues/201
utils::globalVariables(c(".", "where"))

# update named vectors, y values overriding x values if common name
.update_vector <- function(x, y) {
  if (is.null(y)) {
    return(x)
  }
  if (is.null(names(y)) || any(names(y) == "")) {
    stop("All elements of y should be named.")
  }
  for (i in names(y)) {
    if (utils::hasName(x, i)) {
      x[i] <- y[i]
    } else {
      x <- c(x, y[i])
    }
  }
  x
}

# return superscript character
# .superscript_numbers(0:20)
.superscript_numbers <- function(x) {
  if (!is.character(x)) {
    x <- as.character(x)
  }
  x[x == "1"] <- "" # do not show when equal 1
  pattern <- c(
    "0" = "\u2070", "1" = "\u00b9", "2" = "\u00b2",
    "3" = "\u00b3", "4" = "\u2074", "5" = "\u2075",
    "6" = "\u2076", "7" = "\u2077", "8" = "\u2078",
    "9" = "\u2079"
  )
  x %>% stringr::str_replace_all(pattern)
}


# for consistent column order
.order_tidy_columns <- function(x) {
  x %>%
    dplyr::select(
      dplyr::any_of(
        c(
          "y.level", "term", "variable", "var_label", "var_class", "var_type",
          "var_nlevels", "header_row", "contrasts", "contrasts_type", "reference_row", "label"
        )
      ),
      dplyr::everything()
    )
}

# attributes to be saved between tidy_* functions
.save_attributes <- function(x) {
  .attributes <- attributes(x)
  .attributes_names <- intersect(
    names(.attributes),
    c(
      "exponentiate", "coefficients_type", "coefficients_label",
      "variable_labels", "term_labels"
      )
  )
  .attributes[.attributes_names]
}

