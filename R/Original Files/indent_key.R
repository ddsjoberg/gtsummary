#' Make indices compatible with \code{\link[kableExtra]{add_indent}}
#'
#' Creates vector indicating rows to be indented from `fmt_table1` or `fmt_regression`
#' output that can be passed to `kableExtra::add_indent`.
#'
#' @param x `fmt_table1`, `fmt_regression`, or `fmt_uni_regression` object
#' @param ... further arguments passed to or from other methods.
#' @return vector for use in kableExtra::add_indent() or otherwise
#' @export
indent_key <- function(x, ...) UseMethod("indent_key")

#' Makes index of factors requiring indent from `fmt_table1` objects in Rmarkdown
#'
#' @param x `fmt_table1` object
#' @param ...	further arguments passed to or from other methods
#' @export
#' @examples
#' \dontrun{
#' t1 <- trial %>%
#'   fmt_table1() %>%
#'   bold_labels()
#' 
#' knitr::kable(as_tibble(t1),
#'   row.names = FALSE,
#'   caption = "Table 1: Summary of Patient and Clinical Variables"
#' ) %>%
#'   kableExtra::kable_styling(
#'     bootstrap_options = c("condensed"),
#'     font_size = 11,
#'     full_width = F
#'   ) %>%
#'   kableExtra::add_indent(indent_key(t1))
#' }
#' 
indent_key.fmt_table1 <- function(x, ...) {
  key <- which(x$table1$row_type %in% c("level", "missing")) - 1
  return(key)
}


#' Makes index of factor variables requiring indent from
#' `fmt_regression` objects in Rmarkdown
#'
#' @param x `fmt_regression` object
#' @param ...	further arguments passed to or from other methods
#' @export
#' @examples
#' \dontrun{
#' library(lme4)
#' mod_glmer <- glmer(am ~ hp + +wt + factor(cyl) + (1 | gear), mtcars, family = binomial)
#' tglmer <- fmt_regression(mod_glmer, exponentiate = TRUE)
#' 
#' # In RMarkdown, knitting to HTML:
#' knitr::kable(as_tibble(tglmer),
#'   row.names = FALSE,
#'   caption = "Model Summary"
#' ) %>%
#'   kableExtra::kable_styling(
#'     bootstrap_options = c("striped", "condensed"),
#'     font_size = 11,
#'     full_width = F
#'   ) %>%
#'   kableExtra::add_indent(indent_key(tglmer)) %>%
#'   kableExtra::footnote(general = "Model coefficients and p-values calculated using
#' generalized linear mixed model, with random intercept specified
#' as gear type.")
#' }
#' 
indent_key.fmt_regression <- function(x, ...) {
  key <- which(x$model_tbl$row_type %in% c("level", "missing")) - 1
  return(key)
}

#' Makes index of factor variables requiring indent from
#' `fmt_regression` objects in Rmarkdown
#'
#' @param x `fmt_regression` object
#' @param ...	further arguments passed to or from other methods
#' @export
#' @examples
#' \dontrun{
#' library(lme4)
#' uni_mod <- fmt_uni_regression(trial,
#'   method = "glm",
#'   y = "response",
#'   method.args = list(family = binomial),
#'   exponentiate = TRUE
#' )
#' 
#' # In RMarkdown, knitting to HTML:
#' kable(as_tibble(uni_mod),
#'   row.names = FALSE,
#'   caption = "Model Summary"
#' ) %>%
#'   kableExtra::add_indent(indent_key(uni_mod))
#' }
#' 
indent_key.fmt_uni_regression <- function(x, ...) {
  key <- which(x$model_tbl$row_type %in% c("level", "missing")) - 1
  return(key)
}
