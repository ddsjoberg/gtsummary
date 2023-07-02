#' Convert gtsummary object to a kable object
#'
#' @description Output from [knitr::kable] is less full featured compared to
#' summary tables produced with [gt](https://gt.rstudio.com/index.html).
#' For example, kable summary tables do not include indentation, footnotes,
#' or spanning header rows.
#'
#' Line breaks (`\n`) are removed from column headers and table cells.
#'
#' @details Tip: To better distinguish variable labels and level labels when
#' indenting is not supported, try [bold_labels()] or [italicize_levels()].
#'
#' @inheritParams as_gt
#' @param x Object created by a function from the gtsummary package
#' (e.g. [tbl_summary] or [tbl_regression])
#' @inheritParams as_gt
#' @inheritParams as_tibble.gtsummary
#' @param ... Additional arguments passed to [knitr::kable]
#' @export
#' @return A `knitr_kable` object
#' @family gtsummary output types
#' @author Daniel D. Sjoberg
#' @examples
#' \donttest{
#' trial %>%
#'   tbl_summary(by = trt) %>%
#'   bold_labels() %>%
#'   as_kable()
#' }

as_kable <- function(x, ..., include = everything(), return_calls = FALSE) {
  .assert_class(x, "gtsummary")

  # running pre-conversion function, if present --------------------------------
  x <- do.call(get_theme_element("pkgwide-fun:pre_conversion", default = identity), list(x))

  # converting row specifications to row numbers, and removing old cmds --------
  x <- .table_styling_expr_to_row_number(x)

  # creating list of kable calls -----------------------------------------------
  kable_calls <-
    table_styling_to_kable_calls(x = x, ...)
  if (return_calls == TRUE) {
    return(kable_calls)
  }

  # converting to character vector ---------------------------------------------
  include <-
    .select_to_varnames(
      select = {{ include }},
      var_info = names(kable_calls),
      arg_name = "include"
    )

  # making list of commands to include -----------------------------------------
  # this ensures list is in the same order as names(x$kable_calls)
  include <- names(kable_calls) %>% intersect(include)
  # user cannot exclude the first 'kable' command
  include <- "tibble" %>% union(include)

  # taking each kable function call, concatenating them with %>% separating them
  .eval_list_of_exprs(kable_calls[include])
}

table_styling_to_kable_calls <- function(x, ...) {
  dots <- rlang::enexprs(...)

  if (!is.null(dots[["fmt_missing"]])) {
    lifecycle::deprecate_warn(
      when = "1.6.0",
      what = "gtsummary::as_kable_extra(fmt_missing=)"
    )
    dots <- purrr::list_modify(fmt_missing = NULL) %>% purrr::compact()
  }

  kable_calls <-
    table_styling_to_tibble_calls(x, col_labels = FALSE, fmt_missing = TRUE)

  # fmt_missing ----------------------------------------------------------------
  kable_calls[["fmt_missing"]] <-
    c(
      kable_calls[["fmt_missing"]],
      list(expr(dplyr::mutate_all(~ ifelse(is.na(.), "", .))))
    )

  # remove_line_breaks ---------------------------------------------------------
  kable_calls[["remove_line_breaks"]] <-
    expr(dplyr::mutate(
      dplyr::across(.cols = where(is.character),
                    function(x) stringr::str_replace_all(x, pattern = "\\n(?!\\\\)", replacement = ""))))

  # kable ----------------------------------------------------------------------
  kable_calls[["kable"]] <- .construct_call_to_kable(x, ...)

  kable_calls
}

# constructs call to kable, and allows users to overwrite default arg values.
.construct_call_to_kable <- function(x, ...) {
  dots <- rlang::dots_list(...)

  kable_args <-
    # default args
    list(
      caption = x$table_styling$caption,
      col.names =
        dplyr::filter(x$table_styling$header, .data$hide == FALSE)$label %>%
        stringr::str_replace_all(pattern = "\\n(?!\\\\)", replacement = ""),
      align =
        filter(x$table_styling$header, .data$hide == FALSE) %>%
          dplyr::pull("align") %>%
          stringr::str_sub(1, 1)
    ) %>%
    # update with any args from theme element
    purrr::list_modify(!!!get_theme_element("as_kable-arg:dots")) %>%
    # update with any args user passed values
    purrr::list_modify(!!!dots) %>%
    purrr::compact()

  expr(knitr::kable(!!!kable_args))
}
