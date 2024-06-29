#' Add p-value
#'
#' Calculate and add a p-value comparing the two variables in the cross table.
#' If missing levels are included in the tables, they are also included in p-value calculation.
#'
#' @param x (`tbl_cross`)\cr
#'   Object with class `tbl_cross` created with the [`tbl_cross()`] function
#' @param pvalue_fun (`function`)\cr
#'   Function to round and format p-value.
#'   Default is `label_style_pvalue(digits = 1)`, except when `source_note = TRUE` when the
#'   default is `label_style_pvalue(digits = 1, prepend_p = TRUE)`
#' @param source_note (scalar `logical`)\cr
#'   Logical value indicating whether to show p-value
#'   in the \{gt\} table source notes rather than a column.
#' @param test (`string`)\cr
#'   A string specifying statistical test to perform. Default is
#'   `"chisq.test"` when expected cell counts >=5 and "`fisher.test`" when
#'   expected cell counts <5.
#' @param test.args (named `list`)\cr
#'   Named list containing additional arguments to pass to
#'   the test (if it accepts additional arguments).
#'   For example, add an argument for a chi-squared test with
#'   `test.args = list(correct = TRUE)`
#' @inheritParams add_p.tbl_summary
#' @family tbl_cross tools
#' @author Karissa Whiting, Daniel D. Sjoberg
#' @export
#' @examples
#' # Example 1 ----------------------------------
#' trial |>
#'   tbl_cross(row = stage, col = trt) |>
#'   add_p()
#'
#' # Example 2 ----------------------------------
#' trial |>
#'   tbl_cross(row = stage, col = trt) |>
#'   add_p(source_note = TRUE)
add_p.tbl_cross <- function(x,
                            test = NULL,
                            pvalue_fun =
                              ifelse(
                                source_note,
                                label_style_pvalue(digits = 1, prepend_p = TRUE),
                                label_style_pvalue(digits = 1)
                              ),
                            source_note = FALSE,
                            test.args = NULL,
                            ...) {
  set_cli_abort_call()
  check_dots_empty()
  updated_call_list <- c(x$call_list, list(add_p = match.call()))

  # check inputs ---------------------------------------------------------------
  if (missing(test)) {
    test <- get_theme_element("add_p.tbl_cross-arg:test", default = test)
  }
  if (missing(test.args)) {
    test <- get_theme_element("add_p.tbl_cross-arg:test.args", default = test.args)
  }

  if (missing(source_note)) {
    source_note <- get_theme_element("add_p.tbl_cross-arg:source_note", default = source_note)
  }
  check_scalar_logical(source_note)

  if (missing(pvalue_fun)) {
    pvalue_fun <-
      get_deprecated_theme_element("add_p.tbl_cross-arg:pvalue_fun") %||%
      get_theme_element("pkgwide-fn:pvalue_fun", default = pvalue_fun)
  }
  pvalue_fun <- as_function(pvalue_fun)

  # saving function inputs
  tbl_cross_inputs <- as.list(environment())

  # prepping arguments to pass to `add_p.tbl_summary()` ------------------------
  # adding test name if supplied (NULL otherwise)
  input_test <- switch(!is_empty(test), everything() ~ test)
  input_test.args <- switch(!is_empty(test.args), everything() ~ test.args)

  # calculating test result ----------------------------------------------------
  # running add_p to add the p-value to the output
  original_x_inputs <- x$inputs
  # passing the data frame after missing values have been transformed to factor/observed levels
  x$inputs$data <- x$tbl_data
  x$inputs$by <- original_x_inputs$col
  x$inputs$include <- original_x_inputs$row
  x$inputs$type <- list("categorical") |> set_names(original_x_inputs$row)
  x <- x |>
    add_p.tbl_summary(
      test = input_test,
      test.args = input_test.args,
      pvalue_fun = pvalue_fun,
      include = all_of(original_x_inputs$row)
    )
  # replacing the input data set with the original from the `tbl_cross()` call
  x$inputs <- original_x_inputs

  # report p-value as source note ----------------------------------------------
  if (source_note == TRUE) {
    test_name <-
      x$table_styling$footnote |>
      dplyr::filter(.data$column %in% "p.value") |>
      dplyr::pull("footnote")

    # report p-value as a source_note, hiding p-value column from output
    x <- x |>
      modify_table_styling(
        columns = "p.value",
        footnote = NA_character_,
        hide = TRUE
      )

    x$table_styling$source_note <-
      paste(test_name, pvalue_fun(discard(x$table_body$p.value, is.na)), sep = ", ")
    attr(x$table_styling$source_note, "text_interpret") <- "md"
  }

  # strip markdown bold around column label ------------------------------------
  x$table_styling$header <-
    x$table_styling$header %>%
    mutate(
      label =
        ifelse(
          .data$column %in% "p.value",
          str_replace_all(.data$label,
                          pattern = "\\*\\*(.*?)\\*\\*",
                          replacement = "\\1"
          ),
          .data$label
        )
    )

  # return tbl_cross -----------------------------------------------------------
  x$call_list <- updated_call_list
  x


}
