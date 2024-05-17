#' Utilities to write and read tables in html for function documentation
#'
#' This is still experimental. We are planning to potentially expand this to other
#' html output. Principally based on [as_gt()] and [gt::as_raw_html()].
#'
#' @param tbl (`gtsummary` or `html`)\cr tables are converted to html. Any html code (table or not)
#'   can be saved and retrieved for documentation purposes.
#' @param example_name (`string`)\cr usually the name of the function or file where the
#'   table is used.
#' @param out_var_name (`string`)\cr the name of the variable that will be saved. It defaults
#'   to the name of the variable passed to the function (`tbl`).
#' @param method (`string`)\cr saving method. Default is `"rds"` as it produces a smaller file,
#'   but we are planning to extend this to other, non-html outputs like `"png"`.
#'
#' @keywords internal
#' @name utils_examples
write_example_output <- function(tbl,
                                 example_name = "example",
                                 out_var_name = deparse(substitute(tbl))) {
  if (getOption("gtsummary_update_examples", default = FALSE)) {
    if (is(tbl, "gtsummary")) {
      tbl <- tbl |>
        as_gt()
    }
    if (is(tbl, "gt_tbl")) {
      tbl <- tbl |>
        gt::tab_options() |>
        gt::as_raw_html()
    }
    if (!is(tbl, "html")) {
      stop("tbl needs to be a html, gtsummary or gt table. Got something else.")
    }
    fl_nm <- file.path(
      sub("(^.*/gtsummary).*", "\\1", getwd()),
      "inst",
      "example_outputs",
      paste0(example_name, "_", out_var_name, ".rds")
    )
    saveRDS(tbl, file = fl_nm)
  }
}
#' @keywords internal
#' @name utils_examples
read_example_output <- function(out_var_name, example_name = "example") {
  fl_nm <- file.path(
    sub("(^.*/gtsummary).*", "\\1", getwd()),
    "inst",
    "example_outputs",
    paste0(example_name, "_", out_var_name, ".rds")
  )

  if (require("htmltools", quietly = TRUE) && file.exists(fl_nm)) {
    readRDS(fl_nm)
  }
}
#' @keywords internal
#' @name utils_examples
write_read_example_output <- function(tbl,
                                      example_name = "example",
                                      out_var_name = deparse(substitute(tbl))) {
  write_example_output(tbl, example_name, out_var_name)
  read_example_output(out_var_name, example_name)
}
