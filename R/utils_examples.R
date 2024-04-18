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
                                 out_var_name = deparse(substitute(tbl)),
                                 method = c("txt", "rds")[2]) {
  if (isFALSE(method %in% c("txt", "rds"))) {
    stop("Inserted method is not supported.")
  }

  if (getOption("gtsummary_update_examples", default = FALSE)) {
    if (is(tbl, "html")) {
      html_output <- tbl
    } else if (is(tbl, "gtsummary")) {
      html_output <- tbl |>
        as_gt() |>
        gt::tab_options() |>
        gt::as_raw_html()
    } else {
      stop("tbl needs a html or gtsummary table. Got something else.")
    }
    fl_nm <- file.path(
      sub("(^.*/gtsummary).*", "\\1", getwd()),
      "inst",
      "example_outputs",
      paste0(example_name, "_", out_var_name, ".", method)
    )
    if (method == "txt") {
      writeLines(text = html_output, con = fl_nm)
    } else if (method == "rds") {
      saveRDS(html_output, file = fl_nm)
    }
  }
}
#' @keywords internal
#' @name utils_examples
read_example_output <- function(out_var_name,
                                example_name = "example",
                                method = c("txt", "rds")[2]) {
  fl_nm <- file.path(
    sub("(^.*/gtsummary).*", "\\1", getwd()),
    "inst",
    "example_outputs",
    paste0(example_name, "_", out_var_name, ".", method)
  )
  if (isFALSE(method %in% c("txt", "rds"))) {
    stop("Inserted method is not supported.")
  }

  if (require("htmltools", quietly = TRUE) && file.exists(fl_nm)) {
    if (method == "txt") {
      out <- readLines(fl_nm) |>
        paste0(collapse = "\n") |>
        htmltools::HTML()
    } else if (method == "rds") {
      out <- readRDS(fl_nm)
    }
    return(out)
  }
}
#' @keywords internal
#' @name utils_examples
write_read_example_output <- function(tbl,
                                      example_name = "example",
                                      out_var_name = deparse(substitute(tbl)),
                                      method = c("txt", "rds")[2]) {
  write_example_output(tbl, example_name, out_var_name, method)
  read_example_output(out_var_name, example_name, method)
}
