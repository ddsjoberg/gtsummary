#' Utilities to write and read tables in html for function documentation
#'
#' This is still experimental. We are planning to potentially expand this to other
#' html output. Principally based on [as_gt()] and [gt::as_raw_html()].
#'
#' @param tbl
#'
#'
#' @keywords internal
#' @name utils_examples
write_html_output <- function(tbl,
                              example_name = "example",
                              out_var_name = deparse(substitute(tbl)),
                              method = c("txt", "rds")[2]) {
  folder_name <- system.file("html_example_outputs", package = "gtsummary")
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
    fl_nm <- paste0(folder_name, "/", example_name, "_", out_var_name)
    if (method == "txt") {
      writeLines(text = html_output, con = paste0(fl_nm, ".txt"))
    } else if (method == "rds") {
      saveRDS(html_output, file = paste0(fl_nm, ".rds"))
    }
  }
}
#' @keywords internal
#' @name utils_examples
read_html_output <- function(out_var_name,
                             example_name = "example",
                             method = c("txt", "rds")[2]) {
  folder_name <- system.file("html_example_outputs", package = "gtsummary")
  if (isFALSE(method %in% c("txt", "rds"))) {
    stop("Inserted method is not supported.")
  }

  fl_nm <- paste0(
    folder_name, "/", # should we use system.file?
    example_name, "_", out_var_name, ".", method
  )

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
write_and_read_html_output <- function(tbl,
                                       example_name = "example",
                                       out_var_name = deparse(substitute(tbl)),
                                       method = c("txt", "rds")[2]) {
  write_html_output(tbl, example_name, out_var_name, method)
  read_html_output(out_var_name, example_name, method)
}
