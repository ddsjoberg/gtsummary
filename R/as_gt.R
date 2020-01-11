#' Convert gtsummary object to a gt_tbl object
#'
#' Function converts gtsummary objects to a gt_tbl objects.
#' Function is used in the background when the results are printed or knit.
#' A user can use this function if they wish to add customized formatting
#' available via the [gt package](https://gt.rstudio.com/index.html).
#' Review the
#' \href{http://www.danieldsjoberg.com/gtsummary/articles/tbl_summary.html#advanced}{tbl_summary vignette}
#' or
#' \href{http://www.danieldsjoberg.com/gtsummary/articles/tbl_regression.html#advanced}{tbl_regression vignette}
#' for detailed examples in the 'Advanced Customization' section.
#'
#' @param x Object created by a function from the gtsummary package
#' (e.g. [tbl_summary] or [tbl_regression])
#' @param include Commands to include in output. Input may be a vector of
#' quoted or unquoted names. tidyselect and gtsummary select helper
#' functions are also accepted.
#' Default is `everything()`, which includes all commands in `x$gt_calls`.
#' @param exclude DEPRECATED.
#' @param omit DEPRECATED.
#' vector of named gt commands to omit. Default is `NULL`
#' @export
#' @return A `gt_tbl` object
#' @seealso [tbl_summary] [tbl_regression] [tbl_uvregression] [tbl_survival]
#' @author Daniel D. Sjoberg
#' @examples
#' \donttest{
#' as_gt_ex <-
#'   trial[c("trt", "age", "response", "grade")] %>%
#'   tbl_summary(by = trt) %>%
#'   as_gt()
#' }
#' @section Example Output:
#'
#' \if{html}{\figure{as_gt_ex.png}{options: width=50\%}}

as_gt <- function(x, include = everything(), exclude = NULL, omit = NULL) {
  # DEPRECATED notice ---------------------------------------------------------
  # checking if updated version of gt package is required 2019-12-25 -----------
  if (!exists("cells_body", asNamespace("gt"))) {
    usethis::ui_oops(glue(
      "An updated version of the gt package is required to print table.\n",
      "To install the most recent version of gt, run"
    ))
    usethis::ui_code_block('remotes::install_github("rstudio/gt", ref = gtsummary::gt_sha)')
    return(invisible())
  }

  # making list of commands to include -----------------------------------------
  if (!rlang::quo_is_null(rlang::enquo(exclude))) {
    lifecycle::deprecate_warn(
      "1.2.5",
      "gtsummary::as_gt(exclude = )",
      "as_gt(include = )",
      details = paste0(
        "The `include` argument accepts quoted and unquoted expressions similar\n",
        "to `dplyr::select()`. To exclude commands, use the minus sign.\n",
        "For example, `include = -tab_spanner`"
      )
    )
  }

  if (!is.null(omit)) {
    lifecycle::deprecate_warn(
      "1.2.0",
      "gtsummary::as_gt(omit = )",
      "as_gt(include = )",
      details = paste0(
        "The `include` argument accepts quoted and unquoted expressions similar\n",
        "to `dplyr::select()`. To exclude commands, use the minus sign.\n",
        "For example, `include = -tab_spanner`"
      )
    )
    exclude <- omit
  }

  # converting to charcter vector ----------------------------------------------
  include <- var_input_to_string(data = vctr_2_tibble(names(x$gt_calls)),
                                 select_input = !!rlang::enquo(include))
  exclude <- var_input_to_string(data = vctr_2_tibble(names(x$gt_calls)),
                                 select_input = !!rlang::enquo(exclude))

  # this ensures list is in the same order as names(x$gt_calls)
  include <- names(x$gt_calls) %>% intersect(include)

  # user cannot omit the first 'gt' command
  include <- include %>% setdiff(exclude)
  include <- "gt" %>% union(include)

  # taking each gt function call, concatenating them with %>% separating them
  x$gt_calls[include] %>%
    # adding default gt formatting options
    union(getOption("gtsummary.as_gt.addl_cmds", default = NULL)) %>%
    # removing NULL elements
    compact() %>%
    glue_collapse(sep = " %>% ") %>%
    # converting strings into expressions to run
    parse(text = .) %>%
    eval()
}
