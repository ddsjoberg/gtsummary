#' Convert to knitr_kable object
#'
#' @description Function converts a gtsummary object to a knitr_kable object.
#' This function is used in the background when the results are printed or knit.
#' A user can use this function if they wish to add customized formatting
#' available via [knitr::kable].
#'
#' @description Output from [knitr::kable] is less full featured compared to
#' summary tables produced with [gt](https://gt.rstudio.com/index.html).
#' For example, kable summary tables do not include indentation, footnotes,
#' or spanning header rows. To use these features, install gt with
#' `remotes::install_github("rstudio/gt", ref = gtsummary::gt_sha)`.
#'
#' @details Tip: To better distinguish variable labels and level labels when
#' indenting is not supported, try [bold_labels()] or [italicize_levels()].
#'
#' @param x Object created by a function from the gtsummary package
#' (e.g. [tbl_summary] or [tbl_regression])
#' @param include Commands to include in output. Input may be a vector of
#' quoted or unquoted names. tidyselect and gtsummary select helper
#' functions are also accepted.
#' Default is `everything()`, which includes all commands in `x$kable_calls`.
#' @param exclude DEPRECATED
#' @param ... Additional arguments passed to [knitr::kable]
#' @export
#' @return A `knitr_kable` object
#' @seealso [tbl_summary] [tbl_regression] [tbl_uvregression] [tbl_survival]
#' @author Daniel D. Sjoberg
#' @examples
#' trial %>%
#'   tbl_summary(by = trt) %>%
#'   bold_labels() %>%
#'   as_kable()

as_kable <- function(x, include = everything(), exclude = NULL, ...) {
  # checking if `as_kable()` was a default or user requested
  # 1. User didn't set a print engine for kable
  # 2. caller_env has an object called `print_engine`, which comes from print()
  if (is.null(getOption("gtsummary.print_engine")) &&
      !is.null(as.list(rlang::caller_env())$print_engine)) {
    rlang::inform(glue(
      "Results will be printed using `knitr::kable()` and do not \n",
      "support footnotes, spanning headers, or indentation. \n\n",
      "For tables styled by the gt package, use the installation code below.\n",
      "`remotes::install_github(\"rstudio/gt\", ref = gtsummary::gt_sha)`\n\n",
      "If you prefer to always use `knitr::kable()`, add the option\n",
      "`options(gtsummary.print_engine = \"kable\")` to your script\n",
      "or in a user- or project-level startup file, '.Rprofile'."
    ))
  }

  # DEPRECATION notes ----------------------------------------------------------
  if (!rlang::quo_is_null(rlang::enquo(exclude))) {
    lifecycle::deprecate_warn(
      "1.2.5",
      "gtsummary::as_kable(exclude = )",
      "as_kable(include = )",
      details = paste0(
        "The `include` argument accepts quoted and unquoted expressions similar\n",
        "to `dplyr::select()`. To exclude commands, use the minus sign.\n",
        "For example, `include = -cols_hide`"
      )
    )
  }

  # converting to charcter vector ----------------------------------------------
  include <- var_input_to_string(data = vctr_2_tibble(names(x$kable_calls)),
                                 select_input = !!rlang::enquo(include))
  exclude <- var_input_to_string(data = vctr_2_tibble(names(x$kable_calls)),
                                 select_input = !!rlang::enquo(exclude))

  # making list of commands to include -----------------------------------------
  # this ensures list is in the same order as names(x$kable_calls)
  include <- names(x$kable_calls) %>% intersect(include)

  # user cannot exclude the first 'kable' command
  include <- include %>% setdiff(exclude)
  include <- "kable" %>% union(include)

  # saving vector of column labels
  col_labels <-
    x$table_header %>%
    filter(.data$hide == FALSE) %>%
    pull(.data$label)

  # taking each kable function call, concatenating them with %>% separating them
  x$kable_calls[include] %>%
    # removing NULL elements
    compact() %>%
    glue_collapse(sep = " %>% ") %>%
    # converting strings into expressions to run
    parse(text = .) %>%
    eval() %>%
    # performing final modifcations prior to returning kable object
    mutate_all(~ ifelse(is.na(.), "", .)) %>%
    knitr::kable(col.names = col_labels, ...)
}
