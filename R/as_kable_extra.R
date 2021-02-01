#' Convert gtsummary object to a kableExtra object
#'
#' Function converts a gtsummary object to a knitr_kable + kableExtra object.
#' A user can use this function if they wish to add customized formatting
#' available via [knitr::kable] and {kableExtra}. Note that {gtsummary}
#' uses the standard markdown `**` to bold headers, and they may need to be
#' changed manually with kableExtra output.
#'
#' @inheritParams as_kable
#' @inheritParams as_flex_table
#' @export
#' @return A {kableExtra} object
#' @family gtsummary output types
#' @author Daniel D. Sjoberg
#' @examples
#' tbl <-
#'   trial %>%
#'   tbl_summary(by = trt) %>%
#'   as_kable_extra()

as_kable_extra <- function(x, include = everything(), return_calls = FALSE,
                           strip_md_bold = TRUE, ...) {
  # must have kableExtra package installed to use this function ----------------
  assert_package("kableExtra", "as_kable_extra()")

  # converting row specifications to row numbers, and removing old cmds --------
  x <- .clean_table_styling(x)

  # stripping markdown asterisk ------------------------------------------------
  if (strip_md_bold == TRUE) {
    x$table_styling$header <-
      x$table_styling$header %>%
      mutate(
        label = str_replace_all(
          .data$label, pattern = fixed("**"), replacement = fixed("")
        ),
        spanning_header = str_replace_all(
          .data$spanning_header, pattern = fixed("**"), replacement = fixed("")
        )
      )
  }

  # creating list of kableExtra calls ------------------------------------------
  kable_extra_calls <-
    table_styling_to_kable_extra_calls(x = x, ...)

  # adding user-specified calls ------------------------------------------------
  insert_expr_after <- get_theme_element("as_kable_extra-lst:addl_cmds")
  kable_extra_calls <-
    purrr::reduce(
      .x = seq_along(insert_expr_after),
      .f = function(x, y) add_expr_after(calls = x,
                                         add_after = names(insert_expr_after[y]),
                                         expr = insert_expr_after[[y]],
                                         new_name = paste0("user_added", y)),
      .init = kable_extra_calls
    )

  # converting to charcter vector ----------------------------------------------
  include <-
    .select_to_varnames(
      select = {{ include }},
      var_info = names(kable_extra_calls),
      arg_name = "include"
    )

  # making list of commands to include -----------------------------------------
  # this ensures list is in the same order as names(x$kable_calls)
  include <- names(kable_extra_calls) %>% intersect(include)
  # user cannot exclude the first 'kable' command
  include <- "tibble" %>% union(include)

  # return calls, if requested -------------------------------------------------
  if (return_calls == TRUE) return(kable_extra_calls)

  # taking each kable function call, concatenating them with %>% separating them
  kable_extra_calls[include] %>%
    # removing NULL elements
    unlist() %>%
    compact() %>%
    # concatenating expressions with %>% between each of them
    reduce(function(x, y) expr(!!x %>% !!y)) %>%
    # evaluating expressions
    eval()
}

table_styling_to_kable_extra_calls <- function(x, ...) {
  # getting kable calls
  kable_extra_calls <-
    table_header_to_kable_calls(x = x, ...)

  # add_indent -----------------------------------------------------------------
  df_indent <-
    x$table_styling$text_format %>%
    filter(.data$format_type == "indent", .data$column == "label")

  if (nrow(df_indent) > 0) {
    kable_extra_calls[["add_indent"]] <-
      expr(kableExtra::add_indent(!!df_indent$row_numbers[[1]]))
  }

  # add_header_above -----------------------------------------------------------
  if (any(!is.na(x$table_styling$header$spanning_header))) {
    header0 <- x$table_styling$header %>%
      filter(.data$hide == FALSE) %>%
      select(.data$spanning_header) %>%
      mutate(spanning_header = ifelse(is.na(.data$spanning_header),
                                      " ",
                                      .data$spanning_header)) %>%
      group_by(.data$spanning_header) %>%
      dplyr::summarise(n = n()) %>%
      ungroup()
    header <- header0$n %>% set_names(header0$spanning_header)

    kable_extra_calls[["add_header_above"]] <-
      expr(kableExtra::add_header_above(!!header))
  }

  # footnote -------------------------------------------------------------------
  vct_footnote <-
    .number_footnotes(x) %>%
    pull(.data$footnote) %>%
    unique()

  if(length(vct_footnote > 0))
    kable_extra_calls[["footnote"]] <-
    expr(kableExtra::footnote(number = !!vct_footnote))

  kable_extra_calls
}

