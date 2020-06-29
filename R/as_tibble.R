#' Convert gtsummary object to a tibble
#'
#' Function converts gtsummary objects tibbles. The formatting stored in
#' `x$kable_calls` is applied.
#'
#' @inheritParams as_kable
#' @param col_labels Logical argument adding column labels to output tibble.
#' Default is `TRUE`.
#' @param group_header If a group is present (e.g. via `tbl_merge(group_header=)`),
#' this string specifies the column header for the grouping column. Default
#' is `"**Group**"`
#' @param ... Not used
#' @return a [tibble][tibble::tibble-package]
#' @family gtsummary output types
#' @author Daniel D. Sjoberg
#' @export
#' @examples
#' tbl <-
#'   trial %>%
#'   select(trt, age, grade, response) %>%
#'   tbl_summary(by = trt)
#'
#' as_tibble(tbl)
#'
#' # without column labels
#' as_tibble(tbl, col_labels = FALSE)
as_tibble.gtsummary <- function(x, include = everything(), col_labels = TRUE,
                                return_calls = FALSE, group_header = NULL,
                                exclude = NULL,  ...) {
  # DEPRECATION notes ----------------------------------------------------------
  if (!rlang::quo_is_null(rlang::enquo(exclude))) {
    lifecycle::deprecate_warn(
      "1.2.5",
      "gtsummary::as_tibble(exclude = )",
      "as_tibble(include = )",
      details = paste0(
        "The `include` argument accepts quoted and unquoted expressions similar\n",
        "to `dplyr::select()`. To exclude commands, use the minus sign.\n",
        "For example, `include = -cols_hide`"
      )
    )
  }

  # setting defaults -----------------------------------------------------------
  group_header <-
    group_header %||%
    get_theme_element("pkgwide-str:group_header", default = "**Group**")

  # creating list of calls to get formatted tibble -----------------------------
  tibble_calls <- table_header_to_tibble_calls(x = x, col_labels = col_labels,
                                               group_header = group_header)

  # converting to character vector ---------------------------------------------
  include <- var_input_to_string(data = vctr_2_tibble(names(tibble_calls)),
                                 select_input = !!rlang::enquo(include))
  exclude <- var_input_to_string(data = vctr_2_tibble(names(tibble_calls)),
                                 select_input = !!rlang::enquo(exclude))

  # making list of commands to include -----------------------------------------
  # this ensures list is in the same order as names(x$kable_calls)
  include <- names(tibble_calls) %>% intersect(include)

  # user cannot exclude the first 'tibble' command
  include <- include %>% setdiff(exclude)
  include <- "tibble" %>% union(include)

  # return calls, if requested -------------------------------------------------
  if (return_calls == TRUE) return(tibble_calls[include])

  # taking each gt function call, concatenating them with %>% separating them
  tibble_calls[include] %>%
    # removing NULL elements
    unlist() %>%
    compact() %>%
    # concatenating expressions with %>% between each of them
    reduce(function(x, y) expr(!!x %>% !!y)) %>%
    # evaluating expressions
    eval()
}


table_header_to_tibble_calls <- function(x, col_labels =  TRUE, group_header) {
  table_header <- x$table_header
  tibble_calls <- list()

  # tibble ---------------------------------------------------------------------
  tibble_calls[["tibble"]] <- expr(x$table_body)

  # ungroup --------------------------------------------------------------------
  group_var <- select(x$table_body, dplyr::group_cols()) %>% names()
  if (length(group_var) > 0) {
    tibble_calls[["ungroup"]] <- list(
      expr(ungroup()),
      expr(mutate(...new_group_var... = !!sym(group_var))),
      expr(group_by(.data$...new_group_var...)),
      expr(mutate_at(vars(!!sym(group_var)), ~ifelse(dplyr::row_number() == 1, ., NA))),
      expr(ungroup()),
      expr(select(-.data$...new_group_var...))
    )
  }

  # fmt (part 1) ---------------------------------------------------------------
  # this needs to be called in as_tibble() before the bolding and italic function,
  # but the bolding and italic code needs to executed on pre-formatted data
  # (e.g. `bold_p()`) this holds its place for when it is finally run
  tibble_calls[["fmt"]] <- list()

  # tab_style_bold -------------------------------------------------------------
  df_tab_style_bold <-
    table_header %>%
    filter(!is.na(.data$bold)) %>%
    mutate(# lgl indicating the rows to bold
      row_id = map(.data$bold, ~with(x$table_body, eval(parse_expr(.x))))
    )

  tibble_calls[["tab_style_bold"]] <-
    map(
      seq_len(nrow(df_tab_style_bold)),
      ~ expr(mutate_at(gt::vars(!!!syms(df_tab_style_bold$column[[.x]])),
                       ~ifelse(!!df_tab_style_bold$row_id[[.x]], paste0("__", ., "__"), .)))
    )

  # tab_style_italic -------------------------------------------------------------
  df_tab_style_italic <- table_header %>%
    filter(!is.na(.data$italic)) %>%
    mutate(# lgl indicating the rows to italicize
      row_id = map(.data$italic, ~with(x$table_body, eval(parse_expr(.x))))
    )

  tibble_calls[["tab_style_italic"]] <-
    map(
      seq_len(nrow(df_tab_style_italic)),
      ~ expr(mutate_at(gt::vars(!!!syms(df_tab_style_italic$column[[.x]])),
                       ~ifelse(!!df_tab_style_italic$row_id[[.x]], paste0("_", ., "_"), .)))
    )

  # fmt (part 2) ---------------------------------------------------------------
  df_fmt <- table_header %>%
    filter(map_lgl(.data$fmt_fun, ~!is.null(.x)))

  tibble_calls[["fmt"]] <- map(
    seq_len(nrow(df_fmt)),
    ~ expr(mutate_at(vars(!!!syms(df_fmt$column[[.x]])), !!df_fmt$fmt_fun[[.x]]))
  )

  # converting all cols to character...
  # this is important for some output types, e.g. as_flextable, so missing don't
  # display as NA
  cols_to_keep <-
    dplyr::filter(table_header, .data$hide == FALSE) %>%
    pull(.data$column)

  tibble_calls[["fmt"]] <-
    c(tibble_calls[["fmt"]], list(expr(mutate_at(vars(!!!syms(cols_to_keep)), as.character))))

  # cols_hide ------------------------------------------------------------------
  # cols_to_keep object created above in fmt section
  tibble_calls[["cols_hide"]] <- expr(dplyr::select(any_of("groupname_col"), !!!syms(cols_to_keep)))

  # cols_label -----------------------------------------------------------------
  if (col_labels) {
    df_col_labels <-
      dplyr::filter(table_header, .data$hide == FALSE)

    # if there is a grouping variable, add the column header
    if (length(group_var) > 0)
      tibble_calls[["cols_label"]] <-
        expr(rlang::set_names(c(!!group_header, !!df_col_labels$label)))
    else
      tibble_calls[["cols_label"]] <-
        expr(rlang::set_names(!!df_col_labels$label))
  }

  tibble_calls
}
