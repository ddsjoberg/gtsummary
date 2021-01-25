#' Convert gtsummary object to a tibble
#'
#' Function converts a gtsummary object to a tibble.
#'
#' @inheritParams as_kable
#' @param col_labels Logical argument adding column labels to output tibble.
#' Default is `TRUE`.
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
                                return_calls = FALSE, exclude = NULL,  ...) {
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

  # converting row specifications to row numbers, and removing old cmds --------
  x <- .clean_table_body_stylings(x)

  # creating list of calls to get formatted tibble -----------------------------
  tibble_calls <- table_header_to_tibble_calls(x = x, col_labels = col_labels)

  # converting to character vector ---------------------------------------------
  include <-
    .select_to_varnames(
      select = {{ include }},
      var_info = names(tibble_calls),
      arg_name = "include"
    )
  exclude <-
    .select_to_varnames(
      select = {{ exclude }},
      var_info = names(tibble_calls),
      arg_name = "exclude"
    )

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


table_header_to_tibble_calls <- function(x, col_labels =  TRUE) {
  tibble_calls <- list()

  # tibble ---------------------------------------------------------------------
  tibble_calls[["tibble"]] <- expr(x$table_body)

  # ungroup --------------------------------------------------------------------
  group_var <- select(x$table_body, dplyr::group_cols()) %>% names()
  if (length(group_var) > 0) {
    if (group_var != "groupname_col")
      stop("`.$table_body` may only be grouped by column 'groupname_col'")

    tibble_calls[["ungroup"]] <- list(
      expr(mutate(groupname_col =
                    ifelse(dplyr::row_number() == 1, .data$groupname_col, NA))),
      expr(ungroup())
    )
  }

  # fmt (part 1) ---------------------------------------------------------------
  # this needs to be called in as_tibble() before the bolding and italic function,
  # but the bolding and italic code needs to executed on pre-formatted data
  # (e.g. `bold_p()`) this holds its place for when it is finally run
  tibble_calls[["fmt"]] <- list()

  # tab_style_bold -------------------------------------------------------------
  df_bold <- x$table_body_styling$text_format %>% filter(.data$format_type == "bold")

  tibble_calls[["tab_style_bold"]] <-
    map(
      seq_len(nrow(df_bold)),
      ~ expr(mutate_at(gt::vars(!!!syms(df_bold$column[[.x]])),
                       ~ifelse(row_number() %in% !!df_bold$row_numbers[[.x]],
                               paste0("__", ., "__"), .)))
    )

  # tab_style_italic -------------------------------------------------------------
  df_italic <- x$table_body_styling$text_format %>% filter(.data$format_type == "italic")

  tibble_calls[["tab_style_italic"]] <-
    map(
      seq_len(nrow(df_italic)),
      ~ expr(mutate_at(gt::vars(!!!syms(df_italic$column[[.x]])),
                       ~ifelse(row_number() %in% !!df_italic$row_numbers[[.x]],
                               paste0("_", ., "_"), .)))
    )

  # fmt (part 2) ---------------------------------------------------------------
  tibble_calls[["fmt"]] <-
    list(expr(mutate_at(vars(!!!syms(.cols_to_show(x))), as.character))) %>%
    c(map(
      seq_len(nrow(x$table_body_styling$fmt_fun)),
      ~expr(gtsummary:::.apply_fmt_fun(
        columns = !!x$table_body_styling$fmt_fun$column[[.x]],
        row_numbers = !!x$table_body_styling$fmt_fun$row_numbers[[.x]],
        fmt_fun = !!x$table_body_styling$fmt_fun$fmt_fun[[.x]],
        update_from = !!x$table_body
      ))
    ))

  # cols_hide ------------------------------------------------------------------
  # cols_to_keep object created above in fmt section
  tibble_calls[["cols_hide"]] <- expr(dplyr::select(any_of("groupname_col"), !!!syms(.cols_to_show(x))))

  # cols_label -----------------------------------------------------------------
  if (col_labels) {
    df_col_labels <-
      dplyr::filter(x$table_body_styling$header, .data$hide == FALSE)

    tibble_calls[["cols_label"]] <-
      expr(rlang::set_names(!!df_col_labels$label))
  }

  tibble_calls
}

.apply_fmt_fun <- function(data, columns, row_numbers, fmt_fun, update_from) {
  if (all(is.na(row_numbers))) row_numbers <- seq_len(nrow(data))
  data[row_numbers, columns, drop = FALSE] <-
    update_from[row_numbers, columns, drop = FALSE] %>%
    purrr::map_dfc(~fmt_fun(.x))

  data
}
