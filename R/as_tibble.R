#' Convert gtsummary object to a tibble
#'
#' Function converts a gtsummary object to a tibble.
#'
#' @inheritParams as_gt
#' @param col_labels (scalar `logical`)\cr
#'   Logical argument adding column labels to output tibble. Default is `TRUE`.
#' @param fmt_missing (scalar `logical`)\cr
#'   Logical argument adding the missing value formats.
#' @inheritParams rlang::args_dots_empty
#'
#' @return a [tibble](https://tibble.tidyverse.org/)
#'
#' @author Daniel D. Sjoberg
#' @name as_tibble.gtsummary
#' @examples
#' tbl <-
#'   trial |>
#'   tbl_summary(by = trt, include = c(age, grade, response))
#'
#' as_tibble(tbl)
#'
#' # without column labels
#' as_tibble(tbl, col_labels = FALSE)
NULL

#' @export
#' @rdname as_tibble.gtsummary
as_tibble.gtsummary <- function(x, include = everything(), col_labels = TRUE,
                                return_calls = FALSE, fmt_missing = FALSE, ...) {
  set_cli_abort_call()
  # running pre-conversion function, if present --------------------------------
  x <- do.call(get_theme_element("pkgwide-fun:pre_conversion", default = identity), list(x))

  # converting row specifications to row numbers, and removing old cmds --------
  x <- .table_styling_expr_to_row_number(x)

  # creating list of calls to get formatted tibble -----------------------------
  tibble_calls <-
    table_styling_to_tibble_calls(
      x = x,
      col_labels = col_labels,
      fmt_missing = fmt_missing
    )

  # converting to character vector ---------------------------------------------
  cards::process_selectors(
    data = vec_to_df(names(tibble_calls)),
    include = {{ include }}
  )

  # making list of commands to include -----------------------------------------
  # this ensures list is in the same order as names(x$kable_calls)
  include <- names(tibble_calls) %>% intersect(include)
  # user cannot exclude the first 'tibble' command
  include <- "tibble" %>% union(include)

  # return calls, if requested -------------------------------------------------
  if (return_calls == TRUE) {
    return(tibble_calls[include])
  }

  # taking each gt function call, concatenating them with %>% separating them
  .eval_list_of_exprs(tibble_calls[include])
}

#' @export
#' @rdname as_tibble.gtsummary
as.data.frame.gtsummary <- function(...) {
  set_cli_abort_call()
  res <- as_tibble(...)

  if (inherits(res, "data.frame")) {
    return(as.data.frame(res))
  }

  res
}


table_styling_to_tibble_calls <- function(x, col_labels = TRUE, fmt_missing = FALSE) {
  tibble_calls <- list()

  # tibble ---------------------------------------------------------------------
  tibble_calls[["tibble"]] <- expr(x$table_body)

  # ungroup --------------------------------------------------------------------
  if ("groupname_col" %in% x$table_styling$header$column) {
    tibble_calls[["ungroup"]] <-
      list(
        expr(dplyr::group_by(.data$groupname_col)),
        expr(dplyr::mutate(groupname_col = ifelse(dplyr::row_number() == 1,
          as.character(.data$groupname_col),
          NA_character_
        ))),
        expr(dplyr::ungroup())
      )
  }

  # fmt (part 1) ---------------------------------------------------------------
  # this needs to be called in as_tibble() before the bolding and italic function,
  # but the bolding and italic code needs to executed on pre-formatted data
  # (e.g. `bold_p()`) this holds its place for when it is finally run
  tibble_calls[["fmt"]] <- list()

  # cols_merge -----------------------------------------------------------------
  tibble_calls[["cols_merge"]] <-
    map(
      seq_len(nrow(x$table_styling$cols_merge)),
      ~ expr(
        dplyr::mutate(
          !!x$table_styling$cols_merge$column[.x] :=
            ifelse(
              dplyr::row_number() %in% !!x$table_styling$cols_merge$rows[[.x]],
              glue::glue(!!x$table_styling$cols_merge$pattern[.x]) %>% as.character(),
              !!rlang::sym(x$table_styling$cols_merge$column[.x])
            )
        )
      )
    )

  # tab_style_bold -------------------------------------------------------------
  df_bold <- x$table_styling$text_format %>% dplyr::filter(.data$format_type == "bold")

  tibble_calls[["tab_style_bold"]] <-
    map(
      seq_len(nrow(df_bold)),
      ~ expr(dplyr::mutate_at(
        gt::vars(!!!syms(df_bold$column[[.x]])),
        ~ ifelse(dplyr::row_number() %in% !!df_bold$row_numbers[[.x]],
          paste0("__", ., "__"), .
        )
      ))
    )

  # tab_style_italic -------------------------------------------------------------
  df_italic <- x$table_styling$text_format %>% dplyr::filter(.data$format_type == "italic")

  tibble_calls[["tab_style_italic"]] <-
    map(
      seq_len(nrow(df_italic)),
      ~ expr(dplyr::mutate_at(
        gt::vars(!!!syms(df_italic$column[[.x]])),
        ~ ifelse(dplyr::row_number() %in% !!df_italic$row_numbers[[.x]],
          paste0("_", ., "_"), .
        )
      ))
    )

  # fmt (part 2) ---------------------------------------------------------------
  tibble_calls[["fmt"]] <-
    map(
      seq_len(nrow(x$table_styling$fmt_fun)),
      ~ expr((!!expr(!!eval(parse_expr("gtsummary:::.apply_fmt_fun"))))(
        columns = !!x$table_styling$fmt_fun$column[[.x]],
        row_numbers = !!x$table_styling$fmt_fun$row_numbers[[.x]],
        fmt_fun = !!x$table_styling$fmt_fun$fmt_fun[[.x]],
        update_from = !!x$table_body
      ))
    )

  # fmt_missing ----------------------------------------------------------------
  if (isTRUE(fmt_missing)) {
    tibble_calls[["fmt_missing"]] <-
      map(
        seq_len(nrow(x$table_styling$fmt_missing)),
        ~ expr(
          ifelse(
            dplyr::row_number() %in% !!x$table_styling$fmt_missing$row_numbers[[.x]] & is.na(!!sym(x$table_styling$fmt_missing$column[.x])),
            !!x$table_styling$fmt_missing$symbol[.x],
            !!sym(x$table_styling$fmt_missing$column[.x])
          )
        )
      ) %>%
      rlang::set_names(x$table_styling$fmt_missing$column) %>%
      {
        expr(dplyr::mutate(!!!.))
      } %>%
      list()
  } else {
    tibble_calls[["fmt_missing"]] <- list()
  }

  # cols_hide ------------------------------------------------------------------
  # cols_to_keep object created above in fmt section
  tibble_calls[["cols_hide"]] <-
    expr(dplyr::select(any_of("groupname_col"), !!!syms(.cols_to_show(x))))

  # cols_label -----------------------------------------------------------------
  if (col_labels) {
    df_col_labels <-
      dplyr::filter(x$table_styling$header, .data$hide == FALSE)

    tibble_calls[["cols_label"]] <-
      expr(rlang::set_names(!!df_col_labels$label))
  }

  tibble_calls
}

.apply_fmt_fun <- function(data, columns, row_numbers, fmt_fun, update_from) {
  # apply formatting functions
  df_updated <-
    update_from[row_numbers, columns, drop = FALSE] %>%
    map(~ fmt_fun(.x)) |>
    dplyr::bind_cols()

  # convert underlying column to character if updated col is character
  for (v in columns) {
    if (is.character(df_updated[[v]]) && !is.character(data[[v]])) {
      data[[v]] <- as.character(data[[v]])
    }
  }

  # update data and return
  data[row_numbers, columns, drop = FALSE] <- df_updated

  data
}
