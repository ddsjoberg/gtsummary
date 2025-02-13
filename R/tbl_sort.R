#' Sort Hierarchical Tables
#'
#' @description `r lifecycle::badge('experimental')`\cr
#'
#' This function is used to sort hierarchical tables. Options for sorting criteria are:
#'
#' 1. Descending - within each section of the hierarchy table, event rate sums are calculated for each row and rows are
#'    sorted in descending order by sum (default).
#' 2. Alphanumeric - rows are ordered alphanumerically (i.e. A to Z) by label text. By default, [tbl_hierarchical()]
#'    sorts tables in alphanumeric order.
#'
#' @param x (`tbl_hierarchical`, `tbl_hierarchical_count`)\cr
#'   a hierarchical gtsummary table of class `'tbl_hierarchical'` or `'tbl_hierarchical_count'`.
#' @inheritParams cards::ard_sort
#' @inheritParams rlang::args_dots_empty
#'
#' @return A `gtsummary` of the same class as `x`.
#'
#' @name tbl_sort
#' @seealso [tbl_filter()]
#'
#' @examplesIf (identical(Sys.getenv("NOT_CRAN"), "true") || identical(Sys.getenv("IN_PKGDOWN"), "true"))
#' ADAE_subset <- cards::ADAE |>
#'   dplyr::filter(AETERM %in% unique(cards::ADAE$AETERM)[1:5])
#'
#' tbl <- tbl_hierarchical(
#'   data = ADAE_subset,
#'   variables = c(SEX, RACE, AETERM),
#'   by = TRTA,
#'   denominator = cards::ADSL |> mutate(TRTA = ARM),
#'   id = USUBJID,
#'   overall_row = TRUE
#' )
#'
#' # Example 1 - Descending Frequency Sort ------------------
#' tbl <- tbl_sort(tbl)
#' tbl
#'
#' # Example 2 - Alphanumeric Sort --------------------------
#' tbl <- tbl_sort(tbl, sort = "alphanumeric")
#' tbl
NULL

#' @rdname tbl_sort
#' @export
tbl_sort <- function(x, ...) {
  check_not_missing(x)
  check_class(x, "gtsummary")

  UseMethod("tbl_sort")
}

#' @rdname tbl_sort
#' @export
tbl_sort.tbl_hierarchical <- function(x, sort = "descending", ...) {
  set_cli_abort_call()

  ard_args <- attributes(x$cards$tbl_hierarchical)$args
  by_cols <- paste0("group", seq_along(length(ard_args$by)), c("", "_level"))
  x_ard <- x$cards$tbl_hierarchical

  # add dummy rows for variables not in include so their label rows are sorted correctly
  not_incl <- setdiff(ard_args$variables, ard_args$include)
  if (length(not_incl) > 0) {
    cli::cli_inform(
      "Not all hierarchy variables present in the table were included in the {.arg include} argument.
      These variables ({not_incl}) do not have event rate data available so the total sum of the event rates
      for this hierarchy section will be used instead. To use true event rates for all sections of the table,
      set {.code include = everything()} when creating your table via {.fun tbl_hierarchical}."
    )

    x_ard <- x_ard |> mutate(idx_o = seq_len(nrow(x$cards$tbl_hierarchical)))
    for (v in not_incl) {
      i <- length(ard_args$by) + which(ard_args$variables == v)
      x_sum_rows <- x_ard |>
        dplyr::group_by(across(all_of(cards::all_ard_group_n((length(ard_args$by) + 1):i)))) |>
        dplyr::group_map(function(.df, .g) {
          # get pseudo-summary row stat value for descending sort
          if (sort == "descending") {
            stat_nm <- setdiff(.df$stat_name, "N")[1]
            sum <- .df |>
              dplyr::filter(stat_name == !!stat_nm) |>
              dplyr::summarize(s = sum(unlist(stat))) |>
              dplyr::pull(s)
          }
          g_cur <- .g[[ncol(.g) - 1]]
          if (!is.na(g_cur) && g_cur == v) {
            # dummy summary row to add in
            .df[1, ] |> mutate(
              variable = g_cur,
              variable_level = .g[[ncol(.g)]],
              stat_name = if (sort == "descending") stat_nm else "no_stat",
              stat = if (sort == "descending") list(sum) else list(0),
              idx_o = min(.df$idx_o) + 1,
              tmp = TRUE
            )
          } else {
            NULL
          }
        }, .keep = TRUE)
      sum_row_pos <- dplyr::bind_rows(x_sum_rows) |> dplyr::pull(idx_o)
      # adjust prior row indices to add in dummy summary rows
      x_ard <- x_ard |>
        dplyr::bind_rows(x_sum_rows) |>
        dplyr::rowwise() |>
        mutate(idx_o = .data$idx_o + sum(sum_row_pos > .data$idx_o)) |>
        dplyr::ungroup()
    }
    x_ard <- x_ard |>
      dplyr::arrange(idx_o, tmp) |>
      select(-"idx_o")
  }

  # add indices to ARD
  x_ard <- x_ard |>
    dplyr::group_by(across(c(cards::all_ard_groups(), cards::all_ard_variables(), -all_of(by_cols)))) |>
    dplyr::mutate(idx_sort = dplyr::cur_group_id()) |>
    dplyr::ungroup()

  # re-add dropped args attribute
  x_ard <- x_ard |> cards::as_card()
  attr(x_ard, "args") <- ard_args

  # get `by` variable count rows (do not correspond to a table row)
  rm_idx <- x_ard |>
    dplyr::filter(is.na(group1)) |>
    dplyr::pull("idx_sort") |>
    unique()

  # pull index order (each corresponding to one row of x$table_body)
  pre_sort_idx <- x_ard |>
    dplyr::pull("idx_sort") |>
    unique() |>
    setdiff(rm_idx) |>
    as.character()

  # apply sorting
  x_ard_sort <- x_ard |> cards::ard_sort(sort)

  # pull updated index order after sorting
  post_sort_idx <- x_ard_sort |>
    dplyr::pull("idx_sort") |>
    unique() |>
    setdiff(rm_idx) |>
    as.character()

  # get updated (relative) row positions
  idx <- (seq_len(length(pre_sort_idx)) |> stats::setNames(pre_sort_idx))[post_sort_idx]

  # update x$cards
  if ("tmp" %in% names(x_ard_sort)) {
    x_ard_sort <- x_ard_sort |>
      dplyr::filter(is.na(tmp)) |>
      select(-"tmp")
  }
  x$cards$tbl_hierarchical <- x_ard_sort |> select(-"idx_sort")

  # update x$table_body
  x$table_body <- x$table_body[idx, ]

  x
}
