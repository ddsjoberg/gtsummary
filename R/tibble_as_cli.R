#' Print tibble with cli
#'
#' Print a tibble or data frame using cli styling and formatting.
#'
#' @param x (`data.frame`)\cr
#'   a data frame with all character columns.
#' @param na_value (`string`)\cr
#'   a string indicating how an `NA` value will appear in printed table.
#' @param label (named `list`)\cr
#'   named list of column labels to use. Default is to print the column names.
#' @param padding (`integer`)\cr
#'   an integer indicating the amount of padding between columns.
#'
#' @return NULL
#' @keywords internal
#'
#' @examples
#' trial[1:3, ] |>
#'   dplyr::mutate_all(as.character) |>
#'   gtsummary:::tibble_as_cli()
tibble_as_cli <- function(x, na_value = "", label = list(), padding = 3L) {
  # check the input is a data frame --------------------------------------------
  check_data_frame(x)
  check_string(na_value)
  check_integerish(padding)
  check_class(label, cls = "list")
  if (!is_empty(label) && !is_named(label)) {
    cli::cli_abort("Argument {.arg label} must be a named list.")
  }

  # check all labels are strings
  imap(
    label,
    function(lbl, name) {
      if (!is_string(lbl)) cli::cli_abort("Each element of the {.arg label} argument list must be a string, and element {.val {name}} is {.obj_type_friendly {lbl}}.")
    }
  )

  # check all columns are character
  walk(
    names(x),
    function(varname) {
      if (!is.character(x[[varname]])) {
        cli::cli_abort("All columns must be class {.cls character}, and column {.val {varname}} is {.obj_type_friendly {x[[varname]]}}.")
      }
    }
  )

  # convert any NA cells to character na_value ---------------------------------
  x <- dplyr::mutate_all(x, ~ ifelse(is.na(.x), .env$na_value, .x))

  # update 'label' with colnames if not supplied -------------------------------
  label <- as.list(names(x)) |>
    stats::setNames(names(x)) |>
    utils::modifyList(val = label)

  # add a header row as the first row of the data frame ------------------------
  x <- dplyr::add_row(x, !!!label, .before = 1)

  # save the max width of each column ------------------------------------------
  lst_max_nchar <- map(x, ~ as.character(.) |>
    nchar() |>
    max(na.rm = TRUE))


  # add padding to all value in x so they are the same length ------------------
  x <- x |>
    imap(~ str_pad(.x, side = "right", width = lst_max_nchar[[.y]] + padding)) |>
    dplyr::bind_cols()

  # italicizing header row (that is, the first row of the data frame) ----------
  x <- x |>
    dplyr::mutate_all(
      ~ ifelse(dplyr::row_number() == 1L, cli::style_underline(.) |> cli::style_italic(), .)
    )

  # italicize data type string
  x <- x |>
    mutate(across(everything(), ~ str_replace_all(.x, "(<[^>]+>)", cli::style_italic("\\1"))))

  # print header strings blue
  x <- x |>
    dplyr::mutate(
      label = ifelse(dplyr::row_number() != 1L, cli::col_blue(label), label)
    )

  # print dynamic column strings grey
  x <- x |>
    dplyr::mutate(
      # Apply grey color to all columns after the second one
      across(-1:-2, ~ ifelse(dplyr::row_number() != 1L, cli::col_grey(.), .))
    )

  # print the data frame -------------------------------------------------------
  walk(
    seq_len(nrow(x)),
    function(.x) {
      x[.x, ] |>
        unlist() |>
        paste(collapse = "") |>
        cat("\n")
    }
  )

  invisible(x)
}
