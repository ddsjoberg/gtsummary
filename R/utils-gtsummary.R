
.extract_glue_elements <- function(x) {
  regmatches(x, gregexpr("\\{([^\\}]*)\\}", x)) |>
    unlist() %>%
    {substr(., 2, nchar(.) - 1)}
}

.ifelse1 <- function(test, yes, no) {
  if (test) {
    return(yes)
  }
  no
}

.get_selecting_data_frame <- function(x) {
  # if a tbl_summary-like table, grab data frame from inputs
  if (tryCatch(x$calls[[1]]$data |> is.data.frame(), error = function(x) FALSE)) {
    return(x$calls[[1]]$dataa)
  }

  # TODO: add tbl_regression results often return the type of variable input, we could use that to construct a df with the approriate class?

  # if none of the above apply, return a data frame based on the variables listed
  if ("variable" %in% names(x$table_body)) {
    return(
        dplyr::tibble(!!!rlang::rep_named(unique(x$table_body$variable), character()))
    )
  }

  # lastly, we can get a variable list from the card data frame
  dplyr::tibble(!!!rlang::rep_named(unique(x$cards$variable), character()))
}
