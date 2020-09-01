#' Takes a vector and transforms to data frame with those column names
#'
#' This will be used for tidyselect to used those functions to select from
#' the vector
#' @noRd
#' @keywords internal
vctr_2_tibble <- function(x) {
  n <- length(x)
  df <- matrix(ncol = n) %>%
    tibble::as_tibble(.name_repair = "minimal")

  names(df) <- x
  df[0, , drop = FALSE]
}

# prepares the tidy object to be printed with broom.helpers
tidy_prep <- function(x, tidy_fun, exponentiate, conf.level, intercept, label,
                      show_single_row) {
  # run initial tidy -----------------------------------------------------------
  df_tidy_1 <- tryCatch({
    tidy_fun(x, exponentiate = exponentiate, conf.level = conf.level, conf.int = TRUE)
  },
  error = function(e) {
    usethis::ui_oops(paste0(
      "There was an error calling {usethis::ui_code('tidy_fun')}.\n",
      "Most likely, this is because the argument passed in {usethis::ui_code('tidy_fun=')} \n",
      "was misspelled, does not exist, is not compatible with your object, \n",
      "or was missing necessary arguments. See error message below. \n"
    ))
    stop(as.character(e), call. = FALSE)
  })

  # checking reserved names in the tidy data frame
  if (any(c("rowname", "groupname_col") %in% names(df_tidy_1))) {
    paste("The resulting tibble from the initial tidying of the model",
          "(likely from the tidier passed in `tidy_fun=`)",
          "contains a column named 'rowname' or 'groupname_col'.",
          "These column names result in special print behavior in the",
          "{gt} package, and may cause errors or malformed tables.") %>%
      stringr::str_wrap() %>%
      rlang::inform()
  }

  # attach model object to tidy tibble -----------------------------------------
  df_tidy_2 <- broom.helpers::tidy_attach_model(x = df_tidy_1, model = x)

  # remove intercept from output -----------------------------------------------
  if (!intercept) df_tidy_2 <- broom.helpers::tidy_remove_intercept(df_tidy_2)

  # identify variables in model ------------------------------------------------
  df_tidy_3 <- broom.helpers::tidy_identify_variables(df_tidy_2) %>%
    # if intercept remains filling in the variable name withe intercept
    dplyr::mutate(variable = dplyr::coalesce(.data$variable, .data$term))
  if (all(is.na(df_tidy_3$variable))) { # when variables are all missing, print this
    usethis::ui_oops("Review the GitHub issue linked below for a possible solution.")
    usethis::ui_code_block("https://github.com/ddsjoberg/gtsummary/issues/231")
  }

  # creating label and show_single_row named lists -----------------------------
  label <- unique(df_tidy_3$variable) %>% vctr_2_tibble() %>%
    tidyselect_to_list(x = {{ label }}, arg_name = "label")
  show_single_row <- unique(df_tidy_3$variable) %>% vctr_2_tibble() %>%
    var_input_to_string(arg_name = "show_single_row", select_input = {{show_single_row}})

  # add reference row ----------------------------------------------------------
  df_tidy_4 <- broom.helpers::tidy_add_reference_rows(df_tidy_3)

  # add header rows to categorical variables
  df_tidy_5 <-
    broom.helpers::tidy_add_variable_labels(df_tidy_4, labels = label)

  # add header rows to categorical variables -----------------------------------
  df_tidy_6 <-
    broom.helpers::tidy_add_header_rows(df_tidy_5, show_single_row = show_single_row)

  # final tidying before returning ---------------------------------------------
  df_tidy_6 %>%
    mutate(
      N = nrow(gtsummary_model_frame(x)),
      row_type = ifelse(header_row | is.na(header_row), "label", "level")
    ) %>%
    dplyr::select(
      dplyr::any_of(c("variable", "var_label", "var_type", "reference_row",
                      "row_type", "label", "N", "estimate", "std.error", "statistic",
                      "conf.low", "conf.high", "p.value"))
    ) %>%
    dplyr::rename(row_ref = .data$reference_row )
}

gtsummary_model_frame <- function(x) {
  tryCatch(stats::model.frame(x),
  error = function(e) {
    usethis::ui_oops(paste0(
      "There was an error calling {usethis::ui_code('stats::model.frame(x)')}.\n\n",
      "Most likely, this is because the argument passed in {usethis::ui_code('x =')} ",
      "was\nmisspelled, does not exist, or is not a regression model.\n\n",
      "Rarely, this error may occur if the model object was created within\na ",
      "functional programming framework (e.g. using {usethis::ui_code('lappy()')}, ",
      "{usethis::ui_code('purrr::map()')}, etc.). The model N will not be available.\n",
      "Review the GitHub issue linked below for a possible solution."
    ))
    usethis::ui_code_block("https://github.com/ddsjoberg/gtsummary/issues/231")
    stop(as.character(e), call. = FALSE)
  }
  )
}
