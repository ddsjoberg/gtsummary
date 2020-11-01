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
    ui_oops(paste0(
      "There was an error calling {ui_code('tidy_fun')}.\n",
      "Most likely, this is because the argument passed in {ui_code('tidy_fun=')} \n",
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
    ui_oops("Review the GitHub issue linked below for a possible solution.")
    ui_code_block("https://github.com/ddsjoberg/gtsummary/issues/231")
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
    broom.helpers::tidy_add_header_rows(df_tidy_5, strict = TRUE,
                                        show_single_row = show_single_row)

  # add reference row value, requested -----------------------------------------
  if (get_theme_element("tbl_regression-lgl:add_ref_est", default = FALSE)) {
    df_tidy_6 <-
      broom.helpers::tidy_add_estimate_to_reference_rows(df_tidy_6, exponentiate = exponentiate)
  }

  # final tidying before returning ---------------------------------------------
  df_tidy_6 %>%
    mutate(
      N = nrow(gtsummary_model_frame(x)),
      row_type = ifelse(.data$header_row | is.na(.data$header_row), "label", "level")
    ) %>%
    select(
      any_of(c("variable", "var_label", "var_type",
               "reference_row", "row_type", "label", "N")),
      everything()
    )
}

gtsummary_model_frame <- function(x) {
  tryCatch(stats::model.frame(x),
  error = function(e) {
    paste("There was an error calling {usethis::ui_code('stats::model.frame(x)')},",
          "and the model N will not be available in the output.") %>%
      stringr::str_wrap() %>%
      usethis::ui_oops()
  }
  )
}

.tbl_reression_default_table_header <- function(x, exponentiate,
                                                tidy_columns_to_report,
                                                estimate_fun,
                                                pvalue_fun,
                                                conf.level) {
  # label ----------------------------------------------------------------------
  x <-
    modify_table_header(
      x,
      column = "label",
      label = paste0("**", translate_text("Characteristic"), "**"),
      hide = FALSE
    )

  # estimate -------------------------------------------------------------------
  if ("estimate" %in% names(x$table_body))
    x <- modify_table_header(
      x,
      column = "estimate",
      label = glue("**{estimate_header(x$model_obj, exponentiate)}**") %>% as.character(),
      hide = !"estimate" %in% tidy_columns_to_report,
      missing_emdash = "reference_row == TRUE",
      footnote_abbrev =
        estimate_header(x$model_obj, exponentiate) %>% attr("footnote") %||% NA_character_,
      fmt_fun = estimate_fun
    )

  # N --------------------------------------------------------------------------
  if ("N" %in% names(x$table_body))
    x <- modify_table_header(
      x,
      column = "N",
      label = glue("**{translate_text('N')}**")  %>% as.character(),
      fmt_fun = style_number
    )

  # ci -------------------------------------------------------------------------
  if (all(c("conf.low", "conf.high") %in% names(x$table_body))) {
    x <- modify_table_header(
      x,
      column = "ci",
      label = glue("**{style_percent(conf.level, symbol = TRUE)} {translate_text('CI')}**") %>% as.character(),
      hide = !all(c("conf.low", "conf.high") %in% tidy_columns_to_report),
      missing_emdash = "reference_row == TRUE",
      footnote_abbrev = translate_text("CI = Confidence Interval")
    )
    x <- modify_table_header(x,
                             column = c("conf.low", "conf.high"),
                             fmt_fun = estimate_fun)
  }

  # p.value --------------------------------------------------------------------
  if ("p.value" %in% names(x$table_body))
    x <- modify_table_header(
      x,
      column = "p.value",
      label = paste0("**", translate_text("p-value"), "**"),
      fmt_fun = pvalue_fun,
      hide = !"p.value" %in% tidy_columns_to_report
    )

  # std.error ------------------------------------------------------------------
  if ("std.error" %in% names(x$table_body))
    x <- modify_table_header(
      x,
      column = "std.error",
      label = paste0("**", translate_text("SE"), "**"),
      footnote_abbrev = translate_text("SE = Standard Error"),
      missing_emdash = "reference_row == TRUE",
      fmt_fun = purrr::partial(style_sigfig, digits = 3),
      hide = !"std.error" %in% tidy_columns_to_report
    )

  # statistic ------------------------------------------------------------------
  if ("statistic" %in% names(x$table_body))
    x <- modify_table_header(
      x,
      column = "statistic",
      label = paste0("**", translate_text("Statistic"), "**"),
      fmt_fun = purrr::partial(style_sigfig, digits = 3),
      missing_emdash = "reference_row == TRUE",
      hide = !"statistic" %in% tidy_columns_to_report
    )

  # finally adding style_sigfig(x, digits = 3) as default for all other columns
  for (v in names(x$table_body)) {
    if (
      is.numeric(x$table_body[[v]]) && # is a numeric column
      is.null(x$table_header$fmt_fun[x$table_header$column == v]) # fmt_fun is empty
    )
      x <-
        modify_table_header(
          x,
          column = v,
          fmt_fun = purrr::partial(style_sigfig, digits = 3)
        )
  }

  x
}
