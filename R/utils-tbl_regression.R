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
                      show_single_row, include) {
  df_tidy <-
    broom.helpers::tidy_plus_plus(
      model = x,
      tidy_fun = tidy_fun,
      exponentiate = exponentiate,
      variable_labels = {{ label }},
      show_single_row = {{ show_single_row }},
      intercept = intercept,
      include = {{ include }},
      conf.level = conf.level,
      conf.int = TRUE,
      add_header_rows =
        get_theme_element("tbl_regression-lgl:add_header_rows", default = TRUE),
      interaction_sep =
        get_theme_element("tbl_regression-str:interaction_sep", default = " * "),
      categorical_terms_pattern =
        get_theme_element("tbl_regression-str:categorical_terms_pattern", default = "{level}"),
      add_reference_rows  =
        get_theme_element("tbl_regression-lgl:add_reference_rows", default = TRUE),
      no_reference_row =
        get_theme_element("tbl_regression:no_reference_row", default = NULL),
      add_estimate_to_reference_rows =
        get_theme_element("tbl_regression-lgl:add_estimate_to_reference_rows", default = FALSE),
      add_header_rows   =
        get_theme_element("tbl_regression-lgl:add_header_rows", default = FALSE),
      strict = TRUE
    )

  # add reference row value, requested -----------------------------------------
  if (get_theme_element("tbl_regression-lgl:add_ref_est", default = FALSE)) {
    df_tidy <-
      broom.helpers::tidy_add_estimate_to_reference_rows(df_tidy, exponentiate = exponentiate)
  }

  # final tidying before returning ---------------------------------------------
  df_tidy %>%
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
    usethis::ui_oops(paste0(
      "There was an error calling {usethis::ui_code('stats::model.frame(x)')}.\n\n",
      "Most likely, this is because the argument passed in {usethis::ui_code('x =')} ",
      "was\nmisspelled, does not exist, or is not a regression model.\n\n",
      "Rarely, this error may occur if the model object was created within\na ",
      "functional programming framework (e.g. using {usethis::ui_code('lappy()')}, ",
      "{usethis::ui_code('purrr::map()')}, etc.).\n",
      "Review the GitHub issue linked below for a possible solution.\n",
      "The model N will not be available in the output."
    ))
    usethis::ui_code_block("https://github.com/ddsjoberg/gtsummary/issues/231")
    data.frame()
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
