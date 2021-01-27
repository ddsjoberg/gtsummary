# prepares the tidy object to be printed with broom.helpers
tidy_prep <- function(x, tidy_fun, exponentiate, conf.level, intercept, label,
                      show_single_row, include, add_estimate_to_reference_rows) {
  # quoting inputs
  label <- rlang::enquo(label)
  show_single_row <- rlang::enquo(show_single_row)
  include <- rlang::enquo(include)

  # getting the default `tidy_plus_plus()` args
  tidy_plus_plus_args <-
    get_theme_element("tbl_regression-lst:tidy_plus_plus", default = list()) %>%
    c(list(
      conf.int = TRUE,
      add_header_rows = TRUE
    ))

  # keeping the first arg listed if duplicated (first is the user-specified one)
  tidy_plus_plus_args <-
    tidy_plus_plus_args[names(tidy_plus_plus_args) %>% {
      !duplicated(.)
    }]

  # tidying up the tidy data frame with `broom.helpers::tidy_plus_plus()`
  df_tidy <-
    rlang::expr(
      broom.helpers::tidy_plus_plus(
        model = !!x,
        tidy_fun = !!tidy_fun,
        exponentiate = !!exponentiate,
        variable_labels = !!label,
        show_single_row = !!show_single_row,
        intercept = !!intercept,
        include = !!include,
        conf.level = !!conf.level,
        add_estimate_to_reference_rows = !!add_estimate_to_reference_rows,
        strict = TRUE,
        !!!tidy_plus_plus_args
      )
    ) %>% rlang::eval_tidy()

  # final tidying before returning ---------------------------------------------
  df_tidy %>%
    mutate(
      N = nrow(gtsummary_model_frame(x)),
      row_type = ifelse(.data$header_row | is.na(.data$header_row), "label", "level")
    ) %>%
    select(
      any_of(c(
        "variable", "var_label", "var_type",
        "reference_row", "row_type", "label", "N"
      )),
      everything()
    )
}

gtsummary_model_frame <- function(x) {
  tryCatch(stats::model.frame(x),
    error = function(e) {
      paste(
        "There was an error calling {usethis::ui_code('stats::model.frame(x)')},",
        "and the model N will not be available in the output."
      ) %>%
        stringr::str_wrap() %>%
        usethis::ui_oops()
      data.frame()
    }
  )
}

.tbl_regression_default_table_header <- function(x, exponentiate,
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
  if ("estimate" %in% names(x$table_body)) {
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
  }

  # N --------------------------------------------------------------------------
  if ("N" %in% names(x$table_body)) {
    x <- modify_table_header(
      x,
      column = "N",
      label = glue("**{translate_text('N')}**") %>% as.character(),
      fmt_fun = style_number
    )
  }

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
      fmt_fun = estimate_fun
    )
  }

  # p.value --------------------------------------------------------------------
  if ("p.value" %in% names(x$table_body)) {
    x <- modify_table_header(
      x,
      column = "p.value",
      label = paste0("**", translate_text("p-value"), "**"),
      fmt_fun = pvalue_fun,
      hide = !"p.value" %in% tidy_columns_to_report
    )
  }

  # std.error ------------------------------------------------------------------
  if ("std.error" %in% names(x$table_body)) {
    x <- modify_table_header(
      x,
      column = "std.error",
      label = paste0("**", translate_text("SE"), "**"),
      footnote_abbrev = translate_text("SE = Standard Error"),
      missing_emdash = "reference_row == TRUE",
      fmt_fun = purrr::partial(style_sigfig, digits = 3),
      hide = !"std.error" %in% tidy_columns_to_report
    )
  }

  # statistic ------------------------------------------------------------------
  if ("statistic" %in% names(x$table_body)) {
    x <- modify_table_header(
      x,
      column = "statistic",
      label = paste0("**", translate_text("Statistic"), "**"),
      fmt_fun = purrr::partial(style_sigfig, digits = 3),
      missing_emdash = "reference_row == TRUE",
      hide = !"statistic" %in% tidy_columns_to_report
    )
  }

  # finally adding style_sigfig(x, digits = 3) as default for all other columns
  for (v in names(x$table_body)) {
    if (
      is.numeric(x$table_body[[v]]) && # is a numeric column
        is.null(x$table_header$fmt_fun[x$table_header$column == v][[1]]) # fmt_fun is empty
    ) {
      x <-
        modify_table_header(
          x,
          column = v,
          fmt_fun = purrr::partial(style_sigfig, digits = 3)
        )
    }
  }

  x
}


chr_w_backtick <- function(x) map_chr(x, ~ rlang::sym(.) %>% deparse(backtick = TRUE))
# > chr_w_backtick("var with spaces")
# [1] "`var with spaces`"
