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
    tidy_plus_plus_args[names(tidy_plus_plus_args) %>% {!duplicated(.)}]

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
    ) %>%
    rlang::eval_tidy() %>%
    {dplyr::bind_cols(
      .,
      attributes(.)[names(attributes(.)) %in% c("N_obs", "N_event", "coefficients_type", "coefficients_label")] %>%
        tibble::as_tibble()
    )} %>%
    mutate(
      row_type = ifelse(.data$header_row | is.na(.data$header_row), "label", "level")
    )

  # these are old column names, but i prefer to keep the consistently names from broom.helpers
  # adding these back to the data frame for backwards compatibility
  if ("N_obs" %in% names(df_tidy)) df_tidy <- mutate(df_tidy, N = .data$N_obs)
  if ("N_event" %in% names(df_tidy)) df_tidy <- mutate(df_tidy, nevent = .data$N_event)

  df_tidy %>%
    select(
      any_of(c("variable", "var_label", "var_type",
               "reference_row", "row_type", "header_row", "N_obs", "N_event", "N",
               "coefficients_type", "coefficients_label", "label")),
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
    modify_table_styling(
      x,
      columns = "label",
      rows = .data$row_type != 'label',
      label = paste0("**", translate_text("Characteristic"), "**"),
      hide = FALSE,
      text_format = "indent"
    )

  # estimate -------------------------------------------------------------------
  estimate_column_labels <- .estimate_column_labels(x)
  if ("estimate" %in% names(x$table_body))
    x <-
      modify_table_styling(
        x,
        columns = "estimate",
        label = glue("**{estimate_column_labels$label}**") %>% as.character(),
        hide = !"estimate" %in% tidy_columns_to_report,
        footnote_abbrev = glue("{estimate_column_labels$footnote}") %>% as.character(),
        fmt_fun = estimate_fun
      ) %>%
      modify_table_styling(
        columns = "estimate",
        rows = .data$reference_row == TRUE,
        missing_symbol = get_theme_element("tbl_regression-str:ref_row_text", default = "\U2014")
      )

  # N --------------------------------------------------------------------------
  if ("N" %in% names(x$table_body))
    x <-
      modify_table_styling(
        x,
        columns = "N",
        label = glue("**{translate_text('N')}**")  %>% as.character(),
        fmt_fun = style_number
      )

  # ci -------------------------------------------------------------------------
  if (all(c("conf.low", "conf.high") %in% names(x$table_body))) {
    x <-
      modify_table_styling(
        x,
        columns = "ci",
        label = glue("**{style_percent(conf.level, symbol = TRUE)} {translate_text('CI')}**") %>% as.character(),
        hide = !all(c("conf.low", "conf.high") %in% tidy_columns_to_report),
        footnote_abbrev = translate_text("CI = Confidence Interval")
      ) %>%
      modify_table_styling(
        columns = "ci",
        rows = .data$reference_row == TRUE,
        missing_symbol = get_theme_element("tbl_regression-str:ref_row_text", default = "\U2014")
      )

    x <-
      modify_table_styling(x,
                           columns = c("conf.low", "conf.high"),
                           fmt_fun = estimate_fun)
  }

  # p.value --------------------------------------------------------------------
  if ("p.value" %in% names(x$table_body))
    x <- modify_table_styling(
      x,
      columns = "p.value",
      label = paste0("**", translate_text("p-value"), "**"),
      fmt_fun = pvalue_fun,
      hide = !"p.value" %in% tidy_columns_to_report
    )

  # std.error ------------------------------------------------------------------
  if ("std.error" %in% names(x$table_body))
    x <-
      modify_table_styling(
        x,
        columns = "std.error",
        label = paste0("**", translate_text("SE"), "**"),
        footnote_abbrev = translate_text("SE = Standard Error"),
        fmt_fun = purrr::partial(style_sigfig, digits = 3),
        hide = !"std.error" %in% tidy_columns_to_report
      ) %>%
      modify_table_styling(
        columns = "std.error",
        rows = .data$reference_row == TRUE,
        missing_symbol = get_theme_element("tbl_regression-str:ref_row_text", default = "\U2014")
      )

  # statistic ------------------------------------------------------------------
  if ("statistic" %in% names(x$table_body))
    x <-
      modify_table_styling(
        x,
        columns = "statistic",
        label = paste0("**", translate_text("Statistic"), "**"),
        fmt_fun = purrr::partial(style_sigfig, digits = 3),
        hide = !"statistic" %in% tidy_columns_to_report
      ) %>%
      modify_table_styling(
        columns = "statistic",
        rows = .data$reference_row == TRUE,
        missing_symbol = get_theme_element("tbl_regression-str:ref_row_text", default = "\U2014")
      )

  # finally adding style_sigfig(x, digits = 3) as default for all other columns
  x <-
    modify_table_styling(
      x,
      columns =
        vars(where(is.numeric), -any_of(c("estimate", "conf.low", "conf.high", "p.value", "std.error", "statistic"))),
      fmt_fun = purrr::partial(style_sigfig, digits = 3)
    )

  x
}


chr_w_backtick <- function(x) map_chr(x, ~rlang::sym(.) %>% deparse(backtick = TRUE))
# > chr_w_backtick("var with spaces")
# [1] "`var with spaces`"

.estimate_column_labels <- function(x) {
  language <- get_theme_element("pkgwide-str:language", default = "en")

  result <- list()
  result$label <- unique(x$table_body$coefficients_label) %>% translate_text(language)
  result$footnote <-
    case_when(
      result$label %in% c("OR", "log(OR)") ~ "OR = Odds Ratio",
      result$label %in% c("HR", "log(HR)") ~ "HR = Hazard Ratio",
      result$label %in% c("IRR", "log(IRR)") ~ "IRR = Incidence Rate Ratio"
    ) %>%
    translate_text(language) %>%
    {switch(!is.na(.), .)}
  result
}
