# prepares the tidy object to be printed with broom.helpers
tidy_prep <- function(x, tidy_fun, exponentiate, conf.level, intercept, label,
                      show_single_row, include, add_estimate_to_reference_rows,
                      conf.int, ...) {
  # getting the default `tidy_plus_plus()` args
  tidy_plus_plus_args <-
    get_theme_element("tbl_regression-lst:tidy_plus_plus", default = list()) |>
    c(list(add_header_rows = TRUE)) |>
    utils::modifyList(val = rlang::enquos(...))

  # keeping the first arg listed if duplicated (first is the user-specified one)
  tidy_plus_plus_args <-
    tidy_plus_plus_args[names(tidy_plus_plus_args) %>% {!duplicated(.)}] # styler: off

  # tidying up the tidy data frame with `broom.helpers::tidy_plus_plus()`
  df_tidy <-
    rlang::expr(
      broom.helpers::tidy_plus_plus(
        model = x,
        tidy_fun = tidy_fun,
        exponentiate = exponentiate,
        variable_labels = {{ label }},
        show_single_row = {{ show_single_row }},
        intercept = intercept,
        include = {{ include }},
        conf.level = conf.level,
        conf.int = conf.int,
        add_estimate_to_reference_rows = add_estimate_to_reference_rows,
        strict = TRUE,
        !!!tidy_plus_plus_args
      )
    ) |>
    eval_tidy() %>%
    # styler: off
    {dplyr::bind_cols(
      .,
      attributes(.)[names(attributes(.)) %in% c("N_obs", "N_event", "coefficients_type", "coefficients_label")] %>%
        dplyr::as_tibble()
    )}
  # styler: on

  if (!"header_row" %in% names(df_tidy)) {
    df_tidy$header_row <- NA
  }

  df_tidy <- df_tidy |>
    mutate(
      row_type = ifelse(.data$header_row | is.na(.data$header_row), "label", "level")
    )

  # these are old column names, but i prefer to keep the consistently names from broom.helpers
  # adding these back to the data frame for backwards compatibility
  if ("N_obs" %in% names(df_tidy)) df_tidy <- mutate(df_tidy, N = .data$N_obs)
  if ("N_event" %in% names(df_tidy)) df_tidy <- mutate(df_tidy, nevent = .data$N_event)

  df_tidy |>
    select(
      any_of(c(
        "variable", "var_label", "var_type",
        "reference_row", "row_type", "header_row", "N_obs", "N_event", "N",
        "coefficients_type", "coefficients_label", "label"
      )),
      everything()
    )
}


.tbl_regression_default_table_header <- function(x,
                                                 exponentiate,
                                                 tidy_columns_to_report,
                                                 estimate_fun,
                                                 pvalue_fun,
                                                 conf.level) {
  # label ----------------------------------------------------------------------
  x <-
    modify_table_styling(
      x,
      columns = any_of("label"),
      label = paste0("**", translate_string("Characteristic"), "**"),
      hide = FALSE
    )

  # estimate -------------------------------------------------------------------
  estimate_column_labels <- .estimate_column_labels(x)
  x <-
    modify_table_styling(
      x,
      columns = any_of("estimate"),
      label = glue("**{estimate_column_labels$label}**") %>% as.character(),
      hide = !"estimate" %in% tidy_columns_to_report,
      footnote_abbrev = glue("{estimate_column_labels$footnote}") %>% as.character(),
      fmt_fun = estimate_fun
    ) |>
    modify_table_styling(
      columns = any_of("estimate"),
      rows = .data$reference_row == TRUE,
      missing_symbol = get_theme_element("tbl_regression-str:ref_row_text", default = "\U2014")
    )

  # N --------------------------------------------------------------------------
  x <-
    modify_table_styling(
      x,
      columns = any_of("N"),
      label = glue("**{translate_string('N')}**") %>% as.character(),
      fmt_fun = label_style_number()
    ) |>
    modify_table_styling(
      columns = any_of(c("N_obs", "N_event", "n_obs", "n_event")),
      fmt_fun = label_style_number()
    )

  # conf.low -------------------------------------------------------------------
  x <-
    modify_table_styling(
      x,
      columns = any_of("conf.low"),
      label = glue("**{style_number(conf.level*100,)}% {translate_string('CI')}**") |> as.character(),
      hide = !all(c("conf.low", "conf.high") %in% tidy_columns_to_report),
      footnote_abbrev =
        ifelse(
          inherits(x$inputs$x, c("stanreg", "stanfit", "brmsfit", "rjags")),
          translate_string("CI = Credible Interval"),
          translate_string("CI = Confidence Interval")
        )
    ) |>
    modify_table_styling(
      columns = any_of("conf.low"),
      rows = .data$reference_row == TRUE,
      missing_symbol = get_theme_element("tbl_regression-str:ref_row_text", default = "\U2014")
    ) |>
    modify_table_styling(
      columns = any_of("conf.low"),
      rows = !is.na(.data$conf.low),
      cols_merge_pattern =
        paste("{conf.low}", "{conf.high}", sep = get_theme_element("pkgwide-str:ci.sep", default = ", "))
    )

  x <-
    modify_table_styling(x,
      columns = any_of(c("conf.low", "conf.high")),
      fmt_fun = estimate_fun
    )

  # the "ci" column was deprecated in v2.0
  if (all(c("conf.low", "conf.high") %in% names(x$table_body))) {
    x <- x |>
      modify_table_body(
        ~ .x |>
          dplyr::mutate(
            .before = "conf.low",
            ci = dplyr::case_when(
              .data$reference_row == TRUE & is.na(.data$conf.low) ~
                get_theme_element("tbl_regression-str:ref_row_text", default = "\U2014"),
              !is.na(.data$conf.low) ~
                glue::glue("{estimate_fun(conf.low)}{get_theme_element('pkgwide-str:ci.sep', default = ', ')} {estimate_fun(conf.high)}"),
              TRUE ~ NA_character_
            )
          )

      ) %>% # suppress deprecation warning about "ci" column
      {suppressWarnings(
        modify_header(., ci = x$table_styling$header$label[x$table_styling$header$column == "conf.low"]) |>
          modify_column_hide("ci")
      )}
  }

  # p.value --------------------------------------------------------------------
  x <- modify_table_styling(
    x,
    columns = any_of("p.value"),
    label = paste0("**", translate_string("p-value"), "**"),
    fmt_fun = pvalue_fun,
    hide = !"p.value" %in% tidy_columns_to_report
  )

  # std.error ------------------------------------------------------------------
  x <-
    modify_table_styling(
      x,
      columns = any_of("std.error"),
      label = paste0("**", translate_string("SE"), "**"),
      footnote_abbrev = translate_string("SE = Standard Error"),
      fmt_fun = label_style_sigfig(digits = 3),
      hide = !"std.error" %in% tidy_columns_to_report
    ) |>
    modify_table_styling(
      columns = any_of("std.error"),
      rows = .data$reference_row == TRUE,
      missing_symbol = get_theme_element("tbl_regression-str:ref_row_text", default = "\U2014")
    )

  # statistic ------------------------------------------------------------------
  x <-
    modify_table_styling(
      x,
      columns = any_of("statistic"),
      label = paste0("**", translate_string("Statistic"), "**"),
      fmt_fun = label_style_sigfig(digits = 3),
      hide = !"statistic" %in% tidy_columns_to_report
    ) |>
    modify_table_styling(
      columns = any_of("statistic"),
      rows = .data$reference_row == TRUE,
      missing_symbol = get_theme_element("tbl_regression-str:ref_row_text", default = "\U2014")
    )

  # finally adding style_sigfig(x, digits = 3) as default for all other columns
  x <-
    modify_table_styling(
      x,
      columns =
        c(
          where(is.numeric),
          -any_of(c(
            "estimate", "conf.low", "conf.high", "p.value", "std.error", "statistic",
            "N", "N_obs", "N_event", "n_obs", "n_event"
          ))
        ),
      fmt_fun = label_style_sigfig(digits = 3)
    )

  x
}

.estimate_column_labels <- function(x) {
  language <- get_theme_element("pkgwide-str:language", default = "en")


  result <- list()
  result$label <- unique(x$table_body$coefficients_label)

  if (result$label %in% c("Beta", "exp(Beta)")) {
    exponentiate <- x$inputs$exponentiate
    result$label <- get_theme_element("tbl_regression-str:coef_header",
      default = result$label
    )
  }

  result$footnote <-
    dplyr::case_when(
      result$label %in% c("OR", "log(OR)") ~ "OR = Odds Ratio",
      result$label %in% c("HR", "log(HR)") ~ "HR = Hazard Ratio",
      result$label %in% c("RR", "log(RR)") ~ "RR = Relative Risk",
      result$label %in% c("IRR", "log(IRR)") ~ "IRR = Incidence Rate Ratio"
    ) %>%
    translate_string(language) %>%
    {switch(!is.na(.), .)} # styler: off
  result$label <- translate_string(result$label, language)
  result
}
