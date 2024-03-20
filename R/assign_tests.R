assign_tests <- function(x, ...) {
  check_not_missing(x)
  check_class(x, "gtsummary")
  UseMethod("assign_tests")
}

assign_tests.tbl_summary <- function(x, test, group = NULL, include,
                                     calling_fun = c("add_p", "add_difference"),
                                     call = parent.frame()) {
  # processing inputs ----------------------------------------------------------
  calling_fun <- arg_match(calling_fun)
  data <- x$inputs$data
  summary_type <- x$inputs$type

  # loop over the variables and assign default test if not provided by user
  lapply(
    include,
    function(variable) {
      # if there is a user-supplied test, use that one
      if (!is.null(test[[variable]])) return(test[[variable]]) # styler: off

      if (calling_fun %in% "add_p") {
        default_test <-
          add_p_tbl_summary_default_test(data, variable = variable, by = by, summary_type = summary_type[[variable]])
      }

      if (is.null(default_test)) {
        cli::cli_abort(c(
          "There is no default test set for column {.val {variable}}.",
          i = "Set a value in the {.arg test} argument for column {.val {variable}} or exclude it with {.code include = -{variable}}.",
          call = call
        ))
      }
      default_test
    }
  )
}

add_p_tbl_summary_default_test <- function(data, variable, by, summary_type) {
  # for continuous data, default to non-parametric tests
  if (is.null(group) && summary_type %in% c("continuous", "continuous2") && length(unique(data[[by]])) == 2) {
    test_func <-
      get_theme_element("add_p.tbl_summary-attr:test.continuous_by2", default = "wilcox.test")
    return(test_func)
  }
  if (is.null(group) && summary_type %in% c("continuous", "continuous2")) {
    test_func <-
      get_theme_element("add_p.tbl_summary-attr:test.continuous", default = "kruskal.test")
    return(test_func)
  }
  # now assign categorical default tests
  if (is.null(group) && summary_type %in% c("categorical", "dichotomous")) {
    # calculate expected counts to select between chi-square and fisher
    min_exp <-
      tryCatch(
        suppressWarnings(
          table(data[[by]], data[[variable]]) |>
            proportions() %>%
            {expand.grid(rowSums(.), colSums(.))} |> #styler: off
            dplyr::mutate(
              exp = .data$Var1 * .data$Var2 *
                sum(!is.na(data[[variable]]) & !is.na(data[[by]]))
            ) %>%
            dplyr::pull(exp) |>
            min()
        ),
        error = \(e) Inf # if there is an error for whatever reason, return Inf
      )
    # if expected counts are greater than 5, then chisq test
    if (isTRUE(min_exp >= 5 || is.nan(min_exp))) {
      test_func <-
        get_theme_element("add_p.tbl_summary-attr:test.categorical", default = "chisq.test.no.correct")
      return(test_func)
    }
    # otherwise fishers test
    test_func <-
      get_theme_element("add_p.tbl_summary-attr:test.categorical.low_count", default = "fisher.test")
    return(test_func)
  }

  # now setting default tests for grouped data
  # if group variable supplied, fit a random effects model
  if (!is.null(group) && length(unique(data[[by]])) == 2) {
    if (summary_type[[variable]] %in% c("continuous", "continuous2")) {
      test_func <-
        get_theme_element("add_p.tbl_summary-attr:test.continuous.group_by2", default = "lme4")
      return(test_func)
    }
    if (summary_type[[variable]] %in% c("categorical", "dichotomous")) {
      test_func <-
        get_theme_element("add_p.tbl_summary-attr:test.categorical.group_by2", default = "lme4")
      return(test_func)
    }
  }
}
