#' Display univariate regression model results in table
#'
#' @description
#' This function estimates univariate regression models and returns them in
#' a publication-ready table.  It can create univariate regression models holding
#' either a covariate or outcome constant.
#'
#' For models holding outcome constant, the function takes as arguments a data frame,
#' the type of regression model, and the outcome variable `y=`. Each column in the
#' data frame is regressed on the specified outcome. The `tbl_uvregression`
#' function arguments are similar to the [tbl_regression] arguments. Review the
#' \href{https://www.danieldsjoberg.com/gtsummary/articles/tbl_regression.html#tbl_uvregression}{tbl_uvregression vignette}
#' for detailed examples.
#'
#' You may alternatively hold a single covariate constant. For this, pass a data
#' frame, the type of regression model, and a single
#' covariate in the `x=` argument. Each column of the data frame will serve as
#' the outcome in a univariate regression model. Take care using the `x` argument
#' that each of the columns in the data frame are appropriate for the same type
#' of model, e.g. they are all continuous variables appropriate for [lm], or
#' dichotomous variables appropriate for logistic regression with [glm].
#'
#' @inheritSection tbl_regression Methods
#'
#' @param data Data frame to be used in univariate regression modeling.  Data
#' frame includes the outcome variable(s) and the independent variables.
#' Survey design objects are also accepted.
#' @param method Regression method (e.g. [lm], [glm], [survival::coxph],
#' `survey::svyglm`, and more).
#' @param y Model outcome (e.g. `y = recurrence` or `y = Surv(time, recur)`).
#' All other column in `data` will be regressed on `y`.
#' Specify one and only one of `y` or `x`
#' @param x Model covariate (e.g. `x = trt`).
#' All other columns in `data` will serve as the outcome in a regression model
#' with `x` as a covariate.  Output table is best when `x` is a continuous or
#' dichotomous variable displayed on a single row.
#' Specify one and only one of `y` or `x`
#' @param formula String of the model formula.
#' Uses [glue::glue] syntax. Default is `"{y} ~ {x}"`, where `{y}`
#' is the dependent variable, and `{x}` represents a single covariate. For a
#' random intercept model, the formula may be `formula = "{y} ~ {x} + (1 | gear)"`.
#' @param method.args List of additional arguments passed on to the regression
#' function defined by `method`.
#' @param hide_n Hide N column. Default is `FALSE`
#' @inheritParams tbl_regression
#' @author Daniel D. Sjoberg
#' @seealso See tbl_regression \href{https://www.danieldsjoberg.com/gtsummary/articles/tbl_regression.html#tbl_uvregression}{vignette}  for detailed examples
#' @seealso Review [list, formula, and selector syntax][syntax] used throughout gtsummary
#' @family tbl_uvregression tools
#' @export
#' @return A `tbl_uvregression` object
#' @examples
#' \donttest{
#' # Example 1 ----------------------------------
#' tbl_uv_ex1 <-
#'   tbl_uvregression(
#'     trial[c("response", "age", "grade")],
#'     method = glm,
#'     y = response,
#'     method.args = list(family = binomial),
#'     exponentiate = TRUE
#'   )
#'
#' # Example 2 ----------------------------------
#' # rounding pvalues to 2 decimal places
#' library(survival)
#' tbl_uv_ex2 <-
#'   tbl_uvregression(
#'     trial[c("ttdeath", "death", "age", "grade", "response")],
#'     method = coxph,
#'     y = Surv(ttdeath, death),
#'     exponentiate = TRUE,
#'     pvalue_fun = function(x) style_pvalue(x, digits = 2)
#'   )
#' }
#' @section Example Output:
#' \if{html}{Example 1}
#'
#' \if{html}{\out{
#' `r man_create_image_tag(file = "tbl_uv_ex1.png", width = "50")`
#' }}
#'
#' \if{html}{Example 2}
#'
#' \if{html}{\out{
#' `r man_create_image_tag(file = "tbl_uv_ex2.png", width = "50")`
#' }}

tbl_uvregression <- function(data, method, y = NULL, x = NULL, method.args = NULL,
                             exponentiate = FALSE, label = NULL,
                             include = everything(), tidy_fun = NULL,
                             hide_n = FALSE, show_single_row = NULL, conf.level = NULL,
                             estimate_fun = NULL, pvalue_fun = NULL, formula = "{y} ~ {x}",
                             add_estimate_to_reference_rows = NULL, conf.int = NULL, ...) {
  # checking input -------------------------------------------------------------
  # data is a data frame
  if (!is.data.frame(data) && !is_survey(data)) {
    stop("`data` argument must be a data frame or survey object.", call. = FALSE)
  }
  if (missing(method) || !rlang::is_function(method)) {
    cli::cli_abort("Argument {.code method} is required and must be a function.")
  }

  # setting defaults -----------------------------------------------------------
  pvalue_fun <-
    pvalue_fun %||%
    get_theme_element("tbl_regression-arg:pvalue_fun") %||%
    get_theme_element("pkgwide-fn:pvalue_fun") %||%
    .get_deprecated_option("gtsummary.pvalue_fun", default = style_pvalue) %>%
    gts_mapper("tbl_uvregression(pvalue_fun=)")
  estimate_fun <-
    estimate_fun %||%
    .get_deprecated_option(
      "gtsummary.tbl_regression.estimate_fun",
      default = ifelse(exponentiate == TRUE, style_ratio, style_sigfig)
    ) %>%
    gts_mapper("tbl_uvregression(estimate_fun=)")
  conf.int <-
    conf.int %||%
    get_theme_element("tbl_regression-arg:conf.int", default = TRUE)
  conf.level <-
    conf.level %||%
    .get_deprecated_option("gtsummary.conf.level", default = 0.95)

  # bare to string/enexpr ------------------------------------------------------
  # updated method and y inputs to be bare, and converting them to strings
  # to be compatible with the rest of the function that assumes character input
  method <- rlang::enexpr(method)
  method.args <- rlang::enexpr(method.args)

  # converting to string, or keeping as NULL.  Using the standard
  # variable selector, but users may also pass `Surv(ttdeath, death)`,
  # which is not a column header, rather a function.  In that case,
  # converting the bare input to a string.
  x <- rlang::enexpr(x)
  y <- rlang::enexpr(y)
  x <-
    switch(!is.null(x),
      tryCatch(
        {
          .select_to_varnames(
            select = !!x,
            data = .extract_data_frame(data),
            arg_name = "x"
          ) %>%
            rlang::sym()
        },
        error = function(e) x
      ) %>%
        rlang::quo_text()
    )

  y <-
    switch(!is.null(y),
      tryCatch(
        {
          .select_to_varnames(
            select = !!y,
            data = .extract_data_frame(data),
            arg_name = "y"
          ) %>%
            rlang::sym()
        },
        error = function(e) y
      ) %>%
        rlang::quo_text()
    )

  # checking selections of x and y
  if (is.null(x) + is.null(y) != 1L) {
    stop("Specify one, and only one, of `x` and `y`. This function can
         create univariate regression models holding either a covariate or outcome
         constant.", call. = FALSE)
  }
  if ((!is.null(x) && length(x) != 1) | (!is.null(y) && length(y) != 1)) {
    stop("Select only a single column in argument `x=` or `y=`.", call. = FALSE)
  }

  include <-
    .select_to_varnames(
      select = {{ include }},
      data = .extract_data_frame(data),
      arg_name = "include"
    )
  show_single_row <-
    .select_to_varnames(
      select = {{ show_single_row }},
      data = .extract_data_frame(data),
      arg_name = "show_single_row"
    )
  check_haven_labelled(data, include)

  # checking formula correctly specified ---------------------------------------
  if (!rlang::is_string(formula)) {
    stop('`formula` must be passed as a string, e.g. `formula = "{y} ~ {x}"`',
      call. = FALSE
    )
  }
  # checking that '{x}' appears on RHS of formula
  if (word(formula, start = 2L, sep = fixed("~")) %>%
    str_detect(pattern = fixed("{x}")) == FALSE) {
    stop("'{x}' must appear on RHS of '~' in formula argument", call. = FALSE)
  }
  # checking that '{y}' appears on LHS of formula
  if (word(formula, start = 1L, sep = fixed("~")) %>%
    str_detect(pattern = fixed("{y}")) == FALSE) {
    stop("'{y}' must appear on LHS of '~' in formula argument", call. = FALSE)
  }

  # checking estimate_fun and pvalue_fun are functions -------------------------
  if (!is.function(estimate_fun) | !is.function(pvalue_fun)) {
    stop("Arguments 'estimate_fun' and 'pvalue_fun' must be functions.",
      call. = FALSE
    )
  }

  # converting tidyselect formula lists to named lists -------------------------
  label <-
    .formula_list_to_named_list(
      x = label,
      data = .extract_data_frame(data),
      arg_name = "label",
      type_check = chuck(type_check, "is_string", "fn"),
      type_check_msg = chuck(type_check, "is_string", "msg")
    )

  # all specified labels must be a string of length 1
  if (!every(label, ~ rlang::is_string(.x))) {
    stop("Each `label` specified must be a string of length 1.", call. = FALSE)
  }

  # will return call, and all object passed to in table1 call
  # the object func_inputs is a list of every object passed to the function
  tbl_uvregression_inputs <- as.list(environment())
  dots <- rlang::enquos(...)
  tbl_uvregression_inputs <-
    tbl_uvregression_inputs[!names(tbl_uvregression_inputs) %in% c("x_name", "y_name")]


  # get all vars not specified -------------------------------------------------
  all_vars <-
    names(.extract_data_frame(data)) %>%
    # removing x or y variable
    setdiff(paste(c(y, x), "~ 1") %>% stats::as.formula() %>% all.vars()) %>%
    # removing any other variables listed in the formula
    setdiff(all.vars(stats::as.formula(formula), unique = FALSE)) %>%
    # removing {y} and {x}
    setdiff(c("x", "y"))

  if (!is.null(include)) all_vars <- intersect(all_vars, include)
  if (length(all_vars) == 0) {
    stop("There were no covariates selected.", call. = FALSE)
  }

  # building regression models -------------------------------------------------
  tbl_reg_args <-
    c(
      "exponentiate", "conf.level", "label", "include", "show_single_row",
      "tidy_fun", "estimate_fun", "pvalue_fun",
      "add_estimate_to_reference_rows", "conf.int"
    )

  df_model <-
    tibble(
      # quoting the bad names in backticks
      all_vars = all_vars,
      y = switch(!is.null(y),
        rep_len(y, length(all_vars))
      ) %||%
        chr_w_backtick(all_vars),
      x = switch(!is.null(x),
        rep_len(x, length(all_vars))
      ) %||%
        chr_w_backtick(all_vars)
    ) %>%
    # building model
    mutate(
      type = ifelse(!is.null(.env$y), "x_varies", "y_varies"),
      formula_chr = glue(formula),
      model = map(
        .data$formula_chr,
        ~ safe_model_construction(.x, method, data, method.args)
      ),
      # removing backticks
      y = switch(is.null(.env$y),
        all_vars
      ) %||% y,
      x = switch(is.null(.env$x),
        all_vars
      ) %||% x
    ) %>%
    select(all_of(c("y", "x", "type", "model"))) %>%
    # preparing tbl_regression function arguments
    mutate(
      tbl_args = pmap(
        list(.data$model, .data$y, .data$x, .data$type),
        function(model, y, x, type) {
          args <- tbl_uvregression_inputs
          # removing NULL elements from list
          args[sapply(args, is.null)] <- NULL
          args$label <- args$label[names(args$label) %in% x]
          # keeping args to pass to tbl_regression
          args <- args[names(args) %in% tbl_reg_args]

          # fixing show_single_row arg for x_varies
          if (type == "x_varies") {
            args[["show_single_row"]] <- intersect(x, show_single_row)
          }

          # only include the one x var of interest
          args[["include"]] <- x

          if (type == "y_varies") {
            args[["label"]] <- list(label[[y]] %||% attr(data[[y]], "label") %||% y) %>% set_names(x)
          }

          # adding model object
          args[["x"]] <- model
          args
        }
      )
    )

  # creating tbl_regression object
  df_model$tbl <- pmap(
    list(df_model$tbl_args, df_model$type, df_model$y),
    function(tbl_args, type, y) {
      # browser()
      tbl <- call2(tbl_regression, !!!tbl_args, !!!dots) %>% eval()
      if (type == "y_varies") {
        tbl$table_body$variable <- y
        tbl$table_body$var_type <- NA_character_
      }
      tbl
    }
  )

  # stacking results to return -------------------------------------------------
  results <- tbl_stack(df_model$tbl)
  names(results$tbls) <- all_vars
  class(results) <- c("tbl_uvregression", "gtsummary")

  # update column header if `x=` was used --------------------------------------
  if (!is.null(x)) {
    results <- modify_table_styling(results, columns = "label", label = "**Outcome**")
  }

  # creating a meta_data table -------------------------------------------------
  # (this will be used in subsequent functions, eg add_global_p)
  results$meta_data <-
    results$table_body %>%
    filter(.data$row_type == "label") %>%
    select(any_of(c("variable", "var_type", "label", "N_obs", "N_event")))

  # removing modify_stat_* columns ---------------------------------------------
  results$table_styling$header <-
    results$table_styling$header %>%
    select(-starts_with("modify_stat_"))

  # adding column of N ---------------------------------------------------------
  if (hide_n == FALSE) results <- add_n(results, location = "label")

  # exporting results ----------------------------------------------------------
  results$inputs <- tbl_uvregression_inputs
  results$call_list <- list(tbl_uvregression = match.call())

  results
}

# function to safely build and evaluate model, with nicer error messaging
safe_model_construction <- function(formula, method, data, method.args) {
  # defining formula and data call (or formula and design)
  call_list <-
    switch(is.data.frame(data),
      list(method, formula = as.formula(formula), data = data)
    ) %||%
    list(method, formula = as.formula(formula), design = data) %>%
    c(as.list(method.args)[-1])

  # evaluate model
  tryCatch(
    as.call(call_list) %>% eval(),
    error = function(e) {
      # construct call to show in error message
      if (is_survey(data)) {
        call_list$design <- expr(.)
      } else {
        call_list$data <- expr(.)
      }
      call_chr <- call_list %>%
        as.call() %>%
        rlang::expr_text()

      paste(
        "There was an error constructing model {.code {call_chr}}",
        "See error below."
      ) %>%
        cli_alert_danger()
      abort(as.character(e))
    }
  )
}
