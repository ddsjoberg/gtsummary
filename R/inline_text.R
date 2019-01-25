#' Report statistics from `fmt_table1` and `fmt_regression` inline in an Rmarkdown document
#'
#' @param x `fmt_table1` or `fmt_regression` object
#' @param ... further arguments passed to or from other methods.
#' @seealso \code{\link{inline_text.fmt_table1}}, \code{\link{inline_text.fmt_regression}}
#' @export
inline_text <- function(x, ...) UseMethod("inline_text")

#' Report statistics from `fmt_table1` inline in an Rmarkdown document
#'
#' @param x object of class `fmt_table1` object from \code{\link{fmt_table1}} function
#' @param cell identifier for which cell to return from the `fmt_table1` object
#' @param sep a character string to separate the terms (e.g. variable name, )
#' @param pvalue logical indicator to return p-value.  Default is `FALSE`
#' @param p_pvalue logical indicator to return p-value with prepended p
#' (e.g. p=0.3 and p<0.001).  Default is `FALSE`
#' @param overall logical indicator to return overall summary
#' statistic.  Default is `FALSE`.  This only applies when the `fmt_table1` object contains
#' both summary statistics by some variable AND overall summary statistics.
#' @param ... further arguments passed from generic `inline_text()`
#' @return Formatted descriptive statistics from a \code{fmt_table1} object
#' @examples
#' t1 <- fmt_table1(mtcars)
#' t2 <- fmt_table1(mtcars, by = "am")
#' t3 <- fmt_table1(mtcars, by = "am") %>% add_overall()
#' 
#' inline_text(t1, "mpg") # mpg
#' inline_text(t1, "cyl:4") # cyl=4
#' inline_text(t2, "mpg:1") # mpg with am=1
#' inline_text(t2, "cyl:4:1") # cyl=4 with am=1
#' inline_text(t3, "mpg", overall = TRUE) # overall mpg summary
#' @export
inline_text.fmt_table1 <- function(x, cell, sep = ":", pvalue = FALSE,
                                   p_pvalue = FALSE, overall = FALSE, ...) {
  # CHECK, no variable names can contain the sep string
  if (stringr::str_detect(x$meta_data$.variable, sep) %>% sum() > 0) {
    stop(glue::glue(
      "A variable name in 'x' contains '{sep}'. Alter 'sep = {sep}' to a string ",
      "that does not appear in any variable names."
    ))
  }

  # counting number of inputs in cell
  word_n <- stringr::str_count(cell, pattern = stringr::fixed(sep)) + 1

  # extract variable name (always first)
  var <- stringr::word(cell, 1, sep = stringr::fixed(sep))
  # CHECK! a variable selected that is in fmt_table1 object
  if (!(var %in% x$table1$.variable)) {
    stop(glue::glue(
      "'{var}' is not a variable found in x. ",
      "Select one of {x$meta_data$.variable %>% paste(collapse = ', ')}"
    ))
  }

  # returning p-value if pvalue==TRUE
  if (pvalue == TRUE) {
    # requested pvlaue without first add_comparison
    if (x$call_list$add_comparison %>% is.null()) {
      stop("Cannot print p-value because it does not exist.  Run add_comparison() first.")
    }
    return(
      x$meta_data %>%
        dplyr::filter_(~ .variable == var) %>%
        dplyr::pull(pvalue)
    )
  }
  if (p_pvalue == TRUE) {
    # requested pvlaue without first add_comparison
    if (x$call_list$add_comparison %>% is.null()) {
      stop("Cannot print p-value because it does not exist.  Run add_comparison() first.")
    }
    p <- x$meta_data %>%
      dplyr::filter_(~ .variable == var) %>%
      dplyr::pull(pvalue)
    return(
      dplyr::case_when(
        is.na(p) ~ NA_character_,
        stringr::str_sub(p, end = 1L) %in% c("<", ">") ~ paste0("p", p),
        TRUE ~ paste0("p=", p)
      )
    )
  }

  # extract summary type from x
  summary_type <- x$meta_data %>%
    dplyr::filter_(~ .variable == var) %>%
    dplyr::pull(".summary_type")

  # if categorical, extract level to display (always second)
  if (summary_type == "categorical") {
    if (x$table1 %>%
      dplyr::filter_(~ .variable == var) %>%
      dplyr::slice(-1) %>%
      dplyr::pull("label") %>%
      stringr::str_detect(sep) %>%
      sum() > 0) {
      stop(glue::glue(
        "A level of variable '{var}' contains '{sep}'. Change 'sep = {sep}' to a string ",
        "that does not appear in levels of '{var}', for example sep = '::'."
      ))
    }


    level <- stringr::word(cell, 2, sep = stringr::fixed(sep))
    # CHECK!
    lvls_in_x <- x$table1 %>%
      dplyr::filter_(~ .variable == var) %>%
      dplyr::slice(-1) %>%
      dplyr::pull("label")
    if (!(level %in% lvls_in_x)) {
      stop(glue::glue(
        "'{level}' is not a level of variable '{var}'. ",
        "Select one of {paste(lvls_in_x, collapse = ', ')}"
      ))
    }
  }

  # if there is a by variable, extract column name (always last)
  if (!is.null(x$by) & overall == FALSE) {
    # getting a character version of by var, and the numeric ID
    by_dta <- get_by_info(x$inputs$data, x$by)

    # CHECK, no levels of the by variable contain the sep character (default ":")
    if (by_dta$by_chr %>% stringr::str_detect(sep) %>% sum() > 0) {
      stop(glue::glue(
        "A level of by variable '{x$by}' contains '{sep}'. Alter 'sep = {sep}' to a string ",
        "that does not appear in any levels of '{x$by}'."
      ))
    }

    by_val <- stringr::word(cell, -1, sep = stringr::fixed(sep))
    if (!(by_val %in% by_dta$by_chr)) {
      stop(glue::glue(
        "'{col}' is not a level of the by variable '{x$by}'. ",
        "Select one of {paste(by_lvls, collapse = ', ')}"
      ))
    }
  }

  # filtering on the selected variable
  results <- x$table1 %>% dplyr::filter_(~ .variable == var & row_type != "missing")

  # if categorical grabbing appropriate level
  if (summary_type == "categorical") {
    results <- results %>% dplyr::filter_(~ label == level)
  }

  # if no by variable, grabbing stat column, otherwise grabbing by var column
  if (is.null(x$by) | overall == TRUE) {
    results <- results %>% dplyr::pull("stat_overall")
  } else {
    col_name <- by_dta %>%
      dplyr::filter_(~ by_chr == by_val) %>%
      dplyr::pull("by_col")
    results <- results %>% dplyr::pull(col_name)
  }

  return(results)
}


#' Report statistics from `fmt_regression` and `fmt_uni_regression` inline in an Rmarkdown document
#'
#' @param x object of class `fmt_regression` or `fmt_uni_regression` object
#' @param cell identifier for which cell to return from the \code{fmt_regression} object
#' @param stat Statistic to report. User can access the primary estimate (`est`), the lower
#' and upper limit of the confidence interval (`ll` and `ul`), confidence interval (`ci`),
#' N (`N`), pvalue (`pvalue`), and pvalue with `p=` or `p<` appended (`p_pvalue`).
#' Default is \code{"{est} (95\% CI {ci}; {p_pvalue})"}
#' @param sep a character string to separate the terms
#' @param ... further arguments passed from generic `inline_text()`
#' @return Formatted descriptive statistics from a `fmt_regression` object
#' @export
#' @examples
#' mod <- glm(response ~ age + grade + stage, trial, family = binomial(link = "logit")) %>%
#'   fmt_regression(exponentiate = TRUE)
#' inline_text(mod, "age")
#' 
#' inline_text(mod, "grade:III")
#' 
#' trial %>%
#'   fmt_uni_regression(
#'     method = "lm",
#'     y = "age"
#'   ) %>%
#'   inline_text("grade:II")
inline_text.fmt_regression <- function(x, cell, stat = "{est} (95% CI {ci}; {p_pvalue})", sep = ":", ...) {
  # CHECK, no variable names can contain the sep string
  if (stringr::str_detect(x$model_tbl$variable, sep) %>% sum(na.rm = T) > 0) {
    stop(glue::glue(
      "A variable name in 'x' contains '{sep}'. Alter 'sep = {sep}' to a string ",
      "that does not appear in any variable names."
    ))
  }

  # counting number of inputs in cell
  word_n <- stringr::str_count(cell, pattern = stringr::fixed(sep)) + 1

  # extract variable name (always first)
  var <- stringr::word(cell, 1, sep = stringr::fixed(sep))
  # CHECK! a variable selected that is in model object
  if (!(var %in% x$model_tbl$variable)) {
    stop(glue::glue(
      "'{var}' is not a variable found in x. ",
      "Select one of {x$model_tbl$variable %>% paste(collapse = ', ')}"
    ))
  }

  # extract summary type from x
  summary_type <- x$model_tbl %>%
    dplyr::filter_(~ variable == var) %>%
    dplyr::pull("var_type") %>%
    unique()

  # if categorical, extract level to display (always second)
  if (summary_type == "categorical") {
    if (x$model_tbl %>%
      dplyr::filter_(~ variable == var) %>%
      dplyr::slice(-1) %>%
      dplyr::pull("label") %>%
      stringr::str_detect(sep) %>%
      sum() > 0) {
      stop(glue::glue(
        "A level of variable '{var}' contains '{sep}'. Change 'sep = {sep}' to a string ",
        "that does not appear in levels of '{var}', for example sep = '::'."
      ))
    }


    level <- stringr::word(cell, 2, sep = stringr::fixed(sep))
    # CHECK!
    lvls_in_x <- x$model_tbl %>%
      dplyr::filter_(~ variable == var) %>%
      dplyr::slice(-1) %>%
      dplyr::pull("label")
    if (!(level %in% lvls_in_x)) {
      stop(glue::glue(
        "'{level}' is not a level of variable '{var}'. ",
        "Select one of {paste(lvls_in_x, collapse = ', ')}"
      ))
    }
  }


  # filtering on the selected variable
  results <- x$model_tbl %>% dplyr::filter_(~ variable == var)

  # if categorical grabbing appropriate level
  if (summary_type == "categorical") {
    results <- results %>% dplyr::filter_(~ label == level)
  }

  # calculating statistic to be returned
  results <-
    results %>%
    dplyr::mutate_(
      stat_return = ~ as.character(glue::glue(stat))
    ) %>%
    dplyr::pull("stat_return")


  return(results)
}

#' @rdname inline_text.fmt_regression
#' @export
inline_text.fmt_uni_regression <- inline_text.fmt_regression
