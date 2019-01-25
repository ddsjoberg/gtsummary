#' Adds the global p-value for a categorical variables
#'
#' This function uses \code{\link[car]{Anova}} from the `car` package with `type = "III"` to calculate global p-values.
#' If a needed class of model is not supported by `car::`\code{\link[car]{Anova}}, please put in an
#' \href{https://github.mskcc.org/datadojo/biostatR/issues}{issue} to request support.
#' Output from `fmt_regression` and `fmt_uni_regression` objects supported.
#'
#' @param x `fmt_regression` or `fmt_uni_regression` object
#' @param ... further arguments passed to or from other methods.
#' @seealso \code{\link{add_global.fmt_regression}}, \code{\link{add_global.fmt_uni_regression}}
#' @export
add_global <- function(x, ...) UseMethod("add_global")


#' Adds the global p-value for a categorical variables in `fmt_regression` objects
#'
#' This function uses \code{\link[car]{Anova}} from the `car` package with `type = "III"` to calculate global p-values.
#' If a needed class of model is not supported by \code{\link[car]{Anova}}, please put in an
#' issue at https://github.mskcc.org/datadojo/biostatR/issues to request support.
#'
#' @param x object with class `fmt_regression` from the \code{\link{fmt_regression}} function
#' @param terms Character vector of terms for which to add global p-values.  Default
#' is `NULL` which will add global p-values for all categorical variables
#' @param keep logical argument whether to keep the individual p-values for the
#' levels of the categorical variable. Default is `FALSE`
#' @param ... arguments to be passed to \code{\link[car]{Anova}}.  Adding `test.statistic = `
#' can change the type of test (e.g. Likelihood-ratio, Wald, etc.).
#' @examples
#' lm(marker ~ stage + grade, trial) %>% fmt_regression() %>% add_global()
#' @export

add_global.fmt_regression <- function(x, terms = NULL, keep = FALSE, ...) {

  # fetching categorical variables from model
  model_terms <- x %>%
    purrr::pluck("model_tbl") %>%
    dplyr::select(dplyr::one_of(c("var_type", "variable"))) %>%
    dplyr::filter_(~ var_type == "categorical") %>%
    dplyr::distinct() %>%
    dplyr::pull("variable")

  # if not terms supplied, getting list of all categorical terms in model
  if (is.null(terms)) terms <- model_terms

  # if no terms are provided, stop and return x
  if (length(terms) == 0) {
    message("No terms were selected, and no global p-values added to table")
    return(x)
  }

  # check that terms selected appear in model.
  if (!all(terms %in% model_terms)) {
    stop(glue::glue(
      "Terms selected are not categorical terms from model: ",
      "{paste(terms[!(terms %in% model_terms)], collpase = ', ')}"
    ))
  }

  # calculating global pvalues
  global_p <-
    x$model_obj %>%
    car::Anova(type = "III", ...) %>%
    # stats::drop1(test = test) %>% # this function only supports lm and glm
    as.data.frame() %>%
    tibble::rownames_to_column(var = "variable") %>%
    dplyr::filter_(~ variable %in% terms) %>%
    dplyr::select(c("variable", dplyr::starts_with("Pr(>"))) %>% # selecting the pvalue column
    purrr::set_names(c("variable", "global_pvalue_exact"))

  global_p <- global_p %>%
    dplyr::mutate_(
      row_type = ~"label",
      global_pvalue = ~ x$inputs$pvalue_fun(global_pvalue_exact),
      global_p_pvalue = ~ dplyr::case_when(
        is.na(global_pvalue) ~ NA_character_,
        stringr::str_sub(global_pvalue, end = 1L) %in% c("<", ">") ~ paste0("p", global_pvalue),
        TRUE ~ paste0("p=", global_pvalue)
      )
    ) %>%
    dplyr::select(c("row_type", "variable", dplyr::starts_with("global_")))

  # merging in global pvalue
  x$model_tbl <-
    dplyr::left_join(
      x$model_tbl,
      global_p,
      by = c("row_type", "variable")
    ) %>%
    dplyr::mutate_(
      pvalue_exact = ~ dplyr::coalesce(global_pvalue_exact, pvalue_exact),
      pvalue = ~ dplyr::coalesce(global_pvalue, pvalue),
      p_pvalue = ~ dplyr::coalesce(global_p_pvalue, p_pvalue)
    ) %>%
    dplyr::select(-dplyr::starts_with("global_"))

  # if keep == FALSE, then deleting variable-level p-values
  if (keep == FALSE) {
    x$model_tbl <-
      x$model_tbl %>%
      dplyr::left_join(global_p %>% dplyr::select(-dplyr::one_of("row_type")),
        by = "variable"
      ) %>%
      dplyr::mutate_(
        pvalue_exact = ~ ifelse(row_type == "level" & !is.na(global_pvalue), NA, pvalue_exact),
        pvalue = ~ ifelse(row_type == "level" & !is.na(global_pvalue), NA, pvalue),
        p_pvalue = ~ ifelse(row_type == "level" & !is.na(global_pvalue), NA, p_pvalue)
      ) %>%
      dplyr::select(-dplyr::one_of("global_pvalue"))
  }

  return(x)
}

#' Adds the global p-value for a categorical variables in `fmt_uni_regression` objects
#'
#' This function uses \code{\link[car]{Anova}} from the `car` package with `type = "III"` to calculate global p-values.
#' If a needed class of model is not supported by \code{\link[car]{Anova}}, please put in an
#' issue at https://github.mskcc.org/datadojo/biostatR/issues to request support.
#'
#' @param x object with class `fmt_uni_regression` from the \code{\link{fmt_uni_regression}} function
#' @param ... arguments to be passed to \code{\link[car]{Anova}}.  Adding `test.statistic = `
#' can change the type of test (e.g. Likelihood-ratio, Wald, etc.).
#' @examples
#' fmt_uni_regression(
#'   trial,
#'   method = "glm",
#'   y = "response",
#'   method.args = list(family = binomial),
#'   exponentiate = TRUE
#' ) %>%
#'   add_global()
#' @export

add_global.fmt_uni_regression <- function(x, ...) {

  # calculating global pvalues
  global_p <-
    purrr::map2_dfr(
      x$model_obj, names(x$model_obj),
      ~ car::Anova(.x, type = "III") %>%
        as.data.frame() %>%
        tibble::rownames_to_column(var = "variable") %>%
        dplyr::filter_(~ variable == .y) %>%
        dplyr::select(c("variable", dplyr::starts_with("Pr(>"))) %>% # selecting the pvalue column
        purrr::set_names(c("variable", "global_pvalue_exact"))
    ) %>%
    dplyr::mutate_(
      global_pvalue = ~ x$inputs$pvalue_fun(global_pvalue_exact),
      global_p_pvalue = ~ dplyr::case_when(
        is.na(global_pvalue) ~ NA_character_,
        stringr::str_sub(global_pvalue, end = 1L) %in% c("<", ">") ~ paste0("p", global_pvalue),
        TRUE ~ paste0("p=", global_pvalue)
      )
    ) %>%
    dplyr::select(c("variable", dplyr::starts_with("global_")))

  # adding global p-value to meta_data object
  x$meta_data <-
    x$meta_data %>%
    dplyr::left_join(
      global_p,
      by = "variable"
    )

  # making tbl to merge with model_tbl
  global_p_merge <-
    dplyr::data_frame(row_type = "header1", pvalue = "p-value") %>%
    dplyr::bind_rows(
      global_p %>%
        dplyr::select(c("variable", "global_pvalue")) %>%
        purrr::set_names(c("variable", "pvalue")) %>%
        dplyr::mutate_(
          row_type = ~"label"
        )
    )

  # merging in global pvalue
  x$model_tbl <-
    x$model_tbl %>%
    dplyr::select(-c("pvalue")) %>%
    dplyr::left_join(
      global_p_merge,
      by = c("row_type", "variable")
    )

  x$call_list <- c(x$call_list, list(add_global = match.call()))

  return(x)
}
