#' This function takes in the meta-data and calculates an appropriate p-value
#'
#' @param data input data set
#' @param variable categorical or continuous variable for which a test with \code{by_var} is desired
#' @param by categorical variable
#' @param group the group variable for clustered data
#' @param type the type of variable, one of categorical or continuous, from the metadata
#' @param test list of statistical tests from meta data
#' @return a table of p-values for each variable
#' @keywords internal
#' @author Emily Zabor

calculate_pvalue <- function(data, variable, by, test, type, group) {
  purrr::pmap_dbl(
    list(variable, by, test, type),
    ~ calculate_pvalue_one(data, ..1, ..2, ..3, ..4, group)
  )
}

calculate_pvalue_one <- function(data, variable, by, test, type, group) {

  # if there is no by variable, and thus test is NA, return NA
  if (is.na(test)) return(NA)

  # convert by variables to factor (some functions don't allow strings)
  data[[by]] <- data[[by]] %>% factor()

  # omitting missing values before calculating pvalues
  data <- data %>% select(c(group, variable, by)) %>% stats::na.omit()

  # Wilcoxon and Kruskal-Wallis tests
  if (test %in% c("wilcox.test", "kruskal.test")) {
    tryCatch(
      pval <- stats::kruskal.test(data[[variable]], data[[by]])$p.value,
      warning = function(w) {
        message(paste0("For variable ", variable, ": ", w))
        pval <<- NA
      },
      error = function(e) {
        message(paste0("For variable ", variable, ": ", e))
        pval <<- NA
      },
      finally = {
        return(pval)
      }
    )
  }

  # T-test
  if (test == "t.test") {
    tryCatch(
      pval <- stats::t.test(data[[variable]] ~ data[[by]])$p.value,
      warning = function(w) {
        message(paste0("For variable ", variable, ": ", w))
        pval <<- NA
      },
      error = function(e) {
        message(paste0("For variable ", variable, ": ", e))
        pval <<- NA
      },
      finally = {
        return(pval)
      }
    )
  }

  # Chi-squared test
  if (test == "chisq.test") {
    tryCatch(
      pval <- stats::chisq.test(data[[variable]], data[[by]])$p.value,
      warning = function(w) {
        message(paste0("For variable ", variable, ": ", w))
        pval <<- NA
      },
      error = function(e) {
        message(paste0("For variable ", variable, ": ", e))
        pval <<- NA
      },
      finally = {
        return(pval)
      }
    )
  }

  # Fisher's exact test
  if (test == "fisher.test") {
    tryCatch(
      pval <- stats::fisher.test(data[[variable]], data[[by]])$p.value,
      warning = function(w) {
        message(paste0("For variable ", variable, ": ", w))
        pval <<- NA
      },
      error = function(e) {
        message(paste0("For variable ", variable, ": ", e))
        pval <<- NA
      },
      finally = {
        return(pval)
      }
    )
  }

  # Random effects - continuous or dichotomous
  if (test == "re" & type %in% c("continuous", "dichotomous")) {
    form1 <- get("as.formula")(paste0(by, " ~ ", variable, " + (1 | ", group, ")"))
    mod1 <- tryCatch(
      lme4::glmer(form1, data = get("na.omit")(data), family = get("binomial")),
      warning = function(w) {
        message(paste0("For variable ", variable, ": ", w))
      },
      error = function(e) {
        message(paste0("For variable ", variable, ": ", e))
      }
    )
    if (class(mod1) != "glmerMod") {
      return(NA)
    } else {
      return(summary(mod1)$coefficients[2, "Pr(>|z|)"])
    }
  }

  # Random effects - categorical
  if (test == "re" & type == "categorical") {
    form0 <- get("as.formula")(paste0(by, " ~ 1 + (1 | ", group, ")"))
    form1 <- get("as.formula")(paste0(
      by, " ~ factor(", variable,
      ") + (1 | ", group, ")"
    ))
    mod0 <- tryCatch(
      lme4::glmer(form0, data = get("na.omit")(data), family = get("binomial")),
      warning = function(w) {
        message(paste0("For variable ", variable, ": ", w))
      },
      error = function(e) {
        message(paste0("For variable ", variable, ": ", e))
      }
    )
    mod1 <- tryCatch(
      lme4::glmer(form1, data = get("na.omit")(data), family = get("binomial")),
      warning = function(w) {
        message(paste0("For variable ", variable, ": ", w))
      },
      error = function(e) {
        message(paste0("For variable ", variable, ": ", e))
      }
    )
    if (class(mod1) != "glmerMod") {
      return(NA)
    } else {
      return(get("anova")(mod0, mod1)$"Pr(>Chisq)"[2])
    }
  }
}


# calculate_pvalue_one(data = mtcars, variable = "hp", by = "am",
#                      test = "wilcox.test", group = NULL, type = "continuous")
# calculate_pvalue_one(data = mtcars, variable = "hp", by = "am",
#                      test = "kruskal.test", group = NULL, type = "continuous")
# calculate_pvalue_one(data = mtcars, variable = "hp", by = "am",
#                      test = "t.test", group = NULL, type = "continuous")
# calculate_pvalue_one(data = mtcars, variable = "gear", by = "am",
#                      test = "chisq.test", group = NULL, type = "categorical")
# calculate_pvalue_one(data = mtcars, variable = "gear", by = "am",
#                      test = "fisher.test", group = NULL, type = "categorical")
# calculate_pvalue_one(data = mtcars, variable = "hp", by = "am",
#                      group = "gear", test = "re", type = "continuous")
# calculate_pvalue_one(data = mtcars, variable = "hp", by = "am",
#                      group = "gear", test = "re", type = "categorical")
# calculate_pvalue_one(data = mtcars, variable = "mpg", by = "am",
#                      group = "gear", test = "re", type = "continuous")
