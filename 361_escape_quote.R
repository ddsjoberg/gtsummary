#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Overview ----
# Description: Code to work on solving issue 361.
# Name:        Stephanie Lobaugh
# Date:        2/13/2020: Created
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Program name
pgm <- "task_program"

# Set pathway for output
path <- "./tasks/task_folder/"

# Today's date (yymmdd)
today <- gsub("-","",substring(Sys.Date(),3))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Relevant packages ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

library(biostatR)
library(tidyverse)
library(survey)
library(stringr)
library(clusrank)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Data ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

data(crd)
# "group" is the group variable, "id" is the cluster ID variable)

# Add categorical variable
crd2 <- crd %>%
  mutate(cat = if_else(z < 4,
                       "No",
                       "Yes"))

count(crd2, z, cat)
count(crd2, cat)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Rao-Scott code and function ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

dclus1 <- svydesign(id = ~id, data = crd2)
x <- svychisq(~cat + group, dclus1,
              statistic = "Chisq")
x$p.value
x$method

my_raoscott <- function(data, variable, by, group, ...) {
  result <- list()

  string1 <- paste0("~", group)
  formula1 <- as.formula(string1)

  dclus1 <- svydesign(formula1, data = data)

  string2 <- paste0("~", variable, " + ", by, collapse = "")
  formula2 <- as.formula(string2)

  x <- svychisq(formula2, dclus1, statistic = "Chisq")

  result$p <- x$p.value
  result$test <- x$method
  result
}

my_raoscott(
  data = crd2,
  variable = "cat",
  by = "group",
  group = "id"
)

my_raoscott2 <- function(data, variable, by, group, ...) {
  result <- list()

  string1 <- paste0("~", group)
  formula1 <- as.formula(string1)

  dclus1 <- svydesign(formula1, data = data)

  string2 <- paste0("~", variable, " + ", by, collapse = "")
  formula2 <- as.formula(string2)

  x <- svychisq(formula2, dclus1, statistic = "Chisq")

  result$p <- x$p.value
  result$test <- '"My X^2 method"'
  result
}


my_raoscott2(
  data = crd2,
  variable = "cat",
  by = "group",
  group = "id"
)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Clustered Wilcoxon rank sum test code----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

my_clusWilcox <- function(data, variable, by, group, ...) {
  result <- list()
  x <-
    clusWilcox.test(data[[variable]] ~ data[[by]] + cluster(data[[group]]),
                    method = "rgl", exact = FALSE)
  result$p <- x$p.value
  result$test <- x$method
  result
}

# Test the function
my_clusWilcox(variable = "z",
              by = "group",
              data = crd,
              group = "id")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# McNemar code ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

my_mcnemar <- function(data, variable, by, ...) {
  result <- list()
  result$p <- stats::mcnemar.test(data[[variable]], data[[by]])$p.value
  result$test <- "McNemar's test"
  result
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Function to take method and add escape chars ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

add_p_method_string_replace <- function(string_in) {
  if(is.null(string_in)) {
    return(NULL)
  }

  string_out <- string_in %>%
    str_replace_all(fixed("'"), fixed("\\'")) %>%
    str_replace_all(fixed('"'), fixed('\\"')) %>%
    str_replace_all(fixed("X^2"), "chi-square")
  return(string_out)
}

add_p_method_string_replace(string_in = "Pearson's X^2")
add_p_method_string_replace(string_in = '"Hello world"')
add_p_method_string_replace(string_in = NULL)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Test changes ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Just cat var with a single quote and "X^2" in the method string
tbl <- crd2 %>%
  select(group, id, cat) %>%
  tbl_summary(
    by = group
  ) %>%
  add_p(
    test = vars(cat) ~ "my_raoscott",
    group = id
  )

footnote <- tbl$gt_calls$tab_footnote
locations <- str_locate(footnote, "Statistical tests performed: ")
start <- locations[,2]
method <- substr(footnote, start, str_length(footnote))
method

# Just cat var with a double quote and "X^2" in the method string
crd2 %>%
  select(group, id, cat) %>%
  tbl_summary(
    by = group
  ) %>%
  add_p(
    test = vars(cat) ~ "my_raoscott2",
    group = id
  )

# Cat var with a single quote and "X^2" in the method string
# Cont var also
crd2 %>%
  select(group, id, cat, z) %>%
  tbl_summary(
    by = group
  ) %>%
  add_p(
    test = c(vars(cat) ~ "my_raoscott",
             vars(z) ~ "my_clusWilcox"),
    group = id
  )

# Cat var with a double quote and "X^2" in the method string
# Cont var also
crd2 %>%
  select(group, id, cat, z) %>%
  tbl_summary(
    by = group
  ) %>%
  add_p(
    test = c(vars(cat) ~ "my_raoscott2",
             vars(z) ~ "my_clusWilcox"),
    group = id
  )

trial[c("response", "trt")] %>%
  tbl_summary(by = trt) %>%
  add_p(test = response ~ "my_mcnemar")





