## ----message=FALSE, warning=FALSE, include=FALSE------------------------------
library(knitr)
options(knitr.kable.NA = "")
knitr::opts_chunk$set(comment = ">")
options(digits = 3)

pkgs <- c("effectsize", "ggplot2", "correlation", "parameters", "bayestestR")
if (!all(sapply(pkgs, require, quietly = TRUE, character.only = TRUE))) {
  knitr::opts_chunk$set(eval = FALSE)
}

## -----------------------------------------------------------------------------
set.seed(1)
data <- bayestestR::simulate_difference(
  n = 10,
  d = 0.2,
  names = c("Group", "Outcome")
)

## ---- echo=FALSE--------------------------------------------------------------
print(data, digits = 3)

## -----------------------------------------------------------------------------
cohens_d(Outcome ~ Group, data = data)

## ---- warning=FALSE-----------------------------------------------------------
correlation::correlation(data, include_factors = TRUE)[2, ]

## -----------------------------------------------------------------------------
d_to_r(-0.31)

## -----------------------------------------------------------------------------
fit <- lm(mpg ~ am + hp, data = mtcars)

parameters::model_parameters(fit)

# A couple of ways to get partial-d:
5.28 / sigma(fit)
t_to_d(4.89, df_error = 29)[[1]]

## -----------------------------------------------------------------------------
t_to_r(4.89, df_error = 29)

correlation::correlation(mtcars[, c("mpg", "am", "hp")], partial = TRUE)[1, ]

# all close to:
d_to_r(1.81)

## -----------------------------------------------------------------------------
# 1. Set a threshold
thresh <- 0

# 2. dichotomize the outcome
data$Outcome_binom <- data$Outcome < thresh

# 3. Fit a logistic regression:
fit <- glm(Outcome_binom ~ Group,
  data = data,
  family = binomial()
)

parameters::model_parameters(fit)

# Convert log(OR) (the coefficient) to d
oddsratio_to_d(-0.81, log = TRUE)

## -----------------------------------------------------------------------------
OR <- 3.5
baserate <- 0.85

oddsratio_to_riskratio(OR, baserate)

## -----------------------------------------------------------------------------
OR <- 3.5
baserate <- 0.04

oddsratio_to_riskratio(OR, baserate)

