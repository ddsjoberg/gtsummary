## ----message=FALSE, warning=FALSE, include=FALSE------------------------------
library(knitr)
options(knitr.kable.NA = "")
options(digits = 2)
knitr::opts_chunk$set(comment = ">")

set.seed(1)
pkgs <- c("effectsize", "parameters", "rstanarm", "bayestestR", "car")
if (!all(sapply(pkgs, require, quietly = TRUE, character.only = TRUE))) {
  knitr::opts_chunk$set(eval = FALSE)
}

## ---- warning=FALSE-----------------------------------------------------------
library(rstanarm)

data("hardlyworking", package = "effectsize")

head(hardlyworking)


mod <- stan_glm(salary ~ xtra_hours + n_comps + seniority,
  data = hardlyworking,
  prior = normal(0, scale = c(1, 0.5, 0.5), autoscale = TRUE), # set some priors
  refresh = 0
)

parameters::model_parameters(mod, test = NULL)

## -----------------------------------------------------------------------------
library(effectsize)

standardize_parameters(mod, method = "refit", ci = 0.89)

## -----------------------------------------------------------------------------
library(effectsize)

standardize_parameters(mod,
  method = "basic", ci = 0.89,
  centrality = "MAP", ci_method = "eti"
)

## -----------------------------------------------------------------------------
hardlyworking$age_f <- cut(hardlyworking$age,
  breaks = c(25, 35, 45), right = FALSE,
  labels = c("Young", "Less_young")
)
hardlyworking$comps_f <- cut(hardlyworking$n_comps,
  breaks = c(0, 1, 2, 3),
  include.lowest = TRUE,
  right = FALSE
)

table(hardlyworking$age_f, hardlyworking$comps_f)

## -----------------------------------------------------------------------------
# use (special) effects coding
contrasts(hardlyworking$age_f) <- bayestestR::contr.bayes
contrasts(hardlyworking$comps_f) <- bayestestR::contr.bayes

modAOV <- stan_glm(salary ~ age_f * comps_f,
  data = hardlyworking, family = gaussian(),
  refresh = 0
)

## -----------------------------------------------------------------------------
pes_posterior <- eta_squared_posterior(modAOV,
  draws = 500, # how many samples from the PPD?
  partial = TRUE, # partial eta squared
  # type 3 SS
  ss_function = car::Anova, type = 3
)

head(pes_posterior)

bayestestR::describe_posterior(pes_posterior, rope_range = c(0, 0.1), test = "rope")

## -----------------------------------------------------------------------------
modAOV_f <- lm(salary ~ age_f * comps_f,
  data = hardlyworking
)

eta_squared(car::Anova(modAOV_f, type = 3))

