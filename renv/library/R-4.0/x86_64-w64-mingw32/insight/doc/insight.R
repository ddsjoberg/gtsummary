## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
if (!requireNamespace("lme4", quietly = TRUE)) {
  knitr::opts_chunk$set(eval = FALSE)
}

## ----out.width="100%", echo=FALSE---------------------------------------------
knitr::include_graphics("insight_design_1.png", dpi = 72)

## ----out.width="65%", echo=FALSE----------------------------------------------
knitr::include_graphics("figure3a.png", dpi = 72)

## ----out.width="80%", echo=FALSE----------------------------------------------
knitr::include_graphics("figure3b.png", dpi = 72)

## ----out.width="80%", echo=FALSE----------------------------------------------
knitr::include_graphics("figure3c.png", dpi = 72)

## ----out.width="65%", echo=FALSE----------------------------------------------
knitr::include_graphics("figure3d.png", dpi = 72)

## ----echo=TRUE,message=FALSE,warning=FALSE------------------------------------
library(insight)
library(lme4)
data(sleepstudy)
sleepstudy$mygrp <- sample(1:5, size = 180, replace = TRUE)
sleepstudy$mysubgrp <- NA
sleepstudy$Weeks <- sleepstudy$Days / 7
sleepstudy$cat <- as.factor(sample(letters[1:4], nrow(sleepstudy), replace = TRUE))

for (i in 1:5) {
  filter_group <- sleepstudy$mygrp == i
  sleepstudy$mysubgrp[filter_group] <-
    sample(1:30, size = sum(filter_group), replace = TRUE)
}

model <- lmer(
  Reaction ~ Days + I(Days^2) + log1p(Weeks) + cat +
    (1 | mygrp / mysubgrp) + 
    (1 + Days | Subject),
  data = sleepstudy
)

## ----echo=TRUE,message=FALSE,warning=FALSE------------------------------------
# find the response variable
find_response(model)

# find all predictors, fixed part by default
find_predictors(model)

# find random effects, grouping factors only
find_random(model)

# find random slopes
find_random_slopes(model)

# find all predictors, including random effects
find_predictors(model, effects = "all", component = "all")

# find all terms, including response and random effects
# this is essentially the same as the previous example plus response
find_terms(model)

# find all variables, i.e. also quadratic or log-transformed predictors
find_variables(model)

## ----echo=TRUE,message=FALSE,warning=FALSE------------------------------------
# find model parameters, i.e. coefficients
find_parameters(model)

