## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

options(rlang_backtrace_on_error = "none")

## ----setup--------------------------------------------------------------------
library(hardhat)
library(modeldata)

data(penguins)
penguins <- na.omit(penguins)

## -----------------------------------------------------------------------------
penguin_form <- mold(body_mass_g ~ log(bill_length_mm), penguins)

names(penguin_form)

## -----------------------------------------------------------------------------
penguin_form$predictors

## -----------------------------------------------------------------------------
penguin_form$outcomes

## -----------------------------------------------------------------------------
mold(body_mass_g ~ log(bill_length_mm) + offset(bill_depth_mm), penguins)$extras

## -----------------------------------------------------------------------------
identical(
  mold(~ body_mass_g, penguins), 
  mold(~ body_mass_g, penguins, blueprint = default_formula_blueprint())
)

## -----------------------------------------------------------------------------
no_intercept <- mold(~ body_mass_g, penguins)

no_intercept$predictors

## -----------------------------------------------------------------------------
with_intercept <- mold(
  ~ body_mass_g, penguins, 
  blueprint = default_formula_blueprint(intercept = TRUE)
)

with_intercept$predictors

## ---- error=TRUE--------------------------------------------------------------
mold(~ body_mass_g - 1, penguins)

mold(~ body_mass_g + 0, penguins)

## -----------------------------------------------------------------------------
expanded_dummies <- mold(~ body_mass_g + species, penguins)

expanded_dummies$predictors

## -----------------------------------------------------------------------------
non_expanded_dummies <- mold(
  ~ body_mass_g + species, penguins, 
  blueprint = default_formula_blueprint(indicators = "none")
)

non_expanded_dummies$predictors

## -----------------------------------------------------------------------------
k_cols <- mold(~ species, penguins)

k_minus_one_cols <- mold(
  ~ species, penguins, 
  blueprint = default_formula_blueprint(intercept = TRUE)
)

colnames(k_cols$predictors)

colnames(k_minus_one_cols$predictors)

## -----------------------------------------------------------------------------
.f <- cbind(body_mass_g, bill_length_mm) ~ bill_depth_mm

frame <- model.frame(.f, penguins)

head(frame)

## -----------------------------------------------------------------------------
ncol(frame)

class(frame$`cbind(body_mass_g, bill_length_mm)`)

head(frame$`cbind(body_mass_g, bill_length_mm)`)

## -----------------------------------------------------------------------------
multivariate <- mold(body_mass_g + log(bill_length_mm) ~ bill_depth_mm, penguins)

multivariate$outcomes

## -----------------------------------------------------------------------------
x <- subset(penguins, select = -body_mass_g)
y <- subset(penguins, select =  body_mass_g)

penguin_xy <- mold(x, y)

penguin_xy$predictors

penguin_xy$outcomes

## -----------------------------------------------------------------------------
xy_with_intercept <- mold(x, y, blueprint = default_xy_blueprint(intercept = TRUE))

xy_with_intercept$predictors

## -----------------------------------------------------------------------------
mold(x, y$body_mass_g)$outcomes

## ---- message=FALSE, warning=FALSE--------------------------------------------
library(recipes)

rec <- recipe(bill_length_mm ~ species + bill_depth_mm, penguins) %>%
  step_log(bill_length_mm) %>%
  step_dummy(species)

penguin_recipe <- mold(rec, penguins)

penguin_recipe$predictors

penguin_recipe$outcomes

## -----------------------------------------------------------------------------
recipe_with_intercept <- mold(
  rec, penguins, 
  blueprint = default_recipe_blueprint(intercept = TRUE)
)

recipe_with_intercept$predictors

