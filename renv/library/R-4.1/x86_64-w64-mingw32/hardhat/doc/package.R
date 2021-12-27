## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

options(rlang_backtrace_on_error = "none")

## ----setup--------------------------------------------------------------------
library(hardhat)
library(tibble)
library(modeldata)

data(penguins)
penguins <- na.omit(penguins)

## ----out.width = '100%', echo = FALSE-----------------------------------------
knitr::include_graphics("../man/figures/Fitting.png")

## ----out.width = '100%', echo = FALSE-----------------------------------------
knitr::include_graphics("../man/figures/Prediction.png")

## -----------------------------------------------------------------------------
new_simple_lm <- function(coefs, coef_names, blueprint) {
  
  if (!is.numeric(coefs)) {
    stop("`coefs` should be a numeric vector.", call. = FALSE)
  }
  
  if (!is.character(coef_names)) {
    stop("`coef_names` should be a character vector.", call. = FALSE)
  }
  
  if (length(coefs) != length(coef_names)) {
    stop("`coefs` and `coef_names` must have the same length.")
  }
  
  new_model(
    coefs = coefs, 
    coef_names = coef_names,
    blueprint = blueprint, 
    class = "simple_lm"
  )
}

## -----------------------------------------------------------------------------
manual_model <- new_simple_lm(1, "my_coef", default_xy_blueprint())

manual_model

names(manual_model)

manual_model$blueprint

## -----------------------------------------------------------------------------
simple_lm_impl <- function(predictors, outcomes) {
  lm_fit <- lm.fit(predictors, outcomes)
  
  coefs <- lm_fit$coefficients
  
  coef_names <- names(coefs)
  coefs <- unname(coefs)
  
  list(
    coefs = coefs,
    coef_names = coef_names
  )
}

## -----------------------------------------------------------------------------
predictors <- as.matrix(subset(penguins, select = bill_length_mm))
outcomes <- penguins$body_mass_g

simple_lm_impl(predictors, outcomes)

## -----------------------------------------------------------------------------
simple_lm_bridge <- function(processed) {
  
  validate_outcomes_are_univariate(processed$outcomes)
  
  predictors <- as.matrix(processed$predictors)
  outcomes <- processed$outcomes[[1]]
  
  fit <- simple_lm_impl(predictors, outcomes)
  
  new_simple_lm(
    coefs = fit$coefs,
    coef_names = fit$coef_names,
    blueprint = processed$blueprint
  )
}

## -----------------------------------------------------------------------------
# Simulate formula interface
processed_1 <- mold(bill_length_mm ~ body_mass_g + species, penguins)

# Simulate xy interface
processed_2 <- mold(x = penguins["body_mass_g"], y = penguins$bill_length_mm)

simple_lm_bridge(processed_1)

simple_lm_bridge(processed_2)

## ---- error=TRUE--------------------------------------------------------------
multi_outcome <- mold(bill_length_mm + bill_depth_mm ~ body_mass_g + species, penguins)

simple_lm_bridge(multi_outcome)

## -----------------------------------------------------------------------------
# Generic
simple_lm <- function(x, ...) {
  UseMethod("simple_lm")
}

# Default
simple_lm.default <- function(x, ...) {
  stop(
    "`simple_lm()` is not defined for a '", class(x)[1], "'.", 
    call. = FALSE
  )
}

# XY method - data frame
simple_lm.data.frame <- function(x, y, ...) {
  processed <- mold(x, y)
  simple_lm_bridge(processed)
}

# XY method - matrix
simple_lm.matrix <- function(x, y, ...) {
  processed <- mold(x, y)
  simple_lm_bridge(processed)
}

# Formula method
simple_lm.formula <- function(formula, data, ...) {
  processed <- mold(formula, data)
  simple_lm_bridge(processed)
}

# Recipe method
simple_lm.recipe <- function(x, data, ...) {
  processed <- mold(x, data)
  simple_lm_bridge(processed)
}

## -----------------------------------------------------------------------------
predictors <- penguins[c("bill_length_mm", "bill_depth_mm")]
outcomes_vec <- penguins$body_mass_g
outcomes_df <- penguins["body_mass_g"]

# Vector outcome
simple_lm(predictors, outcomes_vec)

# 1 column data frame outcome
simple_lm(predictors, outcomes_df)

# Formula interface
simple_lm(body_mass_g ~ bill_length_mm + bill_depth_mm, penguins)

## ---- warning=FALSE, message=FALSE--------------------------------------------
library(recipes)

# - Log a predictor
# - Generate dummy variables for factors
simple_lm(body_mass_g ~ log(bill_length_mm) + species, penguins)

# Same, but with a recipe
rec <- recipe(body_mass_g ~ bill_length_mm + species, penguins) %>%
  step_log(bill_length_mm) %>%
  step_dummy(species, one_hot = TRUE)

simple_lm(rec, penguins)

## -----------------------------------------------------------------------------
simple_lm <- function(x, ...) {
  UseMethod("simple_lm")
}

simple_lm.data.frame <- function(x, y, intercept = TRUE, ...) {
  blueprint <- default_xy_blueprint(intercept = intercept)
  processed <- mold(x, y, blueprint = blueprint)
  simple_lm_bridge(processed)
}

simple_lm.matrix <- function(x, y, intercept = TRUE,...) {
  blueprint <- default_xy_blueprint(intercept = intercept)
  processed <- mold(x, y, blueprint = blueprint)
  simple_lm_bridge(processed)
}

simple_lm.formula <- function(formula, data, intercept = TRUE, ...) {
  blueprint <- default_formula_blueprint(intercept = intercept)
  processed <- mold(formula, data, blueprint = blueprint)
  simple_lm_bridge(processed)
}

simple_lm.recipe <- function(x, data, intercept = TRUE, ...) {
  blueprint <- default_recipe_blueprint(intercept = intercept)
  processed <- mold(x, data, blueprint = blueprint)
  simple_lm_bridge(processed)
}

## -----------------------------------------------------------------------------
# By default an intercept is included
simple_lm(predictors, outcomes_df)

# But the user can turn this off
simple_lm(body_mass_g ~ log(bill_length_mm) + species, penguins, intercept = FALSE)

## -----------------------------------------------------------------------------
predict_simple_lm_numeric <- function(object, predictors) {
  
  coefs <- object$coefs
  
  pred <- as.vector(predictors %*% coefs)
  
  out <- spruce_numeric(pred)
  
  out
}

## -----------------------------------------------------------------------------
model <- simple_lm(bill_length_mm ~ body_mass_g + species, penguins)

predictors <- forge(penguins, model$blueprint)$predictors
predictors <- as.matrix(predictors)

predict_simple_lm_numeric(model, predictors)

## -----------------------------------------------------------------------------
predict_simple_lm_bridge <- function(type, object, predictors) {
  
  type <- rlang::arg_match(type, "numeric")
  
  predictors <- as.matrix(predictors)
  
  switch(
    type,
    numeric = predict_simple_lm_numeric(object, predictors)
  )
}

## ---- error=TRUE--------------------------------------------------------------
model <- simple_lm(bill_length_mm ~ body_mass_g + species, penguins)

# Pass in the data frame
predictors <- forge(penguins, model$blueprint)$predictors

predict_simple_lm_bridge("numeric", model, predictors)

# Partial matches are an error
predict_simple_lm_bridge("numer", model, predictors)

## -----------------------------------------------------------------------------
predict.simple_lm <- function(object, new_data, type = "numeric", ...) {
  
  # Enforces column order, type, column names, etc
  processed <- forge(new_data, object$blueprint)
  
  out <- predict_simple_lm_bridge(type, object, processed$predictors)
  
  validate_prediction_size(out, new_data)
  
  out
}

## -----------------------------------------------------------------------------
model <- simple_lm(bill_length_mm ~ log(body_mass_g) + species, penguins)

predict(model, penguins)

## ---- warning=TRUE, error=TRUE------------------------------------------------
# `new_data` isn't a data frame
predict(model, penguins$species)

# Missing a required column
predict(model, subset(penguins, select = -body_mass_g))

# In this case, 'species' is a character, 
# but can be losslessy converted to a factor.
# That happens for you automatically and silently.
penguins_chr_species <- transform(penguins, species = as.character(species))

predict(model, penguins_chr_species)

# Slightly different from above. Here, 'species' is a character, 
# AND has an extra unexpected factor level. It is 
# removed with a warning, but you still get a factor 
# with the correct levels
penguins_chr_bad_species <- penguins_chr_species
penguins_chr_bad_species$species[1] <- "new_level"

predict(model, penguins_chr_bad_species)

# This case throws an error.
# Here, 'species' is a double and
# when it should have been a factor.
# You can't cast a double to a factor!
penguins_dbl_species <- transform(penguins, species = 1)

predict(model, penguins_dbl_species)

