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
penguin_train <- penguins[1:300,]
penguin_test  <- penguins[-(1:300),]

## -----------------------------------------------------------------------------
penguin_form <- mold(
  log(body_mass_g) ~ species + bill_length_mm, 
  penguin_train, 
  blueprint = default_formula_blueprint(indicators = "none")
)

formula_eng <- penguin_form$blueprint

formula_eng

## -----------------------------------------------------------------------------
forge(penguin_test, formula_eng)

## -----------------------------------------------------------------------------
forge(penguin_test, formula_eng, outcomes = TRUE)

## ---- error=TRUE--------------------------------------------------------------
test_missing_column <- subset(penguin_test, select = -species)

forge(test_missing_column, formula_eng)

## ---- error=TRUE--------------------------------------------------------------
test_species_double <- penguin_test
test_species_double$species <- as.double(test_species_double$species)

forge(test_species_double, formula_eng)

## -----------------------------------------------------------------------------
test_species_character <- penguin_test
test_species_character$species <- as.character(test_species_character$species)

forged_char <- forge(test_species_character, formula_eng)

forged_char$predictors

class(forged_char$predictors$species)

levels(forged_char$predictors$species)

## ---- warning=TRUE------------------------------------------------------------
test_species_lossy <- penguin_test
test_species_lossy$species <- as.character(test_species_lossy$species)
test_species_lossy$species[2] <- "im new!"

forged_lossy <- forge(test_species_lossy, formula_eng)

forged_lossy$predictors

## ---- error=FALSE, warning=FALSE, message = FALSE-----------------------------
library(recipes)

rec <- recipe(bill_length_mm ~ body_mass_g + species, penguin_train) %>%
  step_dummy(species)

penguin_recipe <- mold(rec, penguin_train)

penguin_recipe$predictors

## -----------------------------------------------------------------------------
recipe_eng <- penguin_recipe$blueprint

recipe_eng

## -----------------------------------------------------------------------------
forge(penguin_test, recipe_eng, outcomes = TRUE)

## -----------------------------------------------------------------------------
rec2 <- recipe(bill_length_mm ~ body_mass_g + species, penguin_train) %>%
  step_dummy(species) %>%
  step_center(bill_length_mm) # Here we modify the outcome

penguin_recipe2 <- mold(rec2, penguin_train)

recipe_eng_log_outcome <- penguin_recipe2$blueprint

## ---- error=TRUE--------------------------------------------------------------
penguin_test_no_outcome <- subset(penguin_test, select = -bill_length_mm)

forge(penguin_test_no_outcome, recipe_eng_log_outcome)

## -----------------------------------------------------------------------------
rec3 <- recipe(bill_length_mm ~ body_mass_g + species, penguin_train) %>%
  step_dummy(species) %>%
  step_center(bill_length_mm, skip = TRUE)

penguin_recipe3 <- mold(rec3, penguin_train)

recipe_eng_skip_outcome <- penguin_recipe3$blueprint

forge(penguin_test_no_outcome, recipe_eng_skip_outcome)

## -----------------------------------------------------------------------------
forge(penguin_test, recipe_eng_skip_outcome, outcomes = TRUE)$outcomes

# Notice that the `outcome` values haven't been centered
# and are the same as before
head(penguin_test$bill_length_mm)

