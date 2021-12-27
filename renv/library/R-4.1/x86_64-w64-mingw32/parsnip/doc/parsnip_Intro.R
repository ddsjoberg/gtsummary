## ----ex_setup, include=FALSE--------------------------------------------------
knitr::opts_chunk$set(
  message = FALSE,
  digits = 3,
  collapse = TRUE,
  comment = "#>"
  )
options(digits = 3)
library(parsnip)
set.seed(368783)

## ----rf-ex--------------------------------------------------------------------
library(parsnip)
rf_mod <- rand_forest(trees = 2000)

## ----rf-tune------------------------------------------------------------------
tune_mtry <- rand_forest(trees = 2000, mtry = varying())
tune_mtry

## ----rf-def-------------------------------------------------------------------
args(rand_forest)

## ----rf-seed------------------------------------------------------------------
rf_with_seed <- 
  rand_forest(trees = 2000, mtry = varying(), mode = "regression") %>%
  set_engine("ranger", seed = 63233)
rf_with_seed

## ---- eval = FALSE------------------------------------------------------------
#  rf_with_seed %>%
#    set_args(mtry = 4) %>%
#    set_engine("ranger") %>%
#    fit(mpg ~ ., data = mtcars)

## ---- eval = FALSE------------------------------------------------------------
#  set.seed(56982)
#  rf_with_seed %>%
#    set_args(mtry = 4) %>%
#    set_engine("randomForest") %>%
#    fit(mpg ~ ., data = mtcars)

