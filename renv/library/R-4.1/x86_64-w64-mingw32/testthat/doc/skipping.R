## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(testthat)

## -----------------------------------------------------------------------------
skip_if_Tuesday <- function() {
  if (as.POSIXlt(Sys.Date())$wday != 2) {
    return(invisible(TRUE))
  }
  
  skip("Not run on Tuesday")
}

## -----------------------------------------------------------------------------
skip_if_dangerous <- function() {
  if (identical(Sys.getenv("DANGER"), "")) {
    return(invisible(TRUE))
  }
  
  skip("Not run in dangerous enviromnents")
}

## -----------------------------------------------------------------------------
test_that("skip_if_dangerous work", {
  # Test that a skip happens
  withr::local_envvar(DANGER = "yes")
  expect_condition(skip_if_dangerous(), class = "skip") 

  # Test that a skip doesn't happen
  withr::local_envvar(DANGER = "")
  expect_condition(skip_if_dangerous(), NA, class = "skip")
})

