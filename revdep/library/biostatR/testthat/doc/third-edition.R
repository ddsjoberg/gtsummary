## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ---- message = FALSE---------------------------------------------------------
library(testthat)
local_edition(3)

## -----------------------------------------------------------------------------
test_that("I can use the 3rd edition", {
  local_edition(3)
  expect_true(TRUE)
})

## -----------------------------------------------------------------------------
test_that("I want to use the 2nd edition", {
  local_edition(2)
  expect_true(TRUE)
})

## ---- error = TRUE------------------------------------------------------------
f1 <- factor(letters[1:3])
f2 <- ordered(letters[1:3], levels = letters[1:4])

local_edition(2)
expect_equal(f1, f2)

local_edition(3)
expect_equal(f1, f2)

## ---- error = TRUE------------------------------------------------------------
dt1 <- dt2 <- ISOdatetime(2020, 1, 2, 3, 4, 0)
attr(dt1, "tzone") <- ""
attr(dt2, "tzone") <- Sys.timezone()

local_edition(2)
expect_equal(dt1, dt2)

local_edition(3)
expect_equal(dt1, dt2)

