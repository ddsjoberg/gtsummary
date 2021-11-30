## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
set.seed(1014)

## ----setup--------------------------------------------------------------------
library(testthat)

## ----include = FALSE----------------------------------------------------------
snapper <- testthat:::SnapshotReporter$new()
options(testthat.snapshotter = snapper)
snapper$start_file("pizza", "test")

## -----------------------------------------------------------------------------
bullets <- function(text, id = NULL) {
  paste0(
    "<ul", if (!is.null(id)) paste0(" id=\"", id, "\""), ">\n", 
    paste0("  <li>", text, "</li>\n", collapse = ""),
    "</ul>\n"
  )
}
cat(bullets("a", id = "x"))

## -----------------------------------------------------------------------------
test_that("bullets", {
  expect_equal(bullets("a"), "<ul>\n  <li>a</li>\n</ul>\n")
  expect_equal(bullets("a", id = "x"), "<ul id=\"x\">\n  <li>a</li>\n</ul>\n")
})

## -----------------------------------------------------------------------------
test_that("bullets", {
  expect_snapshot_output(cat(bullets("a")))
  expect_snapshot_output(cat(bullets("a", "b")))
})

## ---- include = FALSE---------------------------------------------------------
# Reset snapshot test
snapper$end_file()
snapper$start_file("pizza", "test")

## -----------------------------------------------------------------------------
test_that("bullets", {
  expect_snapshot_output(cat(bullets("a")))
  expect_snapshot_output(cat(bullets("a", "b")))
})

## ---- include = FALSE---------------------------------------------------------
# Reset snapshot test
snapper$end_file()
snapper$start_file("pizza", "test")

## -----------------------------------------------------------------------------
bullets <- function(text, id = NULL) {
  paste0(
    "<ul", if (!is.null(id)) paste0(" id=\"", id, "\""), ">\n", 
    paste0("<li>", text, "</li>\n", collapse = ""),
    "</ul>\n"
  )
}
test_that("bullets", {
  expect_snapshot_output(cat(bullets("a")))
  expect_snapshot_output(cat(bullets("a", "b")))
})

## ---- include = FALSE---------------------------------------------------------
snapper$snaps_cleanup()

