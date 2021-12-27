## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(gargle)

## -----------------------------------------------------------------------------
ddi_dir <- system.file("discovery-doc-ingest", package = "gargle")
list.files(ddi_dir)

## ---- eval = FALSE------------------------------------------------------------
#  .endpoints[["drive.files.create"]]

## ----eval = FALSE-------------------------------------------------------------
#  # googledrive::
#  request_generate <- function(endpoint = character(),
#                               params = list(),
#                               key = NULL,
#                               token = drive_token()) {
#    ept <- .endpoints[[endpoint]]
#    if (is.null(ept)) {
#      stop_glue("\nEndpoint not recognized:\n  * {endpoint}")
#    }
#  
#    ## modifications specific to googledrive package
#    params$key <- key %||% params$key %||% drive_api_key()
#    if (!is.null(ept$parameters$supportsTeamDrives)) {
#      params$supportsTeamDrives <- TRUE
#    }
#  
#    req <- gargle::request_develop(endpoint = ept, params = params)
#    gargle::request_build(
#      path = req$path,
#      method = req$method,
#      params = req$params,
#      body = req$body,
#      token = token
#    )
#  }

## ---- eval = FALSE------------------------------------------------------------
#  # gargle::
#  request_make <- function(x, ..., user_agent = gargle_user_agent()) {
#    stopifnot(is.character(x$method))
#    method <- switch(
#      x$method,
#      GET    = httr::GET,
#      POST   = httr::POST,
#      PATCH  = httr::PATCH,
#      PUT    = httr::PUT,
#      DELETE = httr::DELETE,
#      abort(glue("Not a recognized HTTP method: {bt(x$method)}"))
#    )
#    method(
#      url = x$url,
#      body = x$body,
#      x$token,
#      user_agent,
#      ...
#    )
#  }

## ---- eval = FALSE------------------------------------------------------------
#  # googledrive::
#  request_make <- function(x, ...) {
#    gargle::request_make(x, ..., user_agent = drive_ua())
#  }

## ----asis = TRUE, echo = FALSE, comment = NA----------------------------------
cat(readLines(fs::path(ddi_dir, "method-properties-humane.txt")), sep = "\n")

## ----asis = TRUE, echo = FALSE, comment = NA----------------------------------
cat(readLines(fs::path(ddi_dir, "api-wide-parameters-humane.txt")), sep = "\n")

## ----asis = TRUE, echo = FALSE, comment = NA----------------------------------
cat(readLines(fs::path(ddi_dir, "parameter-properties-humane.txt")), sep = "\n")

