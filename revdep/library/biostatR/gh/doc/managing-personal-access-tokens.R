## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(gh)

## ---- eval = FALSE------------------------------------------------------------
#  gh(endpoint, ..., .token = NULL, ..., .api_url = NULL, ...)

## -----------------------------------------------------------------------------
gitcreds::gitcreds_cache_envvar("https://github.acme.com")

