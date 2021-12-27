## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment  = "#>",
  eval     = FALSE
)

## ----eval=FALSE---------------------------------------------------------------
#  renv::dependencies()

## ----eval=FALSE---------------------------------------------------------------
#  library(dplyr)
#  library(ggplot2)

## ----eval=FALSE---------------------------------------------------------------
#  for (package in c("dplyr", "ggplot2")) {
#    library(package, character.only = TRUE)
#  }

## ----eval=FALSE---------------------------------------------------------------
#  renv::settings$snapshot.type("all")

## ----eval=FALSE---------------------------------------------------------------
#  renv::settings$ignored.packages("<package>")

## ----eval=FALSE---------------------------------------------------------------
#  renv::settings$snapshot.type("explicit")

## ----eval=FALSE---------------------------------------------------------------
#  renv::install()

## ----eval=FALSE---------------------------------------------------------------
#  RENV_CONFIG_SANDBOX_ENABLED = FALSE

## ----eval=FALSE---------------------------------------------------------------
#  getOption("download.file.method")

## ----eval=FALSE---------------------------------------------------------------
#  renv:::renv_download_method()

## ----eval=FALSE---------------------------------------------------------------
#  Sys.setenv(RENV_DOWNLOAD_FILE_METHOD = getOption("download.file.method"))

