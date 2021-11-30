## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  error = !identical(Sys.getenv("IN_PKGDOWN"), "true")
)

project_path <- system.file("demo-project", package = "here")

## ----echo = FALSE-------------------------------------------------------------
fs::dir_tree(project_path)

## ----eval = FALSE-------------------------------------------------------------
#  setwd(file.path(project_path, "analysis"))

## ----include = FALSE----------------------------------------------------------
knitr::opts_knit$set(root.dir = file.path(project_path, "analysis"))

## -----------------------------------------------------------------------------
getwd()

## -----------------------------------------------------------------------------
here::i_am("analysis/report.Rmd")

## -----------------------------------------------------------------------------
library(here)
here("data", "penguins.csv")
here("data/penguins.csv")

