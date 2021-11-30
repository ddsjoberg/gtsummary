## ----include=FALSE------------------------------------------------------------
Sys.setenv("ON_CRAN" = "true")
knitr::opts_chunk$set(comment="")
if(grepl("windows", tolower(Sys.info()[["sysname"]])))
  knitr::opts_chunk$set(comment="", error =TRUE)

## ----cran-installation, eval = FALSE------------------------------------------
#  install.packages("codemetar")

## ----gh-installation, eval = FALSE--------------------------------------------
#  # install.packages("remotes")
#  remotes::install_github("ropensci/codemetar", ref = "dev")

## ----echo=TRUE, eval=FALSE----------------------------------------------------
#  codemetar::write_codemeta()

## ----echo=FALSE, eval = identical(Sys.getenv("NOT_CRAN"), "true")-------------
#  pkg <- "../.."
#  codemetar::write_codemeta(pkg = pkg)

## ----eval = identical(Sys.getenv("NOT_CRAN"), "true")-------------------------
#  library("magrittr")
#  "../../codemeta.json" %>%
#    details::details(summary = "codemetar's codemeta.json",
#                     lang = "json")

## ----echo = FALSE, results='hide', eval = identical(Sys.getenv("NOT_CRAN"), "true")----
#  file.remove(file.path(pkg, "codemeta.json"))

## ---- echo = FALSE------------------------------------------------------------
details::details(system.file("templates", "codemeta-github-actions.yml", package = "codemetar"), 
                 summary = "click here to see the workflow",
                 lang = "yaml")

