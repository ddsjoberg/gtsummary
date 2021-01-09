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
#  setwd(project_path)

## ----include = FALSE----------------------------------------------------------
knitr::opts_knit$set(root.dir = project_path)

## -----------------------------------------------------------------------------
getwd()

## -----------------------------------------------------------------------------
here::i_am("prepare/penguins.R")

## -----------------------------------------------------------------------------
library(here)

## -----------------------------------------------------------------------------
here()

## -----------------------------------------------------------------------------
setwd("analysis")
getwd()
here()
setwd("..")

## -----------------------------------------------------------------------------
here("data", "penguins.csv")
readr::read_csv(
  here("data", "penguins.csv"),
  col_types = list(.default = readr::col_guess()),
  n_max = 3
)

## -----------------------------------------------------------------------------
here("data/penguins.csv")

## -----------------------------------------------------------------------------
data_path <- here("data")
here(data_path)
here(data_path, "penguins.csv")

## -----------------------------------------------------------------------------
dr_here()

## -----------------------------------------------------------------------------
dr_here(show_reason = FALSE)

## ----error = TRUE-------------------------------------------------------------
withr::with_dir(tempdir(), {
  print(getwd())
  here::i_am("prepare/penguins.R")
})

## ----error = TRUE-------------------------------------------------------------
library(plyr)
here()

## -----------------------------------------------------------------------------
here::here()

## ----error = TRUE-------------------------------------------------------------
library(conflicted)
here()

conflicted::conflict_prefer("here", "here")
here()

## -----------------------------------------------------------------------------
uuid::UUIDgenerate()

## -----------------------------------------------------------------------------
temp_project_path <- tempfile()
dir.create(temp_project_path)
scripts_path <- file.path(temp_project_path, "scripts")
dir.create(scripts_path)
script_path <- file.path(scripts_path, "script.R")
writeLines(
  c(
    'here::i_am("scripts/script.R")',
    'print("Hello, world!")'
  ),
  script_path
)
fs::dir_tree(temp_project_path)
writeLines(readLines(script_path))

## ----error = TRUE-------------------------------------------------------------
source(script_path, echo = TRUE)

## ----eval = FALSE-------------------------------------------------------------
#  setwd(temp_project_path)

## ----include = FALSE----------------------------------------------------------
knitr::opts_knit$set(root.dir = temp_project_path)

## -----------------------------------------------------------------------------
source(script_path, echo = TRUE)

## -----------------------------------------------------------------------------
library(rprojroot)
find_root(is_rstudio_project, file.path(project_path, "analysis"))

