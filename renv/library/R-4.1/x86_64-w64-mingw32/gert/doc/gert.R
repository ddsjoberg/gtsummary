## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  error = TRUE
)

## ----setup--------------------------------------------------------------------
library(gert)

## ----eval = FALSE-------------------------------------------------------------
#  git_config_global_set("user.name", "Jerry Johnson")
#  git_config_global_set("user.email", "jerry@gmail.com")

## -----------------------------------------------------------------------------
git_config_global()

## -----------------------------------------------------------------------------
(path <- file.path(tempdir(), "aaa", "bbb", "repo_ccc"))
dir.exists(path)

(r <- git_init(path))
dir.exists(path)

## -----------------------------------------------------------------------------
git_find(r)

dir.create(file.path(r, "child_dir"))
git_find(file.path(r, "child_dir"))

git_find(file.path(tempdir(), "aaa", "bbb"))

## -----------------------------------------------------------------------------
r2 <- file.path(tempdir(), "repo_ddd")
dir.create(r2)

git_init(r2)

## -----------------------------------------------------------------------------
unlink(r, recursive = TRUE)
unlink(r2, recursive = TRUE)

