## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  eval = FALSE
)

## -----------------------------------------------------------------------------
#  # Approach #1: use an option.
#  # Either specify the user:
#  options(gargle_oauth_email = "jenny@example.org")
#  # Or, if you don't use multiple Google identities, you can be more vague:
#  options(gargle_oauth_email = TRUE)
#  
#  # Approach #2: call PACKAGE_auth() proactively.
#  library(googledrive)
#  # Either specify the user:
#  drive_auth(email = "jenny@example.org")
#  # Or, if you don't use multiple Google identities, you can be more vague:
#  drive_auth(email = TRUE)

## -----------------------------------------------------------------------------
#  library(googledrive)
#  
#  drive_auth(path = "/path/to/your/service-account-token.json")

## -----------------------------------------------------------------------------
#  options(gargle_verbosity = "debug")

## -----------------------------------------------------------------------------
#  library(googledrive)
#  
#  my_oauth_token <- # some process that results in the token you want to use
#  drive_auth(token = my_oauth_token)

## -----------------------------------------------------------------------------
#  # googledrive
#  drive_auth(token = readRDS("/path/to/your/oauth-token.rds"))

## -----------------------------------------------------------------------------
#  library(googledrive)
#  
#  # do anything that triggers auth
#  drive_find(n_max)

## -----------------------------------------------------------------------------
#  options(gargle_oauth_email = TRUE)

## -----------------------------------------------------------------------------
#  options(gargle_oauth_email = "jenny@example.org")

## -----------------------------------------------------------------------------
#  drive_auth(email = TRUE)

## -----------------------------------------------------------------------------
#  drive_auth(email = "jenny@example.org")

## -----------------------------------------------------------------------------
#  library(googledrive)
#  
#  # designate project-specific cache
#  options(gargle_oauth_cache = ".secrets")
#  
#  # check the value of the option, if you like
#  gargle::gargle_oauth_cache()
#  
#  # trigger auth on purpose --> store a token in the specified cache
#  drive_auth()
#  
#  # see your token file in the cache, if you like
#  list.files(".secrets/")

## -----------------------------------------------------------------------------
#  library(googledrive)
#  
#  # trigger auth on purpose --> store a token in the specified cache
#  drive_auth(cache = ".secrets")

## -----------------------------------------------------------------------------
#  library(googledrive)
#  
#  options(
#    gargle_oauth_cache = ".secrets",
#    gargle_oauth_email = TRUE
#  )
#  
#  # now use googledrive with no need for explicit auth
#  drive_find(n_max = 5)

## -----------------------------------------------------------------------------
#  library(googledrive)
#  
#  options(
#    gargle_oauth_cache = ".secrets",
#    gargle_oauth_email = "jenny@example.org"
#  )
#  
#  # now use googledrive with no need for explicit auth
#  drive_find(n_max = 5)

## -----------------------------------------------------------------------------
#  library(googledrive)
#  
#  drive_auth(cache = ".secrets", email = TRUE)
#  
#  # now use googledrive with no need for explicit auth
#  drive_fine(n_max = 5)

## -----------------------------------------------------------------------------
#  library(googledrive)
#  
#  drive_auth(cache = ".secrets", email = "jenny@example.org")
#  
#  # now use googledrive with no need for explicit auth
#  drive_auth(n_max = 5)

## -----------------------------------------------------------------------------
#  options(gargle_verbosity = "debug")

