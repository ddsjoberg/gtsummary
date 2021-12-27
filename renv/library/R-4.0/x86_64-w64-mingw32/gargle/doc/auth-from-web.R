## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----eval = FALSE-------------------------------------------------------------
#  library(googledrive)
#  
#  drive_auth(use_oob = TRUE)
#  
#  # now carry on with your work
#  drive_find(n_max = 5)

## ----eval = FALSE-------------------------------------------------------------
#  options(gargle_oob_default = TRUE)

## ---- eval = FALSE------------------------------------------------------------
#  drive_auth <- function(email = gargle::gargle_oauth_email(),
#                         path = NULL,
#                         scopes = "https://www.googleapis.com/auth/drive",
#                         cache = gargle::gargle_oauth_cache(),
#                         use_oob = gargle::gargle_oob_default(),
#                         token = NULL) {...}

