## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ---- eval = FALSE------------------------------------------------------------
#  # googledrive::
#  drive_auth <- function(email = gargle::gargle_oauth_email(),
#                         path = NULL,
#                         scopes = "https://www.googleapis.com/auth/drive",
#                         cache = gargle::gargle_oauth_cache(),
#                         use_oob = gargle::gargle_oob_default(),
#                         token = NULL) {
#    cred <- gargle::token_fetch(
#      scopes = scopes,
#      app = drive_oauth_app() %||% <BUILT_IN_DEFAULT_APP>,
#      email = email,
#      path = path,
#      package = "googledrive",
#      cache = cache,
#      use_oob = use_oob,
#      token = token
#    )
#    if (!inherits(cred, "Token2.0")) {
#      # throw an informative error here
#    }
#    .auth$set_cred(cred)
#    .auth$set_auth_active(TRUE)
#  
#    invisible()
#  }

## ----eval = FALSE-------------------------------------------------------------
#  .auth <- gargle::init_AuthState(
#    package     = "googledrive",
#    auth_active = TRUE
#    # app = NULL,
#    # api_key = NULL,
#    # cred = NULL
#  )

## ---- eval = FALSE------------------------------------------------------------
#  library(googledrive)
#  
#  google_app <- httr::oauth_app(
#    appname = "acme-corp",
#    key     = "123456789.apps.googleusercontent.com",
#    secret  = "abcdefghijklmnopqrstuvwxyz"
#  )
#  drive_auth_configure(app = google_app)
#  
#  drive_oauth_app()
#  #> <oauth_app> acme-corp
#  #>   key:    123456789.apps.googleusercontent.com
#  #>   secret: <hidden>

## ---- eval = FALSE------------------------------------------------------------
#  library(googledrive)
#  
#  drive_auth_configure(api_key = "123456789")
#  
#  drive_api_key()
#  #> "123456789"

## ----eval = FALSE-------------------------------------------------------------
#  # googledrive::
#  drive_auth(email = "janedoe_work@gmail.com")

## ---- eval = FALSE------------------------------------------------------------
#  # googledrive::
#  drive_auth <- function(email = gargle::gargle_oauth_email(),
#                         path = NULL,
#                         scopes = "https://www.googleapis.com/auth/drive",
#                         cache = gargle::gargle_oauth_cache(),
#                         use_oob = gargle::gargle_oob_default(),
#                         token = NULL) { ... }

## ---- eval = FALSE------------------------------------------------------------
#  # googledrive::
#  drive_auth(scopes = "https://www.googleapis.com/auth/drive.readonly")

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
#    params$key <- key %||% params$key %||%
#      drive_api_key() %||% <BUILT_IN_DEFAULT_API_KEY>
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

## ----eval = FALSE-------------------------------------------------------------
#  # googledrive::
#  drive_token <- function() {
#    if (isFALSE(.auth$auth_active)) {
#      return(NULL)
#    }
#    if (!drive_has_token()) {
#      drive_auth()
#    }
#    httr::config(token = .auth$cred)
#  }

## ----eval = FALSE-------------------------------------------------------------
#  # googledrive::
#  drive_has_token <- function() {
#    inherits(.auth$cred, "Token2.0")
#  }

## ----eval = FALSE-------------------------------------------------------------
#  library(googledrive)
#  library(googlesheets4)
#  
#  drive_auth(email = "jane_doe@example.com") # gets a suitably scoped token
#                                             # and stashes for googledrive use
#  
#  sheets_auth(token = drive_token())         # registers token with googlesheets4
#  
#  # now work with both packages freely ...

## ----eval = FALSE-------------------------------------------------------------
#  library(googledrive)
#  
#  drive_auth(email = "janedoe_work@gmail.com")
#  # do stuff with Google Drive here, with Jane Doe's "work" account
#  
#  drive_auth(email = "janedoe_personal@gmail.com")
#  # do other stuff with Google Drive here, with Jane Doe's "personal" account
#  
#  drive_auth(path = "/path/to/a/service-account.json")
#  # do other stuff with Google Drive here, using a service account

