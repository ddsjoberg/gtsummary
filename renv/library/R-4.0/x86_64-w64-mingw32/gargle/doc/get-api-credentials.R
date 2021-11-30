## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----eval = FALSE-------------------------------------------------------------
#  library(googlesheets4)
#  
#  gs4_auth_configure(api_key = "YOUR_API_KEY_GOES_HERE")
#  gs4_deauth()
#  
#  # now you can read public resources, such as official example Sheets,
#  # without any need for auth
#  gs4_example("gapminder") %>%
#    read_sheet()

## ----eval = FALSE-------------------------------------------------------------
#  library(googledrive)
#  
#  # method 1: direct provision client ID and secret
#  google_app <- httr::oauth_app(
#    "my-very-own-google-app",
#    key = "123456789.apps.googleusercontent.com",
#    secret = "abcdefghijklmnopqrstuvwxyz"
#  )
#  drive_auth_configure(app = google_app)
#  
#  # method 2: provide filepath to JSON containing client ID and secret
#  drive_auth_configure(
#    path = "/path/to/the/JSON/you/downloaded/from/gcp/console.json"
#  )
#  
#  # now any new OAuth tokens are obtained with the configured app

## ----eval = FALSE-------------------------------------------------------------
#  # googledrive
#  drive_auth(path = "/path/to/your/service-account-token.json")

