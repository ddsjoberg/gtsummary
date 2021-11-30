## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(gargle)

## ---- eval = FALSE------------------------------------------------------------
#  token_fetch(scopes, ...)

## -----------------------------------------------------------------------------
names(cred_funs_list())

## ---- eval = FALSE------------------------------------------------------------
#  token_fetch(scopes = <SCOPES>, path = "/path/to/your/service-account.json")
#  
#  # leads to this call:
#  credentials_service_account(
#    scopes = <SCOPES>,
#    path = "/path/to/your/service-account.json"
#  )

## ---- eval = FALSE------------------------------------------------------------
#  token_fetch(scopes = <SCOPES>)
#  
#  # credentials_service_account() fails because no `path`,
#  # which leads to this call:
#  credentials_app_default(
#    scopes = <SCOPES>
#  )

## ---- eval = FALSE------------------------------------------------------------
#  ${GOOGLE_APPLICATION_CREDENTIALS}
#  ${CLOUDSDK_CONFIG}/application_default_credentials.json
#  
#  # on Windows:
#  %APPDATA%\gcloud\application_default_credentials.json
#  %SystemDrive%\gcloud\application_default_credentials.json
#  C:\gcloud\application_default_credentials.json
#  
#  # on not-Windows:
#  ~/.config/gcloud/application_default_credentials.json

## ---- eval = FALSE------------------------------------------------------------
#  token_fetch(scopes = <SCOPES>)
#  # or perhaps
#  token_fetch(scopes = <SCOPES>, service_account = <SERVICE_ACCOUNT>)
#  
#  # credentials_service_account() fails because no `path`,
#  # credentials_app_default() fails because no ADC found,
#  # which leads to one of these calls:
#  credentials_gce(
#    scopes = <SCOPES>,
#    service_account = "default"
#  )
#  # or
#  credentials_gce(
#    scopes = <SCOPES>,
#    service_account = <SERVICE_ACCOUNT>
#  )

## ---- eval = FALSE------------------------------------------------------------
#  token_fetch(token = <TOKEN2.0>)
#  
#  # credentials_service_account() fails because no `path`,
#  # credentials_app_default() fails because no ADC found,
#  # credentials_gce() fails because not on GCE,
#  # which leads to this call:
#  credentials_byo_oauth2(
#    token = <TOKEN2.0>
#  )

## ---- eval = FALSE------------------------------------------------------------
#  token_fetch(scopes = <SCOPES>)
#  
#  # credentials_service_account() fails because no `path`,
#  # credentials_app_default() fails because no ADC found,
#  # credentials_gce() fails because not on GCE,
#  # credentials_byo_oauth2() fails because no `token`,
#  # which leads to this call:
#  credentials_user_oauth2(
#    scopes = <SCOPES>,
#    app = <OAUTH_APP>,
#    package = "<PACKAGE>"
#  )

## ---- eval = FALSE------------------------------------------------------------
#  # user initiates auth or does something that triggers it indirectly
#  THINGY_auth()
#  
#  # which then calls
#  gargle::token_fetch(
#    scopes  = <SCOPES_NEEDED_FOR_THE_THINGY_API>,
#    app     = thingy_app(),
#    package = "thingyr"
#  )
#  
#  # which leads to this call:
#  credentials_user_oauth2(
#    scopes  = <SCOPES_NEEDED_FOR_THE_THINGY_API>,
#    app     = thingy_app(),
#    package = "thingyr"
#  )

## ---- eval = FALSE------------------------------------------------------------
#  gargle2.0_token(
#    email   = gargle_oauth_email(),
#    app     = thingy_app(),
#    package = "thingyr",
#    scope   = <SCOPES_NEEDED_FOR_THE_THINGY_API>,
#    cache   = gargle_oauth_cache()
#  )

## ---- eval = FALSE------------------------------------------------------------
#  The thingyr package is requesting access to your Google account. Select a
#  pre-authorised account or enter '0' to obtain a new token. Press Esc/Ctrl + C to
#  abort.
#  
#  1: janedoe_personal@gmail.com
#  2: janedoe@example.com
#  3: janedoe_work@gmail.com
#  
#  Selection: 3

## ---- eval = FALSE------------------------------------------------------------
#  thingy_auth(email = "janedoe_work@gmail.com")

## ---- eval = FALSE------------------------------------------------------------
#  gargle_oauth_sitrep()
#  #' gargle OAuth cache path:
#  #' /Users/janedoe/.R/gargle/gargle-oauth
#  #'
#  #' 14 tokens found
#  #'
#  #' email                         app         scope                          hash...
#  #' ----------------------------- ----------- ------------------------------ ----------
#  #' abcdefghijklm@gmail.com       thingy      ...bigquery, ...cloud-platform 128f9cc...
#  #' buzzy@example.org             gargle-demo                                15acf95...
#  #' stella@example.org            gargle-demo ...drive                       4281945...
#  #' abcdefghijklm@gmail.com       gargle-demo ...drive                       48e7e76...
#  #' abcdefghijklm@gmail.com       tidyverse                                  69a7353...
#  #' nopqr@ABCDEFG.com             tidyverse   ...spreadsheets.readonly       86a70b9...
#  #' abcdefghijklm@gmail.com       tidyverse   ...drive                       d9443db...
#  #' nopqr@HIJKLMN.com             tidyverse   ...drive                       d9443db...
#  #' nopqr@ABCDEFG.com             tidyverse   ...drive                       d9443db...
#  #' stuvwzyzabcd@gmail.com        tidyverse   ...drive                       d9443db...
#  #' efghijklmnopqrtsuvw@gmail.com tidyverse   ...drive                       d9443db...
#  #' abcdefghijklm@gmail.com       tidyverse   ...drive.readonly              ecd11fa...
#  #' abcdefghijklm@gmail.com       tidyverse   ...bigquery, ...cloud-platform ece63f4...
#  #' nopqr@ABCDEFG.com             tidyverse   ...spreadsheets                f178dd8...

