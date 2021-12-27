## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(gargle)

## -----------------------------------------------------------------------------
gargle_verbosity()

## -----------------------------------------------------------------------------
# save current value
op <- options(gargle_verbosity = "debug")

gargle_verbosity()

# restore original value
options(op)

## -----------------------------------------------------------------------------
gargle_verbosity()

with_gargle_verbosity(
  "debug",
  gargle_verbosity()
)

gargle_verbosity()

f <- function() {
  local_gargle_verbosity("debug")
  gargle_verbosity()
}

f()

gargle_verbosity()

## ---- eval = FALSE------------------------------------------------------------
#  gargle_oauth_sitrep()
#  #' > 14 tokens found in this gargle OAuth cache:
#  #' '~/Library/Caches/gargle'
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

## ----eval = FALSE-------------------------------------------------------------
#  install.packages("googlesheets4")

## ---- echo = FALSE, out.width = "400px"---------------------------------------
knitr::include_graphics("deleted_client.png")

