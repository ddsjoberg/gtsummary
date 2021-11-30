## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----eval=FALSE---------------------------------------------------------------
#  
#  # write down the set of packages required by this script
#  renv::use(
#    "digest",         # use the latest-available version of digest
#    "rlang@0.3.4"     # use an older release of rlang (installed from archive)
#  )
#  
#  # use the requested packages
#  digest::digest(list(answer = 42))
#  

## ----eval=FALSE---------------------------------------------------------------
#  
#  # use a particular lockfile in the context of this script
#  renv::use(lockfile = "/path/to/renv.lock")
#  
#  # the rest of this script will run with the packages as declared
#  # in that lockfile installed into a temporary library path
#  

