# check reverse dependencies

library(devtools)
rev_pkgs <- revdep("Lahman")

# > rev_pkgs
# [1] "baseballDBR"   "broom"         "dbplyr"        "dplyr"         "implyr"       
# [6] "mdsr"          "pinnacle.data" "poplite"       "raw"           "sparklyr"     
# [11] "teamcolors"   

# devtools::revdep_check() - deprecated

# see: https://github.com/r-lib/revdepcheck

#source("https://install-github.me/r-lib/revdepcheck")
revdepcheck::revdep_check()

# or maybe
usethis::use_revdep()
