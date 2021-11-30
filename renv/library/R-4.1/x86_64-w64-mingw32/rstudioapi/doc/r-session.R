## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(eval = FALSE)

## -----------------------------------------------------------------------------
#  # restart R, then run some code after
#  rstudioapi::restartSession(command = "print('Welcome back!')")
#  
#  # send some code to the console and execute it immediately
#  rstudioapi::sendToConsole("1 + 1", execute = TRUE)

## -----------------------------------------------------------------------------
#  setHook("rstudio.sessionInit", function(newSession) {
#    if (newSession)
#      message("Welcome to RStudio ", rstudioapi::getVersion())
#  }, action = "append")

