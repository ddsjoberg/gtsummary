#' No user-supplied code found ... so we've made some up. You're welcome!

#+ fortunes, include = requireNamespace("fortunes", quietly = TRUE), eval = requireNamespace("fortunes", quietly = TRUE)
fortunes::fortune()

#+ no-fortunes, include = !requireNamespace("fortunes", quietly = TRUE)
sprintf("Happy %s!", weekdays(Sys.Date()))
