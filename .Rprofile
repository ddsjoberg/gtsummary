# this sets the dev folder in the libPath
tryCatch(
  {
    devtools::dev_mode(on = TRUE)
    cli::cli_alert_info("Library paths set to {.path {.libPaths()}}")
  },
  error = function(e) invisible()
)
