# this sets the dev folder in the libPath
tryCatch(
  {
    devtools::dev_mode(on = TRUE)
    cli::cli_alert_info("Library paths set to {.path {.libPaths()}}")
    cli::cli_alert_info("Run {.code devtools::dev_mode()} to turn off dev mode.")
  },
  error = function(e) {
    message("Use `devtools::dev_mode()` to initiate 'Dev Mode'.")
  }
)
