do_page_setup <- function()
{
  if (is.null(settings))
    settings <- gtkPrintSettings()

  new_page_setup <- gtkPrintRunPageSetupDialog(main_window, page_setup,
                                               settings)

  page_setup <- new_page_setup
}

