settings <- NULL
print_something <-
{
  op <- gtkPrintOperation()

  if (!is.null(settings)) 
    op$setPrintSettings(settings)

  gSignalConnect(op, "begin_print", begin_print)
  gSignalConnect(op, "draw_page", draw_page)

  res <- op$run("print-dialog", main_window)[[1]]

  if (res == "apply")
    settings <- op$getPrintSettings()
}
