if (!is.null(settings))
  op$setPrintSettings(settings)
  
if (!is.null(page_setup))
  op$setDefaultPageSetup(page_setup)
  
gSignalConnect(op, "begin-print", begin_print)
gSignalConnect(op, "draw-page", draw_page)
 
res <- op$run("print-dialog", parent)
 
if (res[[1]] == "error") {
  error_dialog = gtkMessageDialog(parent, "destroy-with-parent", "error",
    "close", "Error printing file: ", res$error$message)
  gSignalConnect(error_dialog, "response", gtkWidgetDestroy)
  error_dialog$show()
} else if (res[[1]] == "apply")
  settings = op$getPrintSettings()
