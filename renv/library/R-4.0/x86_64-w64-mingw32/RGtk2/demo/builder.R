
quit_activate <- function(action) {
  builder$getObject("window1")$destroy()
}

about_activate <- function(action)
{
  about_dlg <- builder$getObject("aboutdialog1")
  about_dlg$run()
  about_dlg$hide()
}

builder <- gtkBuilder()
filename <- system.file("ui", "demo.ui", package = "RGtk2")
res <- builder$addFromFile(filename)
if (!is.null(res$error))
  stop("ERROR: ", res$error$message)
builder$connectSignals(NULL)
window <- builder$getObject("window1")

window$showAll()
