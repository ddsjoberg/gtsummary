update_preview_cb <- function(file_chooser, preview)
{
  filename <- file_chooser$getPreviewFilename()

  pixbuf <- gdkPixbuf(file=filename, w=128, h=128)[[1]]
  have_preview <- !is.null(pixbuf)

  preview$setFromPixbuf(pixbuf)

  file_chooser$setPreviewWidgetActive(have_preview)
}

preview <- gtkImage()
my_file_chooser$setPreviewWidget(preview)
gSignalConnect(my_file_chooser, "update-preview", update_preview_cb, preview)
