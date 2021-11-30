# Function to open a dialog box displaying the message provided.
quick_message <- function(message) {
  ## Create the widgets 
   
  dialog <- gtkDialog("Message", NULL, "destroy-with-parent",
                      "gtk-ok", GtkResponseType["none"],
                      show = FALSE)
  label <- gtkLabel(message)
   
  ## Ensure that the dialog box is destroyed when the user responds.
   
  gSignalConnect(dialog, "response", gtkWidgetDestroy)

  ## Add the label, and show everything we've added to the dialog.

  dialog[["vbox"]]$add(label)
  dialog$showAll()
}
