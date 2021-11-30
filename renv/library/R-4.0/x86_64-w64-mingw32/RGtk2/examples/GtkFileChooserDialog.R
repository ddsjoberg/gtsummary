######
# Request a file from the user and open it
######

# This is how one creates a dialog with buttons and associated response codes.
# (Please ignore the C "Response Code" example in the next section)
dialog <- gtkFileChooserDialog("Open File", parent_window, "open",
                               "gtk-cancel", GtkResponseType["cancel"], 
                               "gtk-open", GtkResponseType["accept"])

if (dialog$run() == GtkResponseType["accept"]) {
  filename <- dialog$getFilename()
  f <- file(filename)
}

dialog$destroy()
