## Let's add some tooltips to some buttons
button_bar_tips <- gtkTooltips()

## Create the buttons and pack them into a GtkHBox
hbox <- gtkHBox(TRUE, 2)
   
load_button <- gtkButton("Load a file")
hbox$packStart(load_button, TRUE, TRUE, 2)
   
save_button <- gtkButton("Save a file")
hbox$packStart(save_button, TRUE, TRUE, 2)
   
## Add the tips
button_bar_tips$setTip(load_button,
                       "Load a new document into this window",
                       paste("Requests the filename of a document.",
                             "This will then be loaded into the current",
                             "window, replacing the contents of whatever",
                             "is already loaded."))
button_bar_tips$setTip(save_button,
                       "Saves the current document to a file",
                       paste("If you have saved the document previously,",
                             "then the new version will be saved over the",
                             "old one. Otherwise, you will be prompted for",
                             "a filename.")) 
