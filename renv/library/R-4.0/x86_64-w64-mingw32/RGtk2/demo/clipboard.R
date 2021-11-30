window <- NULL

copy.button.clicked <- function(button, user.data)
{
  checkPtrType(user.data, "GtkWidget")
  entry <- user.data

 clipboard <- entry$getClipboard(GDK_SELECTION_CLIPBOARD)
  
 clipboard$setText(entry$getText()) # copy all the text to the clipboard
}

paste.received <- function(clipboard, text, user.data)
{
  checkPtrType(user.data, "GtkWidget")
  entry <- user.data

  entry$setText(text)
}

paste.button.clicked <- function(button, user.data)
{
  checkPtrType(user.data, "GtkWidget")
  entry <- user.data

  clipboard <- entry$getClipboard(GDK_SELECTION_CLIPBOARD)

  # Request the contents of the clipboard, paste.received will be
  #   called when we do get the contents.

  clipboard$requestText(paste.received, entry)
}


window <- gtkWindowNew("toplevel", show=F)
vbox <- gtkVBoxNew(FALSE, 0)
vbox$setBorderWidth(8)

window$add(vbox)

label <- gtkLabelNew("\"Copy\" will copy the text\nin the entry to the clipboard")

vbox$packStart(label, FALSE, FALSE, 0)

hbox <- gtkHBoxNew(FALSE, 4)
hbox$setBorderWidth(8)
vbox$packStart(hbox, FALSE, FALSE, 0)

# Create the first entry
entry <- gtkEntryNew()
hbox$packStart(entry, TRUE, TRUE, 0)

# Create the button
button <- gtkButtonNewFromStock("gtk-copy")
hbox$packStart(button, FALSE, FALSE, 0)

gSignalConnect(button, "clicked", copy.button.clicked, entry)
label <- gtkLabelNew("\"Paste\" will paste the text from the clipboard to the entry")
vbox$packStart(label, FALSE, FALSE, 0)

hbox <- gtkHBoxNew(FALSE, 4)
hbox$setBorderWidth(8)
vbox$packStart(hbox, FALSE, FALSE, 0)

# Create the second entry
entry <- gtkEntryNew()
hbox$packStart(entry, TRUE, TRUE, 0)
# Create the button
button <- gtkButtonNewFromStock("gtk-paste")
hbox$packStart(button, FALSE, FALSE, 0)
gSignalConnect(button, "clicked", paste.button.clicked, entry)

window$showAll()
