window <- NULL

# Creates a tree model containing the completions
create.completion.model <- function()
{
  store <- gtkListStoreNew("gchararray")

  # Append one word
  store$set(store$append()$iter, 0, "GNOME")

  # Append another word
  store$set(store$append()$iter, 0, "Duncan")

  # And another word
  store$set(store$append()$iter, 0, "Dunkirk")

  return(store)
}

window <- gtkDialogNewWithButtons("GtkEntryCompletion",
                      NULL,
                      0,
                      "gtk-close",
                      GtkResponseType["none"], show = F)
window$setResizable(FALSE)
gSignalConnect(window, "response", gtkWidgetDestroy)
vbox <- gtkVBoxNew(FALSE, 5)
window$getContentArea()$packStart(vbox, TRUE, TRUE, 0)
vbox$setBorderWidth(5)

label <- gtkLabelNew()
label$setMarkup("Completion demo, try writing <b>duncan</b> or <b>gnome</b> for example.")
vbox$packStart(label, FALSE, FALSE, 0)

# create entry
entry <- gtkEntryNew()
vbox$packStart(entry, FALSE, FALSE, 0)

# Create the completion object
completion <- gtkEntryCompletionNew()

# Assign the completion to the entry
entry$setCompletion(completion)

# Create a tree model and use it as the completion model
completion.model <- create.completion.model()
completion$setModel(completion.model)

# Use model column 0 as the text column
completion$setTextColumn(0)

window$showAll()
