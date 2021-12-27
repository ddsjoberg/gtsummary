window <- gtkDialogNewWithButtons("GtkExpander",
                      NULL,
                      0,
                      "gtk-close",
                      GtkResponseType["none"])
window$setResizable(FALSE)

gSignalConnect(window, "response", gtkWidgetDestroy)
vbox <- gtkVBoxNew(FALSE, 5)
window$getContentArea()$packStart(vbox, TRUE, TRUE, 0)
vbox$setBorderWidth(5)

label <- gtkLabelNew("Expander demo. Click on the triangle for details.")
vbox$packStart(label, FALSE, FALSE, 0)

# Create the expander
expander <- gtkExpanderNew("Details")
vbox$packStart(expander, FALSE, FALSE, 0)

label <- gtkLabelNew("Details can be shown or hidden.")
expander$add(label)

window$showAll()
