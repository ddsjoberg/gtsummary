dialog <- gtkMessageDialog(main_application_window, "destroy-with-parent",
                           "error", "close", "Error loading file '", filename,
                           "': ", message)
# Destroy the dialog when the user responds to it (e.g. clicks a button)
gSignalConnect(dialog, "response", gtkWidgetDestroy)
