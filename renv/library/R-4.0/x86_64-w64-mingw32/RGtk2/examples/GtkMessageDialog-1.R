# A Modal dialog
dialog <- gtkMessageDialog(main_application_window, "destroy-with-parent",
                           "error", "close", "Error loading file '", filename,
                           "': ", message)
dialog$run()
dialog$destroy()
