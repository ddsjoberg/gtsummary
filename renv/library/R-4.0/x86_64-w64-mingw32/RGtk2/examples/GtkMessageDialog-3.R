dialog <- gtkMessageDialog(main_application_window,  "destroy-with-parent",
                           "error", "close")
dialog$setMarkup(message)
