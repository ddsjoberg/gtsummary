# Explicit
dialog <-
  gtkDialogNewWithButtons("My dialog", main_app_window,
                          c("modal", "destroy-with-parent"), 
                          "gtk-ok", GtkResponseType["accept"], 
                          "gtk-cancel", GtkResponseType["reject"])
## Also via collapsed constructor
dialog <- gtkDialog("My dialog", main_app_window,
                    c("modal", "destroy-with-parent"), 
                    "gtk-ok", GtkResponseType["accept"], 
                    "gtk-cancel", GtkResponseType["reject"])

