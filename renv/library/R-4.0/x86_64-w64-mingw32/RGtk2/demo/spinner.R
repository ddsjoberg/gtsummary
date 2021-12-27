on_play_clicked <- function(button, user_data)
{
  spinner_sensitive$start()
  spinner_unsensitive$start()
}

on_stop_clicked <- function(button, user_data)
{
  spinner_sensitive$stop()
  spinner_unsensitive$stop()
}

window <- gtkDialog("GtkSpinner", NULL, 0, "gtk-close",
                    GtkResponseType["none"])

window$setResizable(FALSE)

gSignalConnect(window, "response", gtkWidgetDestroy)
gSignalConnect(window, "destroy", gtkWidgetDestroy)

content_area <- window$getContentArea()

vbox <- gtkVBox(FALSE, 5)
content_area$packStart(vbox)
vbox$setBorderWidth(5)

## Sensitive
hbox <- gtkHBox(FALSE, 5)
spinner <- gtkSpinner()
hbox$add(spinner)
hbox$add(gtkEntry())
vbox$add(hbox)
spinner_sensitive <- spinner

## Disabled
hbox <- gtkHBox(FALSE, 5)
spinner <- gtkSpinner()
hbox$add(spinner)
hbox$add(gtkEntry())
vbox$add(hbox)
spinner_unsensitive <- spinner
hbox$setSensitive(FALSE)

button <- gtkButton(stock = GTK_STOCK_MEDIA_PLAY)
gSignalConnect(button, "clicked", on_play_clicked, spinner)
vbox$add(button)

button <- gtkButton(stock = GTK_STOCK_MEDIA_STOP)
gSignalConnect(button, "clicked", on_stop_clicked, spinner)
vbox$add(button)

on_play_clicked(NULL, NULL)
