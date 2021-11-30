## connect our handler which will popup the menu 
gSignalConnect(window, "button_press_event", my_popup_handler, menu,
               user.data.first=TRUE)
