cancel_button <- dialog$addButton("gtk-cancel", "cancel")
 
ok_button <- dialog$addButton("gtk-ok", "ok")
ok_button$grabDefault()
  
help_button <- dialog$addButton("gtk-help", "help")

dialog$setAlternativeButtonOrder("ok", "cancel", "help")
