## Creating a simple menu item with an accelerator key.

## Create a GtkAccelGroup and add it to the window.
accel_group = gtkAccelGroup()
window$addAccelGroup(accel_group)

## Create the menu item
save_item = gtkMenuItem("Save")
menu$add(save_item)

## Now add the accelerator to the GtkMenuItem. 
## It will be activated if the user types ctrl-s
## We just need to make sure we use the "visible" flag here to show it.
save_item$addAccelerator("activate", accel_group, GDK_S, 
                         "control-mask", "visible")
