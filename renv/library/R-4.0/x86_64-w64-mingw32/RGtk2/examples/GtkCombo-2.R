######
# Creating a combobox with a complex item
######
  
combo <- gtkCombo()
item <- gtkListItem()

## You can put almost anything into the GtkListItem widget. Here we will use
##   a horizontal box with an arrow and a label in it.
hbox <- gtkHbox(FALSE, 3)
item$add(hbox)
  
arrow <- gtkArrow("right", "out")
hbox$packStart(arrow, FALSE, FALSE, 0)

label <- gtkLabel("First Item")
hbox$packStart(label, FALSE, FALSE, 0)

## You must set the string to display in the entry field when the item is
##   selected.
combo$setItemString(item, "1st Item")

## Now we simply add the item to the combo's list.
combo[["list"]]$add(item)
