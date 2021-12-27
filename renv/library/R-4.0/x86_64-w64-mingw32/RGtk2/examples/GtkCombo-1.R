######
# Creating a combobox with simple text items
######

items <- c("First Item", "Second Item", "Third Item", "Fourth Item",
           "Fifth Item")
combo <- gtkCombo()
combo$setPopdownStrings(items)
