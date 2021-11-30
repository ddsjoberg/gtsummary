group <- NULL
for (i in 1:5) {
  item <- gtkRadioMenuItem(group, "This is an example")
  group <- item$getGroup()
  if (i == 1)
    item$setActive(TRUE)
}

