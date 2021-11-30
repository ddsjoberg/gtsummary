window$add(vbox, show = F)
gSignalConnect(merge, "add_widget", add_widget, vbox)
merge$addUiFromFile("my-menus")
merge$addUiFromFile("my-toolbars")
merge$ensureUpdate() 
window$showAll()

