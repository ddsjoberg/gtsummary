# Create a button to let the user select a file in /etc
button <- gtkFileChooserButton("Select a file", "open")
button$setCurrentFolder("/etc")
