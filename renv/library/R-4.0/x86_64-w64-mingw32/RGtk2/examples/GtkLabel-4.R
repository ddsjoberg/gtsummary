## Pressing Alt+H will focus the entry
entry <- gtkEntry()
label <- gtkLabelNewWithMnemonic("_Hello")
label$setMnemonicWidget(entry)
