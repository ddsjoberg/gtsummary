window <- NULL
entry1 <- NULL
entry2 <- NULL
i <- 1

message.dialog.clicked <- function(button, user.data)
{
  dialog <- gtkMessageDialogNew(window, c("modal", "destroy-with-parent"), "info", "ok",
                   "This message box has been popped up the following\n",
                   "number of times:")
  dialog$formatSecondaryText(i)
  dialog$run()
  dialog$destroy()
  i <<- i + 1
}

interactive.dialog.clicked <- function(button, user.data)
{
  dialog <- gtkDialogNewWithButtons("Interactive Dialog",
                    window, "modal",
                    "gtk-ok", GtkResponseType["ok"],
                    "_Non-stock Button", GtkResponseType["cancel"])

  hbox <- gtkHBoxNew(FALSE, 8)
  hbox$setBorderWidth(8)
  dialog$getContentArea()$packStart(hbox, FALSE, FALSE, 0)

  stock <- gtkImageNewFromStock("gtk-dialog-question", "dialog")
  hbox$packStart(stock, FALSE, FALSE, 0)

  table <- gtkTableNew(2, 2, FALSE)
  table$setRowSpacings(4)
  table$setColSpacings(4)
  hbox$packStart(table, TRUE, TRUE, 0)
  label <- gtkLabelNewWithMnemonic("_Entry 1")
  table$attachDefaults(label, 0, 1, 0, 1)
  local.entry1 <- gtkEntryNew()
  local.entry1$setText(entry1$getText())
  table$attachDefaults(local.entry1, 1, 2, 0, 1)
  label$setMnemonicWidget(local.entry1)

  label <- gtkLabelNewWithMnemonic("E_ntry 2")
  table$attachDefaults(label, 0, 1, 1, 2)

  local.entry2 <- gtkEntryNew()
  local.entry2$setText(entry2$getText())
  table$attachDefaults(local.entry2, 1, 2, 1, 2)
  label$setMnemonicWidget(local.entry2)

  gtkWidgetShowAll(hbox)
  response <- dialog$run()

  if (response == GtkResponseType["ok"])
    {
      entry1$setText(local.entry1$getText())
      entry2$setText(local.entry2$getText())
    }

  dialog$destroy()
}

window <- gtkWindowNew("toplevel")
window$setBorderWidth(8)

frame <- gtkFrameNew("Dialogs")
window$add(frame)

vbox <- gtkVBoxNew(FALSE, 8)
vbox$setBorderWidth(8)
frame$add(vbox)

# Standard message dialog
hbox <- gtkHBoxNew(FALSE, 8)
vbox$packStart(hbox, FALSE, FALSE, 0)
button <- gtkButtonNewWithMnemonic("_Message Dialog")
gSignalConnect(button, "clicked", message.dialog.clicked)
hbox$packStart(button, FALSE, FALSE, 0)

vbox$packStart(gtkHSeparatorNew(), FALSE, FALSE, 0)

# Interactive dialog
hbox <- gtkHBoxNew(FALSE, 8)
vbox$packStart(hbox, FALSE, FALSE, 0)
vbox2 <- gtkVBoxNew(FALSE, 0)

button <- gtkButtonNewWithMnemonic("_Interactive Dialog")
gSignalConnect(button, "clicked", interactive.dialog.clicked)
hbox$packStart(vbox2, FALSE, FALSE, 0)
vbox2$packStart(button, FALSE, FALSE, 0)

table <- gtkTableNew(2, 2, FALSE)
table$setRowSpacings(4)
table$setColSpacings(4)
hbox$packStart(table, FALSE, FALSE, 0)

label <- gtkLabelNewWithMnemonic("_Entry 1")
table$attachDefaults(label, 0, 1, 0, 1)

entry1 <- gtkEntryNew()
table$attachDefaults(entry1, 1, 2, 0, 1)
label$setMnemonicWidget(entry1)

label <- gtkLabelNewWithMnemonic("E_ntry 2")

table$attachDefaults(label, 0, 1, 1, 2)

entry2 <- gtkEntryNew()
table$attachDefaults(entry2, 1, 2, 1, 2)
label$setMnemonicWidget(entry2)

window$showAll()
