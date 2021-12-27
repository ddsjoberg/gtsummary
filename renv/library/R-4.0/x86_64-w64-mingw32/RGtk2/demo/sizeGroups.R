# source("/home/larman/research/RGtk/inst/demo/sizeGroups.R")

window <- NULL

# Convenience function to create a combo box holding a number of strings
#

create.combo.box <- function(strings)
{
  combo.box <- gtkComboBoxNewText()
  
  for (str in strings)
    combo.box$appendText(str)

  combo.box$setActive(FALSE)

  return(combo.box)
}

add.row <- function(table, row, size.group, label.text, options)
{
  label <- gtkLabelNewWithMnemonic(label.text)
  label$setAlignment(0, 1)
  table$attach(label, 0, 1, row, row + 1, c("expand", "fill"), 0, 0, 0)
  
  combo.box <- create.combo.box(options)
  label$setMnemonicWidget(combo.box)
  size.group$addWidget(combo.box)
  table$attach(combo.box, 1, 2, row, row + 1, 0, 0, 0, 0)
}

toggle.grouping <- function(check.button, size.group)
{

  # GtkSizeGroup["none"] is not generally useful, but is useful
  # here to show the effect of GtkSizeGroup["horizontal"] by
  # contrast
  #
  if (check.button$getActive())
    new.mode <- GtkSizeGroupMode["horizontal"]
  else
    new.mode <- GtkSizeGroupMode["none"]
  
  size.group$setMode(new.mode)
}

color.options <- c("Red", "Green", "Blue")
  
dash.options <- c("Solid", "Dashed", "Dotted")
  
end.options <- c("Square", "Round", "Arrow")
  
window <- gtkDialogNewWithButtons("GtkSizeGroup", NULL, 0, "gtk-close", GtkResponseType["none"], show=F)
window$setResizable(FALSE)
gSignalConnect(window, "response", gtkWidgetDestroy)

vbox <- gtkVBoxNew(FALSE, 5)
window$getContentArea()$packStart(vbox, TRUE, TRUE, 0)
vbox$setBorderWidth(5)

size.group <- gtkSizeGroupNew(GtkSizeGroupMode["horizontal"])
      
# Create one frame holding color options
#

frame <- gtkFrameNew("Color Options")
vbox$packStart(frame, TRUE, TRUE, 0)

table <- gtkTableNew(2, 2, FALSE)
table$setBorderWidth(5)
table$setRowSpacings(5)
table$setColSpacings(10)
frame$add(table)

add.row(table, 0, size.group, "_Foreground", color.options)
add.row(table, 1, size.group, "_Background", color.options)

# And another frame holding line style options
#
frame <- gtkFrameNew("Line Options")
vbox$packStart(frame, FALSE, FALSE, 0)

table <- gtkTableNew(2, 2, FALSE)
table$setBorderWidth(5)
table$setRowSpacings(5)
table$setColSpacings(10)
frame$add(table)

add.row(table, 0, size.group, "_Dashing", dash.options)
add.row(table, 1, size.group, "_Line ends", end.options)

# And a check button to turn grouping on and off 
check.button <- gtkCheckButtonNewWithMnemonic("_Enable grouping")
vbox$packStart(check.button, FALSE, FALSE, 0)
      
check.button$setActive(TRUE)
gSignalConnect(check.button, "toggled", toggle.grouping, size.group)
window$showAll()
