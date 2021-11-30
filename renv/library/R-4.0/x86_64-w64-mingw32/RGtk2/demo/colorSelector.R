window <- NULL
da <- NULL
color <- NULL
frame <- NULL

# Expose callback for the drawing area
# This is where we actually draw the color rectangle
expose.event.callback <- function(widget, event, data)
{
  if (!is.null(widget[["window"]]))
    {      
      style <- widget$getStyle()
      cr <- gdkCairoCreate(widget[["window"]])
      gdkCairoSetSourceColor(cr, style[["bg"]][[GtkStateType["normal"]+1L]])
      gdkCairoRectangle(cr, event[["area"]])
      cr$fill()
    }

  TRUE
}

# show a color selection dialog and save the selected color
change.color.callback <- function(button, data)
{
  dialog <- gtkColorSelectionDialogNew("Changing color", show=F)

  dialog$setTransientFor(window)

  colorsel <- dialog$getColorSelection()

  colorsel$setPreviousColor(color)
  colorsel$setCurrentColor(color)
  colorsel$setHasPalette(TRUE)

  response <- dialog$run()
  if (response == GtkResponseType["ok"])
    {
      color <- colorsel$getCurrentColor()$color
      # save the color in the graphics context
      da$modifyBg("normal", color)
    }

  dialog$destroy()
}

 # initialize color to blue (r,g,b)
color <- c(red=0, green=0, blue=65535)

# make a window
window <- gtkWindowNew("topleve")
window$setTitle("Color Selection")
window$setBorderWidth(8)

# add a vertical layout
vbox <- gtkVBoxNew(FALSE, 8)
vbox$setBorderWidth(8)
window$add(vbox)

#
# Create the color swatch area
#

frame <- gtkFrameNew()
frame$setShadowType("in")
vbox$packStart(frame, TRUE, TRUE, 0)

da <- gtkDrawingAreaNew()

gSignalConnect(da, "expose_event", expose.event.callback)

# set a minimum size
da$setSizeRequest(200, 200)
# set the color
da$modifyBg("normal", color)

frame$add(da)

# make button line up nicely
alignment <- gtkAlignmentNew(1.0, 0.5, 0.0, 0.0)

button <- gtkButtonNewWithMnemonic("_Change the above color")
alignment$add(button)

vbox$packStart(alignment, FALSE, FALSE, 0)

gSignalConnect(button, "clicked", change.color.callback)

gtkWidgetShowAll(window)
